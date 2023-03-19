#include "multiplayer.h"

#include <cstring>
#include <iomanip>
#include <map>
#include <sstream>
#include <string>
#include "game/kernel/common/Ptr.h"
#include "curl/curl.h"
#include "common/util/json_util.h"
#include "common/util/unicode_util.h"
#include <iostream>

#include "game/kernel/jak1/kscheme.h"
#include "game/runtime.h"

std::string ipAddressOrHostname = "localhost:8080";
// std::string ipAddressOrHostname = "78.108.218.126:25560";
std::stringstream urlStream;

size_t curl_write_callbacka(char* ptr, size_t size, size_t nmemb, void* userdata) {
  size_t len = size * nmemb;
  std::string* response_data = reinterpret_cast<std::string*>(userdata);
  response_data->append(ptr, len);
  return len;
}

MultiplayerInfo* gMultiplayerInfo;
RemotePlayerInfo* gSelfPlayerInfo;
String* uname;

void http_register(u64 mpInfo, u64 selfPlayerInfo) {
  gMultiplayerInfo = Ptr<MultiplayerInfo>(mpInfo).c();
  gSelfPlayerInfo = Ptr<RemotePlayerInfo>(selfPlayerInfo).c();
  uname = Ptr<String>(gSelfPlayerInfo->username).c();

  // spawn new thread to handle parsing curl response
  std::thread([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    // Set curl options
    std::string username = Ptr<String>(gSelfPlayerInfo->username).c()->data();
    std::string url = "http://" + ipAddressOrHostname + "/register?username=" + username;
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "foobar");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write_callbacka);
    std::string response_data;
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data);

    // Perform curl request
    CURLcode res = curl_easy_perform(curl);

    // Cleanup curl
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    // Check if the request was successful
    if (res == CURLE_OK) {
      // Parse JSON response
      nlohmann::json response_json = nlohmann::json::parse(response_data);

      // Extract values from JSON response
      int player_num = response_json["player_num"];
      gMultiplayerInfo->player_num = player_num;
      int game_state = response_json["game_state"];
      gMultiplayerInfo->state = game_state;
    }
  }).detach();
}

void http_post_generic(const std::string& endpoint, const nlohmann::json& payload) {
  // spawn new thread to handle parsing curl response
  std::thread([endpoint, payload]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    std::string payload_str = payload.dump();

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, endpoint.c_str());
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload_str.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write_callbacka);
    std::string response_data;
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data);

    // Perform curl request
    CURLcode res = curl_easy_perform(curl);

    // Cleanup curl
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    // Check if the request was successful
    if (res == CURLE_OK) {
      // assume no action needed after update
    }
  }).detach();
}

void http_mark_found(int idx) {
  RemotePlayerInfo* ownRpInfo = &(gMultiplayerInfo->players[gMultiplayerInfo->player_num]);
  RemotePlayerInfo* foundRpInfo = &(gMultiplayerInfo->players[idx]);

  std::string url = "http://" + ipAddressOrHostname + "/mark_found";

  // Construct JSON payload
  nlohmann::json payload = {{"seeker_username", Ptr<String>(ownRpInfo->username).c()->data()},
                            {"found_username", Ptr<String>(foundRpInfo->username).c()->data()}};

  http_post_generic(url, payload);
}

void http_update() {
  RemotePlayerInfo* rpInfo = &(gMultiplayerInfo->players[gMultiplayerInfo->player_num]);

  std::string username = Ptr<String>(rpInfo->username).c()->data();
  std::string url = "http://" + ipAddressOrHostname + "/update?username=" + username;

  // Construct JSON payload
  nlohmann::json payload = {
      {"username", username},
      {"color", rpInfo->color},
      {"trans_x", rpInfo->trans_x},
      {"trans_y", rpInfo->trans_y},
      {"trans_z", rpInfo->trans_z},
      {"quat_x", rpInfo->quat_x},
      {"quat_y", rpInfo->quat_y},
      {"quat_z", rpInfo->quat_z},
      {"quat_w", rpInfo->quat_w},
      {"tgt_state", rpInfo->tgt_state},
      // role intentionally left out, only updated from server side
      {"mp_state", rpInfo->mp_state}
  };

  http_post_generic(url, payload);
}

void http_get() {
  // spawn new thread to handle parsing curl response
  std::thread([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    std::string url = "http://" + ipAddressOrHostname + "/get";

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write_callbacka);
    std::string response_data;
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_data);

    // Perform curl request
    CURLcode res = curl_easy_perform(curl);

    // Cleanup curl
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    // Check if the request was successful
    if (res == CURLE_OK) {
      // Parse JSON response
      nlohmann::json response_json = nlohmann::json::parse(response_data);

      // game state
      int game_state = response_json["game_state"];
      gMultiplayerInfo->state = game_state;

      // players
      for (const auto& item : response_json["players"].items()) {
        int pNum = stoi(item.key());
        if (pNum < MAX_MULTIPLAYER_COUNT) {
          RemotePlayerInfo* rpInfo = &(gMultiplayerInfo->players[pNum]);

          for (const auto& field : item.value().items()) {
            if (field.key().compare("username") == 0) {
              // str copy username into struct
              std::string uname = field.value();
              strncpy(Ptr<String>(rpInfo->username).c()->data(), uname.c_str(), MAX_USERNAME_LEN);
            } else if (field.key().compare("color") == 0) {
              rpInfo->color = field.value();
            } else if (field.key().compare("trans_x") == 0) {
              rpInfo->trans_x = field.value();
            } else if (field.key().compare("trans_y") == 0) {
              rpInfo->trans_y = field.value();
            } else if (field.key().compare("trans_z") == 0) {
              rpInfo->trans_z = field.value();
            } else if (field.key().compare("quat_x") == 0) {
              rpInfo->quat_x = field.value();
            } else if (field.key().compare("quat_y") == 0) {
              rpInfo->quat_y = field.value();
            } else if (field.key().compare("quat_z") == 0) {
              rpInfo->quat_z = field.value();
            } else if (field.key().compare("quat_w") == 0) {
              rpInfo->quat_w = field.value();
            } else if (field.key().compare("tgt_state") == 0) {
              rpInfo->tgt_state = field.value();
            } else if (field.key().compare("role") == 0) {
              rpInfo->role = field.value();
            } else if (field.key().compare("mp_state") == 0
              && pNum != gMultiplayerInfo->player_num) { // only sync mp_state for remotes. for our own target, only goal code should be updating this
              rpInfo->mp_state = field.value();
            }
          }
        }
      }
    }
  }).detach();
}
