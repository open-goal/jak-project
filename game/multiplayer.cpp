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

#include "game/runtime.h"

size_t curl_write_callbacka(char* ptr, size_t size, size_t nmemb, void* userdata) {
  size_t len = size * nmemb;
  std::string* response_data = reinterpret_cast<std::string*>(userdata);
  response_data->append(ptr, len);
  return len;
}

MultiplayerInfo* gMultiplayerInfo;

void http_register(u64 mpInfo) {
  gMultiplayerInfo = Ptr<MultiplayerInfo>(mpInfo).c();
  // spawn new thread to handle parsing curl response
  std::thread([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, "http://78.108.218.126:25560/register");
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
    }
  }).detach();
}

void http_update_position() {
  // spawn new thread to handle parsing curl response
  std::thread([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    RemotePlayerInfo* rpInfo = &(gMultiplayerInfo->players[gMultiplayerInfo->player_num]);

    // Construct JSON payload
    nlohmann::json payload = {
      {"trans_x", rpInfo->trans_x},
      {"trans_y", rpInfo->trans_y},
      {"trans_z", rpInfo->trans_z},
      {"quat_x", rpInfo->quat_x},
      {"quat_y", rpInfo->quat_y},
      {"quat_z", rpInfo->quat_z},
      {"quat_w", rpInfo->quat_w}
    };
    std::string payload_str = payload.dump();
    std::string url = "http://78.108.218.126:25560/update?player_num=" + std::to_string(gMultiplayerInfo->player_num);

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
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
      // no action needed after posting position
    }
  }).detach();
}

void http_get_positions() {
  // spawn new thread to handle parsing curl response
  std::thread([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();
    std::string url =
        "http://78.108.218.126:25560/get?player_num=" + std::to_string(gMultiplayerInfo->player_num);

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
      for (const auto& item : response_json.items()) {
        int pNum = stoi(item.key());
        if (pNum < 12) {
          RemotePlayerInfo* rpInfo = &(gMultiplayerInfo->players[pNum]);

          for (const auto& field : item.value().items()) {
            if (field.key().compare("trans_x") == 0) {
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
            }
          }
        }
      }
    }
  }).detach();
}
