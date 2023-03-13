#include "multiplayer.h"

#include <cstring>
#include <iomanip>
#include <map>
#include <sstream>
#include <string>
#include "curl/curl.h"
#include "common/util/json_util.h"
#include "common/util/unicode_util.h"
#include <iostream>


#include "game/runtime.h"

MultiplayerInfo& gMultiplayerInfo;

size_t curl_write_callbacka(char* ptr, size_t size, size_t nmemb, void* userdata) {
  size_t len = size * nmemb;
  std::string* response_data = reinterpret_cast<std::string*>(userdata);
  response_data->append(ptr, len);
  return len;
}

void pc_http_register(MultiplayerInfo mpInfo) {
  gMultiplayerInfo = mpInfo;
  // spawn new thread to handle parsing curl response
  std::thread t2([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:8080/register");
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
      gMultiplayerInfo.player_num = player_num;
    }
  });
  t2.detach();
}

void pc_http_update_position() {
  // spawn new thread to handle parsing curl response
  std::thread t2([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    RemotePlayerInfo& rpInfo = gMultiplayerInfo.players[gMultiplayerInfo.player_num];

    // Construct JSON payload
    nlohmann::json payload = {
      "trans_x": rpInfo.trans_x,
      "trans_y": rpInfo.trans_y,
      "trans_z": rpInfo.trans_z,
      "quat_x": rpInfo.quat_x,
      "quat_y": rpInfo.quat_y,
      "quat_z": rpInfo.quat_z,
      "quat_w": rpInfo.quat_w
    };
    std::string payload_str = payload.dump();

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:8080/update?player_num=" + gMultiplayerInfo.player_num);
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
      // Parse JSON response
      nlohmann::json response_json = nlohmann::json::parse(response_data);

      // Extract values from JSON response
      int player_num = response_json["player_num"];
      gMultiplayerInfo.player_num = player_num;
    }
  });
  t2.detach();
}

void pc_http_get_positions() {
  // spawn new thread to handle parsing curl response
  std::thread t2([]() {
    // Initialize curl
    curl_global_cleanup();
    curl_global_init(CURL_GLOBAL_ALL);
    CURL* curl = curl_easy_init();

    // Set curl options
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost:8080/get?player_num=" + gMultiplayerInfo.player_num);
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
        if (pNum < 4) {
          RemotePlayerInfo& rpInfo = gMultiplayerInfo.players[pNum];
          rpInfo.trans_x = item.value()["trans_x"];
          rpInfo.trans_y = item.value()["trans_y"];
          rpInfo.trans_z = item.value()["trans_z"];
          rpInfo.quat_x = item.value()["quat_x"];
          rpInfo.quat_y = item.value()["quat_y"];
          rpInfo.quat_z = item.value()["quat_z"];
          rpInfo.quat_w = item.value()["quat_w"];
        }
      }
    }
  });
  t2.detach();
}
