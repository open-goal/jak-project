#include "kmachine_extras.h"

#include <bitset>
#include <regex>

#include "kscheme.h"

#include "common/symbols.h"
#include "common/util/FontUtils.h"

#include "game/external/discord.h"
#include "game/external/discord_jak1.h"
#include "game/external/discord_jak2.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kscheme.h"
#include "game/overlord/jak2/iso.h"

namespace kmachine_extras {
using namespace jak2;

AutoSplitterBlock g_auto_splitter_block_jak2;

void update_discord_rpc(u32 discord_info) {
  if (gDiscordRpcEnabled) {
    DiscordRichPresence rpc;
    char state[128];
    char large_image_key[128];
    char large_image_text[128];
    char small_image_key[128];
    char small_image_text[128];
    auto info = discord_info ? Ptr<DiscordInfo>(discord_info).c() : NULL;
    if (info) {
      // Get the data from GOAL
      int orbs = (int)info->orb_count;
      int gems = (int)info->gem_count;
      // convert encodings
      std::string status = get_font_bank(GameTextVersion::JAK2)
                               ->convert_game_to_utf8(Ptr<String>(info->status).c()->data());

      // get rid of special encodings like <COLOR_WHITE>
      std::regex r("<.*?>");
      while (std::regex_search(status, r)) {
        status = std::regex_replace(status, r, "");
      }

      char* level = Ptr<String>(info->level).c()->data();
      auto cutscene = Ptr<Symbol4<u32>>(info->cutscene)->value();
      float time = info->time_of_day;
      float percent_completed = info->percent_completed;
      std::bitset<32> focus_status;
      focus_status = info->focus_status;
      char* task = Ptr<String>(info->task).c()->data();

      // Construct the DiscordRPC Object
      const char* full_level_name =
          get_full_level_name(level_names, level_name_remap, Ptr<String>(info->level).c()->data());
      memset(&rpc, 0, sizeof(rpc));
      // if we have an active task, set the mission specific image for it
      // also small hack to prevent oracle image from showing up while inside levels
      // like hideout, onintent, etc.
      if (strcmp(task, "unknown") != 0 && strcmp(task, "city-oracle") != 0) {
        strcpy(large_image_key, task);
      } else {
        // if we are in an outdoors level, use the picture for the corresponding time of day
        if (!indoors(indoor_levels, level)) {
          char level_with_tod[128];
          strcpy(level_with_tod, level);
          strcat(level_with_tod, "-");
          strcat(level_with_tod, time_of_day_str(time));
          strcpy(large_image_key, level_with_tod);
        } else {
          strcpy(large_image_key, level);
        }
      }
      strcpy(large_image_text, full_level_name);
      if (!strcmp(full_level_name, "unknown")) {
        strcpy(large_image_key, full_level_name);
        strcpy(large_image_text, level);
      }
      rpc.largeImageKey = large_image_key;
      if (cutscene != offset_of_s7()) {
        strcpy(state, "Watching a cutscene");
        // temporarily move these counters to the large image tooltip during a cutscene
        strcat(large_image_text,
               fmt::format(" | {:.0f}% | Orbs: {} | Gems: {} | {}", percent_completed,
                           std::to_string(orbs), std::to_string(gems), get_time_of_day(time))
                   .c_str());
      } else {
        strcpy(state, fmt::format("{:.0f}% | Orbs: {} | Gems: {} | {}", percent_completed,
                                  std::to_string(orbs), std::to_string(gems), get_time_of_day(time))
                          .c_str());
      }
      rpc.largeImageText = large_image_text;
      rpc.state = state;
      // check for any special conditions to display for the small image
      if (FOCUS_TEST(focus_status, FocusStatus::Board)) {
        strcpy(small_image_key, "focus-status-board");
        strcpy(small_image_text, "On the JET-Board");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Mech)) {
        strcpy(small_image_key, "focus-status-mech");
        strcpy(small_image_text, "In the Titan Suit");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Pilot)) {
        strcpy(small_image_key, "focus-status-pilot");
        strcpy(small_image_text, "Driving a Zoomer");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Indax)) {
        strcpy(small_image_key, "focus-status-indax");
        strcpy(small_image_text, "Playing as Daxter");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Dark)) {
        strcpy(small_image_key, "focus-status-dark");
        strcpy(small_image_text, "Dark Jak");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Disable) &&
                 FOCUS_TEST(focus_status, FocusStatus::Grabbed)) {
        // being in a turret sets disable and grabbed flags
        strcpy(small_image_key, "focus-status-turret");
        strcpy(small_image_text, "In a Gunpod");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Gun)) {
        strcpy(small_image_key, "focus-status-gun");
        strcpy(small_image_text, "Using a Gun");
      } else {
        strcpy(small_image_key, "");
        strcpy(small_image_text, "");
      }
      rpc.smallImageKey = small_image_key;
      rpc.smallImageText = small_image_text;
      rpc.startTimestamp = gStartTime;
      rpc.details = status.c_str();
      rpc.partySize = 0;
      rpc.partyMax = 0;
      Discord_UpdatePresence(&rpc);
    }
  } else {
    Discord_ClearPresence();
  }
}

void pc_set_levels(u32 lev_list) {
  if (!Gfx::GetCurrentRenderer()) {
    return;
  }
  std::vector<std::string> levels;
  for (int i = 0; i < LEVEL_MAX; i++) {
    u32 lev = *Ptr<u32>(lev_list + i * 4);
    std::string ls = Ptr<String>(lev).c()->data();
    if (ls != "none" && ls != "#f" && ls != "") {
      levels.push_back(ls);
    }
  }

  Gfx::GetCurrentRenderer()->set_levels(levels);
}

void pc_set_active_levels(u32 lev_list) {
  if (!Gfx::GetCurrentRenderer()) {
    return;
  }
  std::vector<std::string> levels;
  for (int i = 0; i < LEVEL_MAX; i++) {
    u32 lev = *Ptr<u32>(lev_list + i * 4);
    std::string ls = Ptr<String>(lev).c()->data();
    if (ls != "none" && ls != "#f" && ls != "") {
      levels.push_back(ls);
    }
  }

  Gfx::GetCurrentRenderer()->set_active_levels(levels);
}

u32 alloc_vagdir_names(u32 heap_sym) {
  auto alloced_heap = (Ptr<u64>)alloc_heap_memory(heap_sym, gVagDir.count * 8 + 8);
  if (alloced_heap.offset) {
    *alloced_heap = gVagDir.count;
    // use entry -1 to get the amount
    alloced_heap = alloced_heap + 8;
    for (size_t i = 0; i < gVagDir.count; ++i) {
      char vagname_temp[9];
      memcpy(vagname_temp, gVagDir.vag[i].name, 8);
      for (int j = 0; j < 8; ++j) {
        vagname_temp[j] = tolower(vagname_temp[j]);
      }
      vagname_temp[8] = 0;
      u64 vagname_val;
      memcpy(&vagname_val, vagname_temp, 8);
      *(alloced_heap + i * 8) = vagname_val;
    }
    return alloced_heap.offset;
  }
  return s7.offset;
}

inline u64 bool_to_symbol(const bool val) {
  return val ? static_cast<u64>(s7.offset) + true_symbol_offset(g_game_version) : s7.offset;
}

inline bool symbol_to_bool(const u32 symptr) {
  return symptr != s7.offset;
}

void init_autosplit_struct() {
  g_auto_splitter_block_jak2.pointer_to_symbol =
      (u64)g_ee_main_mem + (u64)intern_from_c("*autosplit-info-jak2*")->value();
}

// TODO - currently using a single mutex for all background task synchronization
std::mutex background_task_lock;

std::string last_rpc_error = "";

// TODO - add a TTL to this
std::unordered_map<std::string, std::vector<std::pair<std::string, float>>>
    external_speedrun_time_cache = {};
std::unordered_map<std::string, std::vector<std::pair<std::string, float>>>
    external_race_time_cache = {};
std::unordered_map<std::string, std::vector<std::pair<std::string, float>>>
    external_highscores_cache = {};

// clang-format off
// TODO - eventually don't depend on SRC
const std::unordered_map<std::string, std::string> external_speedrun_lookup_urls = {
    {"any", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/n2y6y4ed?embed=players&max=200"},
    {"anyhoverless", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/7kjyn5gk?embed=players&max=200"},
    {"allmissions", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/xk96myxk?embed=players&max=200"},
    {"100", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/z27exp5k?embed=players&max=200"},
    {"anyorbs", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/zdn3vm72?embed=players&max=200"},
    {"anyhero", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/category/q25pv0wd?embed=players&max=200"}};
const std::unordered_map<std::string, std::string> external_race_lookup_urls = {
    {"class3", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/y9m7qmx9/jdr0mg0d?embed=players&max=200"},
    {"class2", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/5wk5zmpw/jdr0mg0d?embed=players&max=200"},
    {"class1", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/5922g639/jdr0mg0d?embed=players&max=200"},
    {"class3rev", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/29v4e8l9/jdr0mg0d?embed=players&max=200"},
    {"class2rev", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/xd4475rd/jdr0mg0d?embed=players&max=200"},
    {"class1rev", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/xd0mre4w/jdr0mg0d?embed=players&max=200"},
    {"erol", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/rw68p7gd/jdr0mg0d?embed=players&max=200"},
    {"port", "https://www.speedrun.com/api/v1/leaderboards/3dxk47y1/level/n93v5xzd/jdr0mg0d?embed=players&max=200"}};
const std::unordered_map<std::string, std::string> external_highscores_lookup_urls = {
    {"scatter", "https://api.jakspeedruns.workers.dev/v1/highscores/2"},
    {"blaster", "https://api.jakspeedruns.workers.dev/v1/highscores/3"},
    {"vulcan", "https://api.jakspeedruns.workers.dev/v1/highscores/4"},
    {"peacemaker", "https://api.jakspeedruns.workers.dev/v1/highscores/5"},
    {"jetboard", "https://api.jakspeedruns.workers.dev/v1/highscores/6"},
    {"onin", "https://api.jakspeedruns.workers.dev/v1/highscores/7"},
    {"mash", "https://api.jakspeedruns.workers.dev/v1/highscores/8"}};
// clang-format on

void callback_fetch_external_speedrun_times(bool success,
                                            const std::string& cache_id,
                                            std::optional<std::string> result) {
  std::scoped_lock lock{background_task_lock};

  if (!success) {
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(true);
    if (result) {
      last_rpc_error = result.value();
    } else {
      last_rpc_error = "Unexpected Error Occurred";
    }
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // TODO - might be nice to have an error if we get an unexpected payload
  if (!result) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // Parse the response
  const auto data = safe_parse_json(result.value());
  if (!data || !data->contains("data") || !data->at("data").contains("players") ||
      !data->at("data").at("players").contains("data") || !data->at("data").contains("runs")) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  auto& players = data->at("data").at("players").at("data");
  auto& runs = data->at("data").at("runs");
  std::vector<std::pair<std::string, float>> times = {};
  for (const auto& run_info : runs) {
    std::pair<std::string, float> time_info;
    if (players.size() > times.size() && players.at(times.size()).contains("names") &&
        players.at(times.size()).at("names").contains("international")) {
      time_info.first = players.at(times.size()).at("names").at("international");
    } else if (players.size() > times.size() && players.at(times.size()).contains("name")) {
      time_info.first = players.at(times.size()).at("name");
    } else {
      time_info.first = "Unknown";
    }
    if (run_info.contains("run") && run_info.at("run").contains("times") &&
        run_info.at("run").at("times").contains("primary_t")) {
      time_info.second = run_info.at("run").at("times").at("primary_t");
      times.push_back(time_info);
    }
  }
  external_speedrun_time_cache[cache_id] = times;
  intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
}

// TODO - duplicate code, put it in a function
void callback_fetch_external_race_times(bool success,
                                        const std::string& cache_id,
                                        std::optional<std::string> result) {
  std::scoped_lock lock{background_task_lock};

  if (!success) {
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(true);
    if (result) {
      last_rpc_error = result.value();
    } else {
      last_rpc_error = "Unexpected Error Occurred";
    }
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // TODO - might be nice to have an error if we get an unexpected payload
  if (!result) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // Parse the response
  const auto data = safe_parse_json(result.value());
  if (!data || !data->contains("data") || !data->at("data").contains("players") ||
      !data->at("data").at("players").contains("data") || !data->at("data").contains("runs")) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  auto& players = data->at("data").at("players").at("data");
  auto& runs = data->at("data").at("runs");
  std::vector<std::pair<std::string, float>> times = {};
  for (const auto& run_info : runs) {
    std::pair<std::string, float> time_info;
    if (players.size() > times.size() && players.at(times.size()).contains("names") &&
        players.at(times.size()).at("names").contains("international")) {
      time_info.first = players.at(times.size()).at("names").at("international");
    } else if (players.size() > times.size() && players.at(times.size()).contains("name")) {
      time_info.first = players.at(times.size()).at("name");
    } else {
      time_info.first = "Unknown";
    }
    if (run_info.contains("run") && run_info.at("run").contains("times") &&
        run_info.at("run").at("times").contains("primary_t")) {
      time_info.second = run_info.at("run").at("times").at("primary_t");
      times.push_back(time_info);
    }
  }
  external_race_time_cache[cache_id] = times;
  intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
}

// TODO - duplicate code, put it in a function
void callback_fetch_external_highscores(bool success,
                                        const std::string& cache_id,
                                        std::optional<std::string> result) {
  std::scoped_lock lock{background_task_lock};

  if (!success) {
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(true);
    if (result) {
      last_rpc_error = result.value();
    } else {
      last_rpc_error = "Unexpected Error Occurred";
    }
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // TODO - might be nice to have an error if we get an unexpected payload
  if (!result) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
    return;
  }

  // Parse the response
  const auto data = safe_parse_json(result.value());
  std::vector<std::pair<std::string, float>> times = {};
  for (const auto& highscore_info : data.value()) {
    if (highscore_info.contains("playerName") && highscore_info.contains("score")) {
      std::pair<std::string, float> time_info;
      time_info.first = highscore_info.at("playerName");
      time_info.second = highscore_info.at("score");
      times.push_back(time_info);
    }
  }
  external_highscores_cache[cache_id] = times;
  intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(false);
}

void pc_fetch_external_speedrun_times(u32 speedrun_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto speedrun_id = std::string(Ptr<String>(speedrun_id_ptr).c()->data());
  if (external_speedrun_lookup_urls.find(speedrun_id) == external_speedrun_lookup_urls.end()) {
    lg::error("No URL for speedrun_id: '{}'", speedrun_id);
    return;
  }

  // First check to see if we've already retrieved this info
  if (external_speedrun_time_cache.find(speedrun_id) == external_speedrun_time_cache.end()) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(true);
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(false);
    // otherwise, hit the URL
    WebRequestJobPayload req;
    req.callback = callback_fetch_external_speedrun_times;
    req.url = external_speedrun_lookup_urls.at(speedrun_id);
    req.cache_id = speedrun_id;
    g_background_worker.enqueue_webrequest(req);
  }
}

void pc_fetch_external_race_times(u32 race_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto race_id = std::string(Ptr<String>(race_id_ptr).c()->data());
  if (external_race_lookup_urls.find(race_id) == external_race_lookup_urls.end()) {
    lg::error("No URL for race_id: '{}'", race_id);
    return;
  }

  // First check to see if we've already retrieved this info
  if (external_race_time_cache.find(race_id) == external_race_time_cache.end()) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(true);
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(false);
    // otherwise, hit the URL
    WebRequestJobPayload req;
    req.callback = callback_fetch_external_race_times;
    req.url = external_race_lookup_urls.at(race_id);
    req.cache_id = race_id;
    g_background_worker.enqueue_webrequest(req);
  }
}

void pc_fetch_external_highscores(u32 highscore_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto highscore_id = std::string(Ptr<String>(highscore_id_ptr).c()->data());
  if (external_highscores_lookup_urls.find(highscore_id) == external_highscores_lookup_urls.end()) {
    lg::error("No URL for highscore_id: '{}'", highscore_id);
    return;
  }

  // First check to see if we've already retrieved this info
  if (external_highscores_cache.find(highscore_id) == external_highscores_cache.end()) {
    intern_from_c("*pc-waiting-on-rpc?*")->value() = bool_to_symbol(true);
    intern_from_c("*pc-rpc-error?*")->value() = bool_to_symbol(false);
    // otherwise, hit the URL
    WebRequestJobPayload req;
    req.callback = callback_fetch_external_highscores;
    req.url = external_highscores_lookup_urls.at(highscore_id);
    req.cache_id = highscore_id;
    g_background_worker.enqueue_webrequest(req);
  }
}

void pc_get_external_speedrun_time(u32 speedrun_id_ptr,
                                   s32 index,
                                   u32 name_dest_ptr,
                                   u32 time_dest_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto speedrun_id = std::string(Ptr<String>(speedrun_id_ptr).c()->data());
  if (external_speedrun_time_cache.find(speedrun_id) != external_speedrun_time_cache.end()) {
    const auto& runs = external_speedrun_time_cache.at(speedrun_id);
    if (index < (int)runs.size()) {
      const auto& run_info = external_speedrun_time_cache.at(speedrun_id).at(index);
      std::string converted =
          get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game(run_info.first);
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = run_info.second;
    } else {
      std::string converted = get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game("");
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = -1.0;
    }
  }
}

void pc_get_external_race_time(u32 race_id_ptr, s32 index, u32 name_dest_ptr, u32 time_dest_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto race_id = std::string(Ptr<String>(race_id_ptr).c()->data());
  if (external_race_time_cache.find(race_id) != external_race_time_cache.end()) {
    const auto& runs = external_race_time_cache.at(race_id);
    if (index < (int)runs.size()) {
      const auto& run_info = external_race_time_cache.at(race_id).at(index);
      std::string converted =
          get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game(run_info.first);
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = run_info.second;
    } else {
      std::string converted = get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game("");
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = -1.0;
    }
  }
}

void pc_get_external_highscore(u32 highscore_id_ptr,
                               s32 index,
                               u32 name_dest_ptr,
                               u32 time_dest_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto highscore_id = std::string(Ptr<String>(highscore_id_ptr).c()->data());
  if (external_highscores_cache.find(highscore_id) != external_highscores_cache.end()) {
    const auto& runs = external_highscores_cache.at(highscore_id);
    if (index < (int)runs.size()) {
      const auto& run_info = external_highscores_cache.at(highscore_id).at(index);
      std::string converted =
          get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game(run_info.first);
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = run_info.second;
    } else {
      std::string converted = get_font_bank(GameTextVersion::JAK2)->convert_utf8_to_game("");
      strcpy(Ptr<String>(name_dest_ptr).c()->data(), converted.c_str());
      *(Ptr<float>(time_dest_ptr).c()) = -1.0;
    }
  }
}

s32 pc_get_num_external_speedrun_times(u32 speedrun_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto speedrun_id = std::string(Ptr<String>(speedrun_id_ptr).c()->data());
  if (external_speedrun_time_cache.find(speedrun_id) != external_speedrun_time_cache.end()) {
    return external_speedrun_time_cache.at(speedrun_id).size();
  }
  return 0;
}

s32 pc_get_num_external_race_times(u32 race_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto race_id = std::string(Ptr<String>(race_id_ptr).c()->data());
  if (external_race_time_cache.find(race_id) != external_race_time_cache.end()) {
    return external_race_time_cache.at(race_id).size();
  }
  return 0;
}

s32 pc_get_num_external_highscores(u32 highscore_id_ptr) {
  std::scoped_lock lock{background_task_lock};
  auto highscore_id = std::string(Ptr<String>(highscore_id_ptr).c()->data());
  if (external_highscores_cache.find(highscore_id) != external_highscores_cache.end()) {
    return external_highscores_cache.at(highscore_id).size();
  }
  return 0;
}

void to_json(json& j, const SpeedrunPracticeEntryHistoryAttempt& obj) {
  if (obj.time) {
    j["time"] = obj.time.value();
  } else {
    j["time"] = nullptr;
  }
}

void from_json(const json& j, SpeedrunPracticeEntryHistoryAttempt& obj) {
  if (j["time"].is_null()) {
    obj.time = {};
  } else {
    obj.time = j["time"];
  }
}

void to_json(json& j, const SpeedrunPracticeEntry& obj) {
  json_serialize(name);
  json_serialize(continue_point_name);
  json_serialize(flags);
  json_serialize(completed_task);
  json_serialize(features);
  json_serialize(secrets);
  json_serialize(starting_position);
  json_serialize(starting_rotation);
  json_serialize(starting_camera_position);
  json_serialize(starting_camera_rotation);
  json_serialize(start_zone_v1);
  json_serialize(start_zone_v2);
  json_serialize_optional(end_zone_v1);
  json_serialize_optional(end_zone_v2);
  json_serialize_optional(end_task);
  json_serialize(history);
}

void from_json(const json& j, SpeedrunPracticeEntry& obj) {
  json_deserialize_if_exists(name);
  json_deserialize_if_exists(continue_point_name);
  json_deserialize_if_exists(flags);
  json_deserialize_if_exists(completed_task);
  json_deserialize_if_exists(features);
  json_deserialize_if_exists(secrets);
  json_deserialize_if_exists(starting_position);
  json_deserialize_if_exists(starting_rotation);
  json_deserialize_if_exists(starting_camera_position);
  json_deserialize_if_exists(starting_camera_rotation);
  json_deserialize_if_exists(start_zone_v1);
  json_deserialize_if_exists(start_zone_v2);
  json_deserialize_optional_if_exists(end_zone_v1);
  json_deserialize_optional_if_exists(end_zone_v2);
  json_deserialize_optional_if_exists(end_task);
  json_deserialize_if_exists(history);
}

void to_json(json& j, const SpeedrunCustomCategoryEntry& obj) {
  json_serialize(name);
  json_serialize(secrets);
  json_serialize(features);
  json_serialize(forbidden_features);
  json_serialize(cheats);
  json_serialize(continue_point_name);
  json_serialize(completed_task);
}

void from_json(const json& j, SpeedrunCustomCategoryEntry& obj) {
  json_deserialize_if_exists(name);
  json_deserialize_if_exists(secrets);
  json_deserialize_if_exists(features);
  json_deserialize_if_exists(forbidden_features);
  json_deserialize_if_exists(cheats);
  json_deserialize_if_exists(continue_point_name);
  json_deserialize_if_exists(completed_task);
}

std::vector<SpeedrunPracticeEntry> g_speedrun_practice_entries;
std::unordered_map<int, SpeedrunPracticeState> g_speedrun_practice_state;

s32 pc_sr_mode_get_practice_entries_amount() {
  // load practice entries from the file
  const auto file_path =
      file_util::get_user_features_dir(g_game_version) / "speedrun-practice.json";
  if (!file_util::file_exists(file_path.string())) {
    lg::info("speedrun-practice.json not found, no entries to return!");
    return 0;
  }
  const auto file_contents = safe_parse_json(file_util::read_text_file(file_path));
  if (!file_contents) {
    lg::error("speedrun-practice.json could not be parsed!");
    return 0;
  }

  g_speedrun_practice_entries = *file_contents;

  for (size_t i = 0; i < g_speedrun_practice_entries.size(); i++) {
    const auto& entry = g_speedrun_practice_entries.at(i);
    s32 last_session_id = -1;
    s32 total_attempts = 0;
    s32 total_successes = 0;
    s32 session_attempts = 0;
    s32 session_successes = 0;
    double total_time = 0;
    float average_time = 0;
    float fastest_time = 0;
    for (const auto& [history_session, times] : entry.history) {
      s32 session_id = stoi(history_session);
      if (session_id > last_session_id) {
        last_session_id = session_id;
      }
      for (const auto& time : times) {
        total_attempts++;
        if (time.time) {
          total_successes++;
          total_time += *time.time;
          if (fastest_time == 0 || *time.time < fastest_time) {
            fastest_time = *time.time;
          }
        }
      }
    }
    if (total_successes != 0) {
      average_time = total_time / total_successes;
    }
    g_speedrun_practice_state[i] = {last_session_id + 1, total_attempts,    total_successes,
                                    session_attempts,    session_successes, total_time,
                                    average_time,        fastest_time};
  }

  return g_speedrun_practice_entries.size();
}

void pc_sr_mode_get_practice_entry_name(s32 entry_index, u32 name_str_ptr) {
  std::string name = "";
  if (entry_index < (int)g_speedrun_practice_entries.size()) {
    name = g_speedrun_practice_entries.at(entry_index).name;
  }
  strcpy(Ptr<String>(name_str_ptr).c()->data(), name.c_str());
}

void pc_sr_mode_get_practice_entry_continue_point(s32 entry_index, u32 name_str_ptr) {
  std::string name = "";
  if (entry_index < (int)g_speedrun_practice_entries.size()) {
    name = g_speedrun_practice_entries.at(entry_index).continue_point_name;
  }
  strcpy(Ptr<String>(name_str_ptr).c()->data(), name.c_str());
}

s32 pc_sr_mode_get_practice_entry_history_success(s32 entry_index) {
  return g_speedrun_practice_state.at(entry_index).total_successes;
}

s32 pc_sr_mode_get_practice_entry_history_attempts(s32 entry_index) {
  return g_speedrun_practice_state.at(entry_index).total_attempts;
}

s32 pc_sr_mode_get_practice_entry_session_success(s32 entry_index) {
  return g_speedrun_practice_state.at(entry_index).session_successes;
}

s32 pc_sr_mode_get_practice_entry_session_attempts(s32 entry_index) {
  return g_speedrun_practice_state.at(entry_index).session_attempts;
}

void pc_sr_mode_get_practice_entry_avg_time(s32 entry_index, u32 time_str_ptr) {
  const auto time = fmt::format("{:.2f}", g_speedrun_practice_state.at(entry_index).average_time);
  strcpy(Ptr<String>(time_str_ptr).c()->data(), time.c_str());
}

void pc_sr_mode_get_practice_entry_fastest_time(s32 entry_index, u32 time_str_ptr) {
  const auto time = fmt::format("{:.2f}", g_speedrun_practice_state.at(entry_index).fastest_time);
  strcpy(Ptr<String>(time_str_ptr).c()->data(), time.c_str());
}

u64 pc_sr_mode_record_practice_entry_attempt(s32 entry_index, u32 success_bool, u32 time_ptr) {
  auto& state = g_speedrun_practice_state.at(entry_index);
  const auto was_successful = symbol_to_bool(success_bool);
  state.total_attempts++;
  state.session_attempts++;
  bool ret = false;
  SpeedrunPracticeEntryHistoryAttempt new_history_entry;
  if (was_successful) {
    auto time = Ptr<float>(time_ptr).c();
    new_history_entry.time = *time;
    state.total_successes++;
    state.session_successes++;
    state.total_time += *time;
    state.average_time = state.total_time / state.total_successes;
    if (*time < state.fastest_time) {
      state.fastest_time = *time;
      ret = true;
    }
  }
  // persist to file
  const auto file_path =
      file_util::get_user_features_dir(g_game_version) / "speedrun-practice.json";
  if (!file_util::file_exists(file_path.string())) {
    lg::info("speedrun-practice.json not found, not persisting!");
  } else {
    auto& history = g_speedrun_practice_entries.at(entry_index).history;
    if (history.find(fmt::format("{}", state.current_session_id)) == history.end()) {
      history[fmt::format("{}", state.current_session_id)] = {};
    }
    history[fmt::format("{}", state.current_session_id)].push_back(new_history_entry);
    json data = g_speedrun_practice_entries;
    file_util::write_text_file(file_path, data.dump(2));
  }
  // return
  return bool_to_symbol(ret);
}

void pc_sr_mode_init_practice_info(s32 entry_index, u32 speedrun_practice_obj_ptr) {
  if (entry_index >= (int)g_speedrun_practice_entries.size()) {
    return;
  }

  auto objective = speedrun_practice_obj_ptr
                       ? Ptr<SpeedrunPracticeObjective>(speedrun_practice_obj_ptr).c()
                       : NULL;
  if (objective) {
    const auto& json_info = g_speedrun_practice_entries.at(entry_index);

    objective->index = entry_index;
    objective->flags = json_info.flags;
    objective->completed_task = json_info.completed_task;
    objective->features = json_info.features;
    objective->secrets = json_info.secrets;
    auto starting_position =
        objective->starting_position ? Ptr<Vector>(objective->starting_position).c() : NULL;
    if (starting_position) {
      for (int i = 0; i < 4; i++) {
        starting_position->data[i] = json_info.starting_position.at(i) * 4096.0;
      }
    }
    auto starting_rotation =
        objective->starting_rotation ? Ptr<Vector>(objective->starting_rotation).c() : NULL;
    if (starting_rotation) {
      for (int i = 0; i < 4; i++) {
        starting_rotation->data[i] = json_info.starting_rotation.at(i);
      }
    }
    auto starting_camera_position = objective->starting_camera_position
                                        ? Ptr<Vector>(objective->starting_camera_position).c()
                                        : NULL;
    if (starting_camera_position) {
      for (int i = 0; i < 4; i++) {
        starting_camera_position->data[i] = json_info.starting_camera_position.at(i) * 4096.0;
      }
    }
    auto starting_camera_rotation = objective->starting_camera_rotation
                                        ? Ptr<Vector>(objective->starting_camera_rotation).c()
                                        : NULL;
    if (starting_camera_rotation) {
      for (int i = 0; i < 16; i++) {
        starting_camera_rotation->data[i] = json_info.starting_camera_rotation.at(i);
      }
    }

    if (json_info.end_task) {
      objective->end_task = *json_info.end_task;
    } else {
      objective->end_task = 0;
    }

    auto starting_zone = objective->start_zone_init_params
                             ? Ptr<ObjectiveZoneInitParams>(objective->start_zone_init_params).c()
                             : NULL;
    if (starting_zone) {
      starting_zone->v1[0] = json_info.start_zone_v1.at(0) * 4096.0;
      starting_zone->v1[1] = json_info.start_zone_v1.at(1) * 4096.0;
      starting_zone->v1[2] = json_info.start_zone_v1.at(2) * 4096.0;
      starting_zone->v1[3] = json_info.start_zone_v1.at(3) * 4096.0;
      starting_zone->v2[0] = json_info.start_zone_v2.at(0) * 4096.0;
      starting_zone->v2[1] = json_info.start_zone_v2.at(1) * 4096.0;
      starting_zone->v2[2] = json_info.start_zone_v2.at(2) * 4096.0;
      starting_zone->v2[3] = json_info.start_zone_v2.at(3) * 4096.0;
    }

    if (json_info.end_zone_v1 && json_info.end_zone_v2) {
      auto ending_zone = objective->end_zone_init_params
                             ? Ptr<ObjectiveZoneInitParams>(objective->end_zone_init_params).c()
                             : NULL;
      if (ending_zone) {
        ending_zone->v1[0] = json_info.end_zone_v1->at(0) * 4096.0;
        ending_zone->v1[1] = json_info.end_zone_v1->at(1) * 4096.0;
        ending_zone->v1[2] = json_info.end_zone_v1->at(2) * 4096.0;
        ending_zone->v1[3] = json_info.end_zone_v1->at(3) * 4096.0;
        ending_zone->v2[0] = json_info.end_zone_v2->at(0) * 4096.0;
        ending_zone->v2[1] = json_info.end_zone_v2->at(1) * 4096.0;
        ending_zone->v2[2] = json_info.end_zone_v2->at(2) * 4096.0;
        ending_zone->v2[3] = json_info.end_zone_v2->at(3) * 4096.0;
      }
    }
  }
}

std::vector<SpeedrunCustomCategoryEntry> g_speedrun_custom_categories;

s32 pc_sr_mode_get_custom_category_amount() {
  // load practice entries from the file
  const auto file_path =
      file_util::get_user_features_dir(g_game_version) / "speedrun-categories.json";
  if (!file_util::file_exists(file_path.string())) {
    lg::info("speedrun-categories.json not found, no entries to return!");
    return 0;
  }
  const auto file_contents = safe_parse_json(file_util::read_text_file(file_path));
  if (!file_contents) {
    lg::error("speedrun-categories.json could not be parsed!");
    return 0;
  }

  g_speedrun_custom_categories = *file_contents;

  return g_speedrun_custom_categories.size();
}

void pc_sr_mode_get_custom_category_name(s32 entry_index, u32 name_str_ptr) {
  std::string name = "";
  if (entry_index < (int)g_speedrun_custom_categories.size()) {
    name = g_speedrun_custom_categories.at(entry_index).name;
  }
  strcpy(Ptr<String>(name_str_ptr).c()->data(), name.c_str());
}

void pc_sr_mode_get_custom_category_continue_point(s32 entry_index, u32 name_str_ptr) {
  std::string name = "";
  if (entry_index < (int)g_speedrun_custom_categories.size()) {
    name = g_speedrun_custom_categories.at(entry_index).continue_point_name;
  }
  strcpy(Ptr<String>(name_str_ptr).c()->data(), name.c_str());
}

void pc_sr_mode_init_custom_category_info(s32 entry_index, u32 speedrun_custom_category_ptr) {
  if (entry_index >= (int)g_speedrun_custom_categories.size()) {
    return;
  }

  auto category = speedrun_custom_category_ptr
                      ? Ptr<SpeedrunCustomCategory>(speedrun_custom_category_ptr).c()
                      : NULL;
  if (category) {
    const auto& json_info = g_speedrun_custom_categories.at(entry_index);
    category->index = entry_index;
    category->secrets = json_info.secrets;
    category->features = json_info.features;
    category->forbidden_features = json_info.forbidden_features;
    category->cheats = json_info.cheats;
    category->completed_task = json_info.completed_task;
  }
}

void pc_sr_mode_dump_new_custom_category(u32 speedrun_custom_category_ptr) {
  const auto file_path =
      file_util::get_user_features_dir(g_game_version) / "speedrun-categories.json";
  if (file_util::file_exists(file_path.string())) {
    // read current categories from file
    const auto file_contents = safe_parse_json(file_util::read_text_file(file_path));
    if (file_contents) {
      g_speedrun_custom_categories = *file_contents;
    }
  }

  auto category = speedrun_custom_category_ptr
                      ? Ptr<SpeedrunCustomCategory>(speedrun_custom_category_ptr).c()
                      : NULL;
  if (category) {
    SpeedrunCustomCategoryEntry new_category;
    new_category.name = fmt::format("custom-category-{}", g_speedrun_custom_categories.size());
    new_category.secrets = category->secrets;
    new_category.features = category->features;
    new_category.forbidden_features = category->forbidden_features;
    new_category.cheats = category->cheats;
    new_category.completed_task = category->completed_task;
    new_category.continue_point_name = "";
    g_speedrun_custom_categories.push_back(new_category);
    // convert to json and write file
    json data = g_speedrun_custom_categories;
    file_util::write_text_file(file_path, data.dump(2));
  }
  return;
}

}  // namespace kmachine_extras
