#include "kmachine_extras.h"

#include <bitset>
#include <regex>

#include "kscheme.h"

#include "common/symbols.h"
#include "common/util/FontUtils.h"

#include "game/external/discord_jak3.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kscheme.h"

namespace jak3 {
namespace kmachine_extras {

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
      std::string status = get_font_bank(GameTextVersion::JAK3)
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
      std::bitset<64> focus_status = info->focus_status;
      auto vehicle = static_cast<VehicleType>(info->current_vehicle);
      char* task = Ptr<String>(info->task).c()->data();

      // Construct the DiscordRPC Object
      const char* full_level_name =
          get_full_level_name(level_names, level_name_remap, Ptr<String>(info->level).c()->data());
      memset(&rpc, 0, sizeof(rpc));
      // if we have an active task, set the mission specific image for it
      if (strcmp(task, "unknown") != 0) {
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
        strcpy(small_image_text, "Controlling a Dark Maker bot");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Pilot)) {
        // TODO vehicle images
        strcpy(small_image_key, "focus-status-pilot");
        auto vehicle_name = VehicleTypeToString(vehicle);
        if (!strcmp(task, "comb-travel") || !strcmp(task, "comb-wild-ride")) {
          strcpy(small_image_text, "Driving the Catacombs Rail Rider");
        } else if (!strcmp(task, "desert-glide")) {
          strcpy(small_image_text, "Flying the Glider");
        } else if (!strcmp(task, "factory-sky-battle")) {
          strcpy(small_image_text, "Flying the Hellcat");
        } else {
          if (vehicle_name != "Unknown") {
            strcpy(small_image_text, fmt::format("Driving the {}", vehicle_name).c_str());
          } else {
            strcpy(small_image_key, "");
            strcpy(small_image_text, "");
          }
        }
      } else if (FOCUS_TEST(focus_status, FocusStatus::Indax)) {
        strcpy(small_image_key, "focus-status-indax");
        strcpy(small_image_text, "Playing as Daxter");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Dark)) {
        strcpy(small_image_key, "focus-status-dark");
        strcpy(small_image_text, "Dark Jak");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Light)) {
        strcpy(small_image_key, "focus-status-light");
        strcpy(small_image_text, "Light Jak");
      } else if (FOCUS_TEST(focus_status, FocusStatus::Turret)) {
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

inline u64 bool_to_symbol(const bool val) {
  return val ? static_cast<u64>(s7.offset) + true_symbol_offset(g_game_version) : s7.offset;
}

inline bool symbol_to_bool(const u32 symptr) {
  return symptr != s7.offset;
}

// TODO - move to common
void encode_utf8_string(u32 src_str_ptr, u32 str_dest_ptr) {
  auto str = std::string(Ptr<String>(src_str_ptr).c()->data());
  std::string converted = get_font_bank(GameTextVersion::JAK3)->convert_utf8_to_game(str);
  strcpy(Ptr<String>(str_dest_ptr).c()->data(), converted.c_str());
}

}  // namespace kmachine_extras
}  // namespace jak3
