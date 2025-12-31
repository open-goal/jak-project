#include "discord_jakx.h"

namespace jakx {
const std::map<std::string, std::string> level_names = {};

// for remapping sub-level names to the matching one in level_names
std::map<std::string, std::string> level_name_remap = {};

const std::map<std::string, std::pair<char, char>> level_remap_hack = {};

void remap_hack() {
  for (auto& name : level_remap_hack) {
    auto base_name = name.first;
    auto suffix_start = name.second.first;
    auto suffix_end = name.second.second;
    for (int i = 0; i < suffix_end - suffix_start; i++) {
      auto suffix = static_cast<char>(suffix_start + i);
      std::string level(base_name);
      level.push_back(suffix);
      level_name_remap.insert(std::make_pair(level, base_name));
    }
  }
}

// levels that are not affected by time of day
const std::vector<std::string> indoor_levels = {};

// time of day string to append to level name for icons
const char* time_of_day_str(float time) {
  int hour = static_cast<int>(time);

  if (hour > 6 && hour < 19) {
    return "day";
  } else {
    return "night";
  }
}
}  // namespace jakx