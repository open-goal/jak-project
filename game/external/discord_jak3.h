#pragma once

#include "discord.h"

namespace jak3 {
extern const std::map<std::string, std::string> level_names;
extern std::map<std::string, std::string> level_name_remap;
void remap_hack();
extern const std::vector<std::string> indoor_levels;
const char* time_of_day_str(float time);
}  // namespace jak3
