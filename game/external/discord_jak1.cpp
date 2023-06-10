#include "discord_jak1.h"

namespace jak1 {
const std::map<std::string, std::string> level_names = {{"intro", "Intro"},
                                                        {"title", "Title screen"},
                                                        {"training", "Geyser Rock"},
                                                        {"village1", "Sandover Village"},
                                                        {"beach", "Sentinel Beach"},
                                                        {"jungle", "Forbidden Jungle"},
                                                        {"misty", "Misty Island"},
                                                        {"firecanyon", "Fire Canyon"},
                                                        {"village2", "Rock Village"},
                                                        {"swamp", "Boggy Swamp"},
                                                        {"rolling", "Precursor Basin"},
                                                        {"sunken", "Lost Precursor City"},
                                                        {"ogre", "Mountain Pass"},
                                                        {"village3", "Volcanic Crater"},
                                                        {"snow", "Snowy Mountain"},
                                                        {"maincave", "Spider Cave"},
                                                        {"lavatube", "Lava Tube"},
                                                        {"citadel", "Gol and Maia's Citadel"},
                                                        {"finalboss", "Final Boss"}};

// for remapping sub-level names to the matching one in level_names
const std::map<std::string, std::string> level_name_remap = {{"jungleb", "jungle"},
                                                             {"sunkenb", "sunken"},
                                                             {"robocave", "maincave"},
                                                             {"darkcave", "maincave"}};

// levels that are not affected by time of day
const std::vector<std::string> indoor_levels = {
    "intro",    "title",    "jungleb",  "sunken",   "sunkenb",
    "maincave", "robocave", "darkcave", "lavatube", "citadel",
};

// time of day string to append to level name for icons
const char* time_of_day_str(float time) {
  int hour = static_cast<int>(time);

  if (hour >= 0 && hour <= 9) {
    return "green-sun";
  } else if (hour < 22) {
    return "day";
  } else if (hour < 25) {
    return "evening";
  } else {
    return "";
  }
}
}  // namespace jak1