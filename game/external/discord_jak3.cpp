#include "discord_jak3.h"

namespace jak3 {
const std::map<std::string, std::string> level_names = {
    {"intro", "Intro"},
    {"title", "Title screen"},
    {"wascity", "Spargus City"},
    {"waspala", "Wasteland Palace"},
    {"wasstad", "Arena of Death"},
    {"wasdoors", "Spargus City (Garage)"},
    {"desert", "Wasteland"},
    {"hang", "Wasteland (Glider)"},
    {"temple", "Monk Temple"},
    {"mine", "Eco Mine"},
    {"comb", "Catacombs"},
    {"rail", "Catacombs"},
    {"nst", "Metal Head Nest"},
    {"factory", "KG War Factory"},
    {"ctyslum", "New Haven"},
    {"precur", "Dark Maker Ship"},
    {"volcano", "Great Volcano"},
    {"ctyport", "Haven City (Port)"},
    {"ctyind", "Haven City (Industrial Zone)"},
    {"ctygen", "Haven City (Main Town)"},
    {"stadium", "Stadium Ruins"},
    {"rubble", "Stadium Ruins"},
    {"vinroom", "Power Station"},
    {"onintent", "Onin's Tent"},
    {"freehq", "Freedom HQ"},
    {"hiphog", "Naughty Ottsel"},
    {"gungame", "Gun Course"},
    {"sew", "Sewer"},
    {"forest", "Haven Forest"},
};

// for remapping sub-level names to the matching one in level_names
std::map<std::string, std::string> level_name_remap = {
    {"introcst", "intro"}, {"templex", "temple"}, {"combx", "comb"},      {"volcanox", "volcano"},
    {"railx", "rail"},     {"railb2", "rail"},    {"rubblea2", "rubble"}, {"wasstada", "wasstad"},
};

const std::map<std::string, std::pair<char, char>> level_remap_hack = {
    {"forest", {'a', 'b'}}, {"factory", {'a', 'd'}}, {"stadium", {'a', 'b'}},
    {"hang", {'a', 'b'}},   {"wascity", {'a', 'b'}}, {"nst", {'a', 'b'}},
    {"mhcity", {'a', 'b'}}, {"sew", {'a', 'o'}},     {"comb", {'a', 'n'}},
    {"rail", {'a', 'e'}},   {"desert", {'a', 'h'}},  {"temple", {'a', 'd'}},
    {"ctygen", {'a', 'c'}}, {"ctyind", {'a', 'b'}},  {"ctyslum", {'a', 'c'}},
    {"mine", {'a', 'e'}},   {"rubble", {'a', 'c'}},  {"precur", {'a', 'd'}},
};

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
const std::vector<std::string> indoor_levels = {
    "intro",   "introcst", "title",  "sew", "gungame", "precur", "hideout",
    "vinroom", "onintent", "hiphog", "nst", "comb",    "rail",   "freehq",
};

// time of day string to append to level name for icons
const char* time_of_day_str(float time) {
  int hour = static_cast<int>(time);

  if (hour > 6 && hour < 19) {
    return "day";
  } else {
    return "night";
  }
}
}  // namespace jak3