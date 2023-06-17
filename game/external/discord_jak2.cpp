#include "discord_jak2.h"

namespace jak2 {
const std::map<std::string, std::string> level_names = {
    {"intro", "Intro"},
    {"title", "Title screen"},
    {"prison", "Prison"},
    {"forexit", "Fortress (Escape)"},
    {"fordump", "Fortress (Ammo Dump)"},
    {"forresc", "Fortress (Rescue Friends)"},
    {"ctyslum", "Haven City (Slums)"},
    {"ctyport", "Haven City (Port)"},
    {"ctyfarm", "Haven City (Gardens)"},
    {"ctyind", "Haven City (Industrial Zone)"},
    {"ctymark", "Haven City (Bazaar)"},
    {"ctypal", "Haven City (Palace Area)"},
    {"ctygen", "Haven City (Main Town)"},
    {"stadium", "Stadium"},
    {"stadiumb", "Stadium (Class 3 Race)"},
    {"stadiumc", "Stadium (Class 2 Race)"},
    {"stadiumd", "Stadium (Class 1 Race)"},
    {"skatea", "Stadium (JET-Board Course)"},
    {"palshaft", "Palace Lobby"},
    {"palcab", "Palace Cable"},
    {"palroof", "Palace Roof"},
    {"throne", "Throne Room"},
    {"palent", "Palace Interior"},
    {"vinroom", "Power Station"},
    {"oracle", "Oracle"},
    {"onintent", "Onin's Tent"},
    {"hiphog", "Hip Hog"},
    {"hideout", "Underground Hideout"},
    {"gungame", "Gun Course"},
    {"caspad", "Landing Pad"},
    {"castle", "Weapons Factory"},
    {"ruins", "Dead Town"},
    {"atoll", "Pumping Station"},
    {"sewer", "Sewer"},
    {"strip", "Strip Mine"},
    {"tomb", "Mar's Tomb"},
    {"dig", "Dig Site"},
    {"drill", "Drill Platform"},
    {"mountain", "Mountain Temple"},
    {"forest", "Haven Forest"},
    {"mincan", "No Man's Canyon"},
    {"consite", "Construction Site"},
    {"under", "Underport"},
    {"nest", "Metal Head Nest"},
    {"village1", "Sandover Village"},
};

// for remapping sub-level names to the matching one in level_names
const std::map<std::string, std::string> level_name_remap = {
    {"forexita", "forexit"}, {"forexitb", "forexit"}, {"fordumpa", "fordump"},
    {"fordumpb", "fordump"}, {"fordumpc", "fordump"}, {"forresca", "forresc"},
    {"forrescb", "forresc"}, {"ctysluma", "ctyslum"}, {"ctyslumb", "ctyslum"},
    {"ctyslumc", "ctyslum"}, {"ctyfarma", "ctyfarm"}, {"ctyfarmb", "ctyfarm"},
    {"ctyinda", "ctyind"},   {"ctyindb", "ctyind"},   {"ctymarka", "ctymark"},
    {"ctymarkb", "ctymark"}, {"ctygena", "ctygen"},   {"ctygenb", "ctygen"},
    {"ctygenc", "ctygen"},   {"tomba", "tomb"},       {"tombb", "tomb"},
    {"tombboss", "tomb"},    {"tombc", "tomb"},       {"tombd", "tomb"},
    {"tombe", "tomb"},       {"dig1", "dig"},         {"dig3a", "dig"},
    {"drillmid", "drill"},   {"nestb", "nest"},       {"sewesc", "sewer"},
    {"garage", "stadium"},   {"casboss", "castle"},   {"introcst", "intro"},
    {"underb", "under"},     {"sagehut", "ruins"},    {"atollext", "atoll"},
    {"mtnext", "mountain"},  {"consiteb", "consite"}, {"drillmtn", "drill"},
    {"drillb", "drill"},     {"forestb", "forest"},   {"sewerb", "sewer"},
    {"sewescb", "sewer"},
};

// levels that are not affected by time of day
const std::vector<std::string> indoor_levels = {
    "intro",    "introcst", "title",    "prison", "forexita", "forexitb", "fordumpa", "fordumpb",
    "fordumpc", "forresca", "forrescb", "tomba",  "tombb",    "tombc",    "tombd",    "dig1",
    "dig3a",    "dig3b",    "palshaft", "sewer",  "sewesc",   "castle",   "tombe",    "tombboss",
    "gungame",  "hideout",  "vinroom",  "under",  "onintent", "oracle",   "hiphog",   "casboss"};

// time of day string to append to level name for icons
const char* time_of_day_str(float time) {
  int hour = static_cast<int>(time);

  if (hour > 6 && hour < 19) {
    return "day";
  } else {
    return "night";
  }
}
}  // namespace jak2