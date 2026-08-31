#pragma once

#include <cctype>
#include <map>
#include <string>
#include <vector>

namespace flava {

struct Variant {
  const char* name;
  int value;
};

struct FlavaSet {
  int reg;
  std::vector<Variant> variants;
  bool battle_mode = false;
};

inline const FlavaSet* lookup(std::string music_name) {
  for (char& ch : music_name) {
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  }

  static const std::map<std::string, FlavaSet> table = {
      {"village1",
       {0,
        {{"default", 0},
         {"sage", 2},
         {"sage-hut", 3},
         {"birdlady", 4},
         {"farmer", 5},
         {"assistant", 6},
         {"mayor", 7},
         {"sculptor", 8},
         {"explorer", 9},
         {"dock", 10}}}},
      {"jungle",
       {0,
        {{"default", 0},
         {"jungle-temple-exit", 1},
         {"jungle-lurkerm", 2},
         {"jungle-temple-top", 3}}}},
      {"jungleb", {0, {{"default", 0}, {"jungleb-eggtop", 1}}}},
      {"beach",
       {0,
        {{"default", 0},
         {"beach-sentinel", 1},
         {"beach-cannon", 2},
         {"beach-grotto", 3},
         {"birdlady", 4}}}},
      {"misty", {0, {{"default", 0}, {"misty-battle", 1}, {"misty-boat", 2}, {"racer", 3}}}},
      {"firecany", {0, {{"default", 0}, {"racer", 1}, {"unused", 2}}}},
      {"village2",
       {0,
        {{"default", 0},
         {"sage", 1},
         {"assistant", 2},
         {"warrior", 3},
         {"geologist", 4},
         {"gambler", 5},
         {"levitator", 6}}}},
      {"swamp", {0, {{"default", 0}, {"swamp-launcher", 2}, {"swamp-battle", 3}, {"flutflut", 4}}}},
      {"rolling", {0, {{"default", 0}, {"rolling-gorge", 1}}}},
      {"ogre", {0, {{"default", 0}, {"ogre-middle", 1}, {"ogre-end", 2}}}},
      {"village3",
       {0,
        {{"default", 0},
         {"miners", 1},
         {"sage", 2},
         {"assistant", 3},
         {"to-maincave", 4},
         {"to-snow", 5}}}},
      {"maincave",
       {0, 
        {{"default", 0}, {"robocave", 1}, {"robocave-top", 2}, {"maincave", 3}, {"darkcave", 4}}}},
      {"snow",
       {0,
        {{"default", 0},
         {"snow-battle", 1},
         {"flutflut", 2},
         {"snow-cave", 3},
         {"snow-fort", 4},
         {"snow-balls", 5}}}},
      {"lavatube", {0, {{"none", 0}, {"default", 1}, {"lavatube-middle", 2}, {"lavatube-end", 3}}}},
      {"citadel",
       {0,
        {{"default", 0},
         {"sage", 1},
         {"assistant", 2},
         {"sage-yellow", 3},
         {"sage-red", 4},
         {"sage-blue", 5},
         {"citadel-center", 6}}}},
      {"finalbos", {0, {{"default", 0}, {"finalboss-middle", 1}, {"finalboss-end", 2}}}},
      {"credits", {0, {{"none", 0}, {"default", 2}}}},

      // jak2
      {"city1", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"pilot", 5}}, true}},
      {"ruins", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}}},
      {"atoll", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"darkjak", 4}}, true}},
      {"sewer",
       {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}, true}},
      {"fortress", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}}},
      {"strip", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}}},
      {"dig", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}}},
      {"mountain",
       {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}, true}},
      {"palcab", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"darkjak", 4}}, true}},
      {"tomb", {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"darkjak", 4}}, true}},
      {"forest",
       {14, {{"default", 0}, {"gun", 1}, {"board", 2}, {"mech", 3}, {"darkjak", 4}}, true}},
  };

  auto it = table.find(music_name);
  return it == table.end() ? nullptr : &it->second;
}

}  // namespace flava
