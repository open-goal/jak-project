#pragma once
#include <string>
#include <unordered_map>

struct ObjectFileData;

struct GameTextResult {
  int total_text = 0;
  int language = -1;
  std::unordered_map<int, std::string> text;
  int total_chars = 0;
};

GameTextResult process_game_text(ObjectFileData& data);
std::string write_game_text(
    const std::unordered_map<int, std::unordered_map<int, std::string>>& data);