#pragma once
#include <string>
#include <unordered_map>

namespace decompiler {
struct ObjectFileData;

struct GameTextResult {
  int total_text = 0;
  int language = -1;
  std::unordered_map<int, std::string> text;
  int total_chars = 0;
};

enum class GameTextVersion { JAK1_V1 = 10, JAK1_V2 = 11, JAK2 = 20, JAK3 = 30, JAKX = 40 };

GameTextResult process_game_text(ObjectFileData& data, GameTextVersion version);
std::string write_game_text(
    const std::unordered_map<int, std::unordered_map<int, std::string>>& data);
}  // namespace decompiler
