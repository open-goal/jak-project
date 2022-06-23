#pragma once
#include <string>
#include <unordered_map>

#include "common/util/FontUtils.h"

namespace decompiler {
struct ObjectFileData;

struct GameTextResult {
  int total_text = 0;
  int language = -1;
  std::unordered_map<int, std::string> text;
  int total_chars = 0;
};

GameTextResult process_game_text(ObjectFileData& data, GameTextVersion version);
std::string write_game_text(
    GameTextVersion version,
    const std::unordered_map<int, std::unordered_map<int, std::string>>& data);
}  // namespace decompiler
