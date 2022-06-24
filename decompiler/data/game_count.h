#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"

namespace decompiler {
struct GameCountResult {
  struct CountInfo {
    s32 money_count;
    s32 buzzer_count;
  };

  std::vector<CountInfo> info;
  u32 mystery_data[2];
};

struct ObjectFileData;
GameCountResult process_game_count(ObjectFileData& data);
std::string write_game_count(const GameCountResult& result);
}  // namespace decompiler
