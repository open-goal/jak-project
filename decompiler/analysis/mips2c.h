#pragma once

#include <vector>

#include "common/versions/versions.h"

namespace decompiler {
class Function;

void run_mips2c(Function* f, GameVersion version);
void run_mips2c_jump_table(Function* f,
                           const std::vector<int>& jump_table_locations,
                           GameVersion version);
}  // namespace decompiler
