#pragma once

#include <vector>

namespace decompiler {
class Function;

void run_mips2c(Function* f);
void run_mips2c_jump_table(Function* f, const std::vector<int>& jump_table_locations);
}  // namespace decompiler
