#pragma once

#include <unordered_set>
#include <vector>

#include "decompiler/Disasm/Register.h"

namespace decompiler {

class Function;

using RegSet = std::unordered_set<Register, Register::hash>;

struct RegUsageInfo {
  struct PerBlock {
    RegSet use, defs, input, output;
  };

  struct PerOp {
    RegSet live, dead, consumes, written_and_unused, live_in;
  };

  int block_count() const { return int(block.size()); }

  std::vector<PerBlock> block;
  std::vector<PerOp> op;

  RegUsageInfo() = default;
  RegUsageInfo(int n_blocks, int n_ops);
};

RegUsageInfo analyze_ir2_register_usage(const Function& function);
}  // namespace decompiler