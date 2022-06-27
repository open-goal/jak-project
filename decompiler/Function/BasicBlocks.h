#pragma once

#include <memory>
#include <vector>

// for RegSet:
#include "decompiler/analysis/reg_usage.h"

namespace decompiler {
class LinkedObjectFile;
class Function;

struct BasicBlock {
  int start_word;
  int end_word;

  std::string label_name;

  std::vector<int> pred;
  int succ_ft = -1;
  int succ_branch = -1;

  BasicBlock(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}
};

struct BlockTopologicalSort {
  std::vector<int> vist_order;
  std::unordered_set<int> unreachable;
};

std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func);
}  // namespace decompiler