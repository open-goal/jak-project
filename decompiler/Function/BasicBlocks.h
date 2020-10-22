#pragma once

#include <vector>
#include <memory>

#include "CfgVtx.h"

class LinkedObjectFile;
class Function;

struct BasicBlock {
  int start_word;
  int end_word;

  std::vector<int> pred;
  int succ_ft = -1;
  int succ_branch = -1;

  BasicBlock(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}
};

std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func);
