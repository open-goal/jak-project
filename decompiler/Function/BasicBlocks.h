#pragma once

#include <vector>
#include <memory>

#include "CfgVtx.h"
#include "decompiler/util/DecompilerTypeSystem.h"

class LinkedObjectFile;
class Function;

struct BasicBlock {
  int start_word;
  int end_word;
  TypeState init_types;

  int start_basic_op = -1;
  int end_basic_op = -1;

  std::string label_name;

  std::vector<int> pred;
  int succ_ft = -1;
  int succ_branch = -1;

  BasicBlock(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}
};

std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func);
