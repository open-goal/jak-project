#pragma once

#include <vector>
#include <memory>

#include "CfgVtx.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/TP_Type.h"

class LinkedObjectFile;
class Function;

using RegSet = std::unordered_set<Register, Register::hash>;

struct BasicBlock {
  int start_word;
  int end_word;
  TypeState init_types;

  // [start, end)
  int start_basic_op = -1;
  int end_basic_op = -1;
  int basic_op_size() const { return end_basic_op - start_basic_op; }

  std::string label_name;

  std::vector<int> pred;
  int succ_ft = -1;
  int succ_branch = -1;

  std::vector<RegSet> live, dead;
  RegSet use, defs;
  RegSet input, output;

  bool op_has_reg_live_out(int basic_op_idx, Register reg) {
    auto& lv = live.at(basic_op_idx - start_basic_op);
    return lv.find(reg) != lv.end();
  }

  BasicBlock(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}
};

struct BlockTopologicalSort {
  std::vector<int> vist_order;
  std::unordered_set<int> unreachable;
};

std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func);
