#pragma once

#ifndef JAK_ALLOCATOR_H
#define JAK_ALLOCATOR_H

#include <vector>
#include "IRegSet.h"
#include <unordered_map>
#include "IRegister.h"
#include "allocate.h"

struct RegAllocBasicBlock {
  std::vector<int> instr_idx;
  std::vector<int> succ;
  std::vector<int> pred;
  std::vector<IRegSet> live, dead;
  IRegSet use, defs, input, output;
  bool is_entry = false;
  bool is_exit = false;
  int idx = -1;
  void analyze_liveliness_phase1(const std::vector<RegAllocInstr>& instructions);
  bool analyze_liveliness_phase2(std::vector<RegAllocBasicBlock>& blocks,
                                 const std::vector<RegAllocInstr>& instructions);
  void analyze_liveliness_phase3(std::vector<RegAllocBasicBlock>& blocks,
                                 const std::vector<RegAllocInstr>& instructions);
  std::string print(const std::vector<RegAllocInstr>& insts);
  std::string print_summary();
};

struct RegAllocCache {
  std::vector<RegAllocBasicBlock> basic_blocks;
  std::vector<LiveInfo> live_ranges;
  int max_var = -1;
  std::vector<bool> was_colored;
  std::vector<IRegister> iregs;
  std::vector<StackOp> stack_ops;
  std::unordered_map<int, int> var_to_stack_slot;
  int current_stack_slot = 0;
  bool used_stack = false;
  bool is_asm_func = false;

  std::vector<std::vector<int>> live_ranges_by_instr;
};

void find_basic_blocks(RegAllocCache* cache, const AllocationInput& in);
void analyze_liveliness(RegAllocCache* cache, const AllocationInput& in);
void do_constrained_alloc(RegAllocCache* cache, const AllocationInput& in, bool trace_debug);
bool check_constrained_alloc(RegAllocCache* cache, const AllocationInput& in);
bool run_allocator(RegAllocCache* cache, const AllocationInput& in, int debug_trace);

#endif  // JAK_ALLOCATOR_H
