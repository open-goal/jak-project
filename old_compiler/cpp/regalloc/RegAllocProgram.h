#ifndef JAK_REGALLOCPROGRAM_H
#define JAK_REGALLOCPROGRAM_H

#include <unordered_map>
#include <vector>
#include <string>
#include <set>
#include <cassert>
#include "regalloc/RegAllocInstr.h"

struct RegAllocBasicBlock {
  std::vector<int> instr_idx, succ, pred;
  std::vector<std::set<int>> live, dead;
  std::set<int> use, defs;
  std::set<int> input, output;

  bool is_entry = false;
  bool is_exit = false;
  int idx;

  std::string print_summary() {
    std::string result = "block " + std::to_string(idx) + "\nsucc: ";
    for (auto s : succ)
      result += std::to_string(s) + " ";
    result += "\npred: ";
    for (auto p : pred)
      result += std::to_string(p) + " ";
    result += "\nuse: ";
    for (auto x : use)
      result += std::to_string(x) + " ";
    result += "\ndef: ";
    for (auto x : defs)
      result += std::to_string(x) + " ";
    result += "\ninput: ";
    for (auto x : input)
      result += std::to_string(x) + " ";
    result += "\noutput: ";
    for (auto x : output)
      result += std::to_string(x) + " ";

    return result;
  }

  std::string print_detailed(std::vector<RegAllocInstr>& insts) {
    std::string result = print_summary() + "\n";
    int k = 0;
    for (auto instr : instr_idx) {
      std::string line = insts.at(instr).print();
      constexpr int pad_len = 30;
      if (line.length() < pad_len) {
        // line.insert(line.begin(), pad_len - line.length(), ' ');
        line.append(pad_len - line.length(), ' ');
      }

      result += "  " + line + " live: ";
      for (auto j : live.at(k)) {
        result += std::to_string(j) + " ";
      }
      result += "\n";

      k++;
    }
    return result;
  }

  void analyze_liveliness_phase1(std::vector<RegAllocInstr>& instructions);
  bool analyze_liveliness_phase2(std::vector<RegAllocBasicBlock>& blocks,
                                 std::vector<RegAllocInstr>& instructions);
  void analyze_liveliness_phase3(std::vector<RegAllocBasicBlock>& blocks,
                                 std::vector<RegAllocInstr>& instructions);
};

class RegAllocProgram {
 public:
  RegAllocProgram() = default;

  int add_instruction(RegAllocInstr& i) {
    instructions.push_back(i);
    return instructions.size() - 1;
  }

  void find_basic_blocks();
  void analyze_block_liveliness(int n_vars);
  void do_constrained_allocations();
  void check_constrained_allocations();
  void allocate();
  void prepare_for_allocation(size_t code_size) {
    for (uint32_t i = 0; i < live_ranges.size(); i++) {
      live_ranges.at(i).prepare_for_allocation(i);
    }
    bonus_instructions.resize(code_size);
  }

  std::vector<int> get_default_alloc_order_for_var(int v);
  std::vector<int> get_default_alloc_order_for_var_spill(int v);

  std::vector<RegAllocInstr> instructions;
  std::vector<RegAllocBasicBlock> basic_blocks;
  std::vector<LiveRange> live_ranges;
  std::vector<RegAllocBonusInstruction> bonus_instructions;
  std::vector<bool> was_colored;
  std::vector<ColoringInput> coloring_input;

  std::string print_all_instrs() {
    std::string result;
    for (auto& instr : instructions) {
      result += instr.print() + "\n";
    }
    return result;
  }

  std::string print_block_summary() {
    std::string result;
    for (auto& b : basic_blocks) {
      result += b.print_summary() + "\n";
    }
    return result;
  }

  std::string print_block_detailed() {
    std::string result;
    for (auto& b : basic_blocks) {
      result += b.print_detailed(instructions) + "\n";
    }
    return result;
  }

  int max_var = 0;
  std::vector<RegConstraint> constraints;
  bool coloring_error = false;

  int get_stack_slot_count() { return current_stack_slot; }

  std::pair<int, int> get_move_stats();
  int get_spill_count();

  bool used_stack = false;

 private:
  void compute_live_ranges();
  void do_allocation_for_var(int var);
  bool try_assignment_for_var(int var, ColoringAssignment ass);
  bool can_var_be_assigned(int var, ColoringAssignment ass);
  void assign_var_no_check(int var, ColoringAssignment ass);
  bool try_spill_coloring(int var);
  bool assignment_ok_at(int var, int idx, ColoringAssignment ass);

  int get_stack_slot_for_var(int var);

  int current_stack_slot = 0;
  std::unordered_map<int, int> var_to_stack_slot;
};

#endif  // JAK_REGALLOCPROGRAM_H
