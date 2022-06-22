#pragma once

#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "IRegSet.h"
#include "IRegister.h"
#include "allocator_interface.h"

// with this on, gaps in usage of registers allow other variables to steal registers.
// this reduces stack spills/moves, but may make register allocation slower.
constexpr bool enable_fancy_coloring = true;

// will attempt to allocate in a way to reduce the number of moves.
constexpr bool move_eliminator = true;

constexpr bool allow_read_write_same_reg = true;

// Indication of where a variable is live and what assignment it has at each point in the range.
struct LiveInfo {
 public:
  LiveInfo(int start, int end) : min(start), max(end) {}
  // min, max are inclusive.
  // meaning the variable written for the first time at min, and read for the last time at max.
  int min, max;

  std::vector<bool> is_alive;
  std::vector<int> indices_of_alive;

  // which variable is this?
  int var = -1;

  // have we actually seen this variable in the code?
  bool seen = false;

  // does this variable have a constraint?
  bool has_constraint = false;

  // the assignment of this variable at each instruction in [min, max]
  std::vector<Assignment> assignment;

  // a hint on where to put this variable.
  Assignment best_hint;

  /*!
   * Add an instruction id where this variable is live.
   */
  void add_live_instruction(int value) {
    ASSERT(value >= 0);
    if (value > max)
      max = value;
    if (value < min)
      min = value;
    indices_of_alive.push_back(value);
    // remember that this variable is actually used
    seen = true;
  }

  /*!
   * Is the given instruction contained in the live range?
   */
  bool is_live_at_instr(int value) const {
    if (value >= min && value <= max) {
      if (enable_fancy_coloring) {
        return is_alive.at(value - min);
      } else {
        return true;
      }
    }
    return false;
  }

  /*!
   * Are we alive at idx, but not alive at idx - 1 (or idx - 1 doesn't exist)
   */
  bool becomes_live_at_instr(int idx) {
    if (enable_fancy_coloring) {
      if (idx == min)
        return true;
      if (idx < min || idx > max)
        return false;
      ASSERT(idx > min);
      return is_alive.at(idx - min) && !is_alive.at(idx - min - 1);
    } else {
      return idx == min;
    }
  }

  /*!
   * Are we alive at idx, but not alive at idx + 1 (or idx + 1 doesn't exist)
   */
  bool dies_next_at_instr(int idx) {
    if (enable_fancy_coloring) {
      if (idx == max)
        return true;
      if (idx < min || idx > max)
        return false;
      ASSERT(idx < max);
      return is_alive.at(idx - min) && !is_alive.at(idx - min + 1);
    } else {
      return idx == max;
    }
  }

  /*!
   * Resize Live Range after instructions have been added.  Do this before assigning.
   */
  void prepare_for_allocation(int id) {
    var = id;
    if (!seen)
      return;  // don't do any prep for a variable which isn't used.
    ASSERT(max - min >= 0);
    assignment.resize(max - min + 1);
    is_alive.resize(max - min + 1);
    for (auto& x : indices_of_alive) {
      is_alive.at(x - min) = true;
    }
  }

  /*!
   * Lock an assignment at a given instruction.
   * Will overwrite any previous assignment here
   * Will set best_hint to this assignment.
   */
  void constrain_at_one(int id, emitter::Register reg) {
    ASSERT(id >= min && id <= max);
    Assignment ass;
    ass.reg = reg;
    ass.kind = Assignment::Kind::REGISTER;
    assignment.at(id - min) = ass;
    has_constraint = true;
    best_hint = ass;
  }

  /*!
   * Lock an assignment everywhere.
   */
  void constrain_everywhere(emitter::Register reg) {
    Assignment ass;
    ass.reg = reg;
    ass.kind = Assignment::Kind::REGISTER;
    for (int i = min; i <= max; i++) {
      assignment.at(i - min) = ass;
    }
    has_constraint = true;
    best_hint = ass;
  }

  /*!
   * At the given instruction, does the given assignment conflict with this one?
   */
  bool conflicts_at(int id, Assignment ass) {
    ASSERT(id >= min && id <= max);
    return assignment.at(id - min).occupies_same_reg(ass);
  }

  /*!
   * At the given instruction, does the given assignment conflict with this one?
   */
  bool conflicts_at(int id, emitter::Register reg) {
    ASSERT(id >= min && id <= max);
    Assignment ass;
    ass.reg = reg;
    ass.kind = Assignment::Kind::REGISTER;
    return assignment.at(id - min).occupies_same_reg(ass);
  }

  /*!
   * Assign variable to the given assignment at all instructions
   * Throws if this would require modifying a currently set assignment.
   */
  void assign_no_overwrite(Assignment ass) {
    ASSERT(seen);
    ASSERT(ass.is_assigned());
    for (int i = min; i <= max; i++) {
      auto& a = assignment.at(i - min);
      if (a.is_assigned() && !(a.occupies_same_reg(ass))) {
        throw std::runtime_error("assign_no_overwrite failed!");
      } else {
        a = ass;
      }
    }
  }

  /*!
   * Get the assignment at the given instruction.
   */
  const Assignment& get(int id) const {
    ASSERT(id >= min && id <= max);
    return assignment.at(id - min);
  }

  int size() const { return 1 + max - min; }

  bool overlaps(const LiveInfo& other) const {
    auto overlap_min = std::max(min, other.min);
    auto overlap_max = std::min(max, other.max);
    return overlap_min <= overlap_max;
  }

  std::string print_assignment();
};

struct RegAllocCache {
  ControlFlowAnalysisCache control_flow;
  std::vector<LiveInfo> live_ranges;
  int max_var = -1;
  std::vector<bool> was_colored;
  std::vector<IRegister> iregs;
  std::vector<StackOp> stack_ops;
  std::unordered_map<int, int> var_to_stack_slot;
  int current_stack_slot = 0;
  bool used_stack = false;
  bool is_asm_func = false;

  struct Stats {
    int num_spill_ops = 0;
  } stats;

  std::vector<std::vector<int>> live_ranges_by_instr;
};

void find_basic_blocks(ControlFlowAnalysisCache* cache, const AllocationInput& in);
void analyze_liveliness(RegAllocCache* cache, const AllocationInput& in);
void do_constrained_alloc(RegAllocCache* cache, const AllocationInput& in, bool trace_debug);
bool check_constrained_alloc(RegAllocCache* cache, const AllocationInput& in);
bool run_allocator(RegAllocCache* cache, const AllocationInput& in, int debug_trace);
AllocationResult allocate_registers(const AllocationInput& input);