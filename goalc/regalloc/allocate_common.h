#ifndef JAK_ALLOCATE_COMMON_H
#define JAK_ALLOCATE_COMMON_H
#include <vector>
#include <stdexcept>
#include "goalc/emitter/Register.h"

/*!
 * An operation that's added to an Instruction so that it loads/stores things from the stack if
 * needed for spilling.
 */
struct StackOp {
  struct Op {
    int slot = -1;
    emitter::Register reg;
    bool load = false;   // load from reg before instruction?
    bool store = false;  // store into reg after instruction?
  };

  std::vector<Op> ops;

  std::string print() const;
};

/*!
 * The assignment of an IRegister to a real Register.
 * For a single IR Instruction.
 */
struct Assignment {
  enum class Kind { STACK, REGISTER, UNASSIGNED } kind = Kind::UNASSIGNED;
  emitter::Register reg = -1;  //! where the IRegister is now
  int stack_slot = -1;         //! index of the slot, if we are ever spilled
  bool spilled = false;        //! are we ever spilled

  std::string to_string() const;

  bool occupies_same_reg(const Assignment& other) const { return other.reg == reg && (reg != -1); }

  bool occupies_reg(emitter::Register other_reg) const { return reg == other_reg && (reg != -1); }

  bool is_assigned() const { return kind != Kind::UNASSIGNED; }
};

// with this on, gaps in usage of registers allow other variables to steal registers.
// this reduces stack spills/moves, but may make register allocation slower.
constexpr bool enable_fancy_coloring = true;

// will attempt to allocate in a way to reduce the number of moves.
constexpr bool move_eliminator = true;

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
      assert(idx > min);
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
      assert(idx < max);
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
    assert(max - min >= 0);
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
    assert(id >= min && id <= max);
    Assignment ass;
    ass.reg = reg;
    ass.kind = Assignment::Kind::REGISTER;
    assignment.at(id - min) = ass;
    has_constraint = true;
    best_hint = ass;
  }

  /*!
   * At the given instruction, does the given assignment conflict with this one?
   */
  bool conflicts_at(int id, Assignment ass) {
    assert(id >= min && id <= max);
    return assignment.at(id - min).occupies_same_reg(ass);
  }

  /*!
   * At the given instruction, does the given assignment conflict with this one?
   */
  bool conflicts_at(int id, emitter::Register reg) {
    assert(id >= min && id <= max);
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
    assert(seen);
    assert(ass.is_assigned());
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
    assert(id >= min && id <= max);
    return assignment.at(id - min);
  }

  std::string print_assignment();
};
#endif  // JAK_ALLOCATE_COMMON_H
