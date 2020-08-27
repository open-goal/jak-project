/*!
 * @file ColoringAssignment.h
 * Input and Output Types for the Coloring System
 */

#ifndef JAK_COLORINGASSIGNMENT_H
#define JAK_COLORINGASSIGNMENT_H

#include <stdexcept>
#include <cassert>
#include "codegen/x86.h"

// Assignment type which is used for constraints and output of the coloring
enum AssignmentKind { STACK, REGISTER, UNASSIGNED };

constexpr bool enable_fancy_coloring = true;
constexpr bool move_eliminator = true;

// The description of where a variable is assigned.
// Can represent a register, the stack, or UNASSIGNED.
// Uses the integer-based register IDs of X86_Registers
struct ColoringAssignment {
  ColoringAssignment() = default;
  ColoringAssignment(AssignmentKind _kind, int _reg_id) : kind(_kind), reg_id(_reg_id) {}
  AssignmentKind kind = UNASSIGNED;
  int reg_id = -1;

  // which slot of spilled variables on the stack this variable goes in.
  // Only valid if spilled is true
  int stack_slot = -1;

  // set if this variable is ever spilled.
  bool spilled = false;

  std::string print() const {
    std::string result = spilled ? "S!" : "";
    switch (kind) {
      case REGISTER:
        result += x86_gpr_names[reg_id];
        break;
      case UNASSIGNED:
        result += "unassigned";
        break;
      case STACK:
        result += "stack " + std::to_string(stack_slot);
        break;
      default:
        throw std::runtime_error("can't print this coloring assignment");
    }
    return result;
  }

  /*!
   * Will these two assignments use the same hardware register?
   * If unassigned or on the stack, always no.
   */
  bool occupies_same_reg(const ColoringAssignment& other) const {
    return other.reg_id == reg_id && (reg_id != -1);
  }

  /*!
   * Are these exactly identical? (not including stack settings)
   */
  bool operator==(const ColoringAssignment& other) const {
    return (other.kind == kind) && (other.reg_id == reg_id);
  }

  /*!
   * Has this assignment been set?
   */
  bool is_assigned() const { return kind != UNASSIGNED; }
};

// A constraint on a specific variable at a specific instruction
struct RegConstraint {
  int var_id;              // the variable
  int instr_id;            // the instruction
  ColoringAssignment ass;  // the assignment of the variable at the instruction
};

// An input to the coloring to tell the system what type of reg it can get.
enum RegisterKind {
  REG_GPR,
  REG_XMM_FLOAT,
  UNASSIGNED_REG  // a default, which is invalid.  This will error if passed to coloring algorithms
};

// The input to the coloring system for a variable.
struct ColoringInput {
  int id = -1;                         // variable id
  RegisterKind kind = UNASSIGNED_REG;  // what type of register it must go in

  std::string print() {
    std::string result;
    switch (kind) {
      case REG_GPR:
        result += "gpr ";
        break;
      case REG_XMM_FLOAT:
        result += "xmm ";
        break;
      default:
        throw std::runtime_error("unknown register kind in ColoringInput");
    }

    result += std::to_string(id);
    return result;
  }
};

// Indication of where a variable is live and what assignment it has at each point in the range.
struct LiveRange {
 public:
  LiveRange(int start, int end) : min(start), max(end) {}
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
  std::vector<ColoringAssignment> assignment;

  // a hint on where to put this variable.
  ColoringAssignment best_hint;

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
  bool is_live_at_instr(int value) {
    if (value >= min && value <= max) {
      if (enable_fancy_coloring) {
        return is_alive.at(value - min);
      } else {
        return true;
      }
    }
    return false;
  }

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
  void constrain_at_one(int id, ColoringAssignment ass) {
    assert(id >= min && id <= max);
    assignment.at(id - min) = ass;
    has_constraint = true;
    best_hint = ass;
  }

  /*!
   * At the given instruction, does the given assignment conflict with this one?
   */
  bool conflicts_at(int id, ColoringAssignment ass) {
    assert(id >= min && id <= max);
    return assignment.at(id - min).occupies_same_reg(ass);
  }

  /*!
   * Assign variable to the given assignment at all instructions
   * Throws if this would require modifying a currently set assignment.
   */
  void assign_no_overwrite(ColoringAssignment ass) {
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
  const ColoringAssignment& get(int id) {
    assert(id >= min && id <= max);
    return assignment.at(id - min);
  }

  std::string print() {
    std::string result = "Live Range for var " + std::to_string(var) + "\n";
    for (uint32_t i = 0; i < assignment.size(); i++) {
      result += "instr " + std::to_string(i + min) + ":" + assignment.at(i).print() + "\n";
    }
    return result;
  }
};

// An extra instruction to load/store variables from the stack
struct BonusOp {
  int stack_slot = -1;            // stack slot to load/store into
  ColoringAssignment ass;         // register to load/store into
  bool load_from_stack = false;   // load from stack into register
  bool store_into_stack = false;  // store into stack from register

  std::string print() const {
    if (!load_from_stack && !store_into_stack)
      return "";
    std::string result = "";
    if (load_from_stack) {
      result += "load-from-stack ";
    }
    if (store_into_stack) {
      result += "store-into-stack ";
    }
    result += std::to_string(stack_slot);
    return result + ass.print();
  }
};

// A list of bonus operations to go with an Instruction
struct RegAllocBonusInstruction {
  std::vector<BonusOp> ops;

  void clear() { ops.clear(); }

  std::string print() const {
    std::string result;
    for (auto& op : ops) {
      result += op.print() + " ";
    }
    return result;
  }
};

#endif  // JAK_COLORINGASSIGNMENT_H
