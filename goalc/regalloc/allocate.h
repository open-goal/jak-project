#pragma once

/*!
 * @file allocate.h
 * Interface for the register allocator.
 *
 * The IR is translated to RegAllocInstrs, which are added to a RegAllocFunc.
 * These, plus any additional info are put in a AllocationInput, which is processed by the
 * allocate_registers algorithm.
 */

#ifndef JAK_ALLOCATE_H
#define JAK_ALLOCATE_H

#include <vector>
#include "goalc/emitter/Register.h"
#include "IRegister.h"
#include "allocate_common.h"

/*!
 * Information about an instruction needed for register allocation.
 * The model is this:
 *   instruction reads all read registers
 *   instruction writes junk into all clobber registers
 *   instruction writes all write registers
 *
 * The "exclude" registers cannot be used at any time during this instruction, for any reason.
 * Possibly because the actual implementation requires using it.
 */
struct RegAllocInstr {
  std::vector<emitter::Register> clobber;  // written, but safe to use as input/output
  std::vector<emitter::Register> exclude;  // written, unsafe to use for input/output
  std::vector<IRegister> write;            // results go in here
  std::vector<IRegister> read;             // inputs go in here
  std::vector<int> jumps;                  // RegAllocInstr indexes of possible jumps
  bool fallthrough = true;                 // can it fall through to the next instruction
  bool is_move = false;                    // is this a move?
  std::string print() const;

  /*!
   * Does this read IReg id?
   */
  bool reads(int id) const {
    for (const auto& x : read) {
      if (x.id == id)
        return true;
    }
    return false;
  }

  /*!
   * Does this write IReg id?
   */
  bool writes(int id) const {
    for (const auto& x : write) {
      if (x.id == id)
        return true;
    }
    return false;
  }
};

/*!
 * Result of the allocate_registers algorithm
 */
struct AllocationResult {
  bool ok = false;                                  // did it work?
  std::vector<std::vector<Assignment>> assignment;  // variable, instruction
  std::vector<LiveInfo> ass_as_ranges;              // another format, maybe easier?
  std::vector<emitter::Register> used_saved_regs;   // which saved regs get clobbered?
  int stack_slots = 0;                              // how many space on the stack do we need?
  std::vector<StackOp> stack_ops;                   // additional instructions to spill/restore
  bool needs_aligned_stack_for_spills = false;
};

/*!
 * Input to the allocate_registers algorithm
 */
struct AllocationInput {
  std::vector<RegAllocInstr> instructions;           // all instructions in the function
  std::vector<IRegConstraint> constraints;           // all register constraints
  int max_vars = -1;                                 // maximum register id.
  std::vector<std::string> debug_instruction_names;  // optional, for debug prints

  struct {
    bool print_input = false;
    bool print_analysis = false;
    bool trace_debug_constraints = false;
    int allocate_log_level = 0;
    bool print_result = false;
  } debug_settings;

  /*!
   * Add instruction and return its idx.
   */
  int add_instruction(const RegAllocInstr& instr) {
    instructions.push_back(instr);
    return int(instructions.size()) - 1;
  }
};

AllocationResult allocate_registers(const AllocationInput& input);

#endif  // JAK_ALLOCATE_H
