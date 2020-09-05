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
  std::vector<emitter::Register> clobber;
  std::vector<emitter::Register> exclude;
  std::vector<IRegister> write;
  std::vector<IRegister> read;
  std::vector<int> jumps;
  bool fallthrough = true;
  bool is_move = false;
  std::string print() const;
};


struct AllocationResult {
  bool ok = false;
};

struct AllocationInput {
  std::vector<RegAllocInstr> instructions;
  std::vector<IRegConstraint> constraints;
  bool requires_aligned_stack = false;
  int max_vars = -1;

  std::vector<std::string> debug_instruction_names; // optional



  int add_instruction(const RegAllocInstr& instr) {
    instructions.push_back(instr);
    return int(instructions.size()) - 1;
  }

  struct {
    bool print_input = false;
    bool print_analysis = false;
  } debug_settings;
};

AllocationResult allocate_registers(const AllocationInput& input);

#endif  // JAK_ALLOCATE_H
