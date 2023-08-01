#pragma once

/*!
 * @file allocate.h
 * Interface for the register allocator.
 *
 * The IR is translated to RegAllocInstrs, which are added to a RegAllocFunc.
 * These, plus any additional info are put in a AllocationInput, which is processed by the
 * allocate_registers algorithm.
 */

#include <unordered_set>
#include <vector>

#include "goalc/emitter/Register.h"
#include "goalc/regalloc/IRegSet.h"
#include "goalc/regalloc/IRegister.h"

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
 * An operation that's added to an Instruction so that it loads/stores things from the stack if
 * needed for spilling.
 */
struct StackOp {
  struct Op {
    int slot = -1;
    emitter::Register reg;
    RegClass reg_class = RegClass::INVALID;
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
  int stack_slot = -1;  //! index of the slot, if we are ever spilled
  enum class Kind : u8 { STACK, REGISTER, UNASSIGNED } kind = Kind::UNASSIGNED;
  emitter::Register reg = -1;  //! where the IRegister is now
  bool spilled = false;        //! are we ever spilled

  std::string to_string() const;

  bool occupies_same_reg(const Assignment& other) const { return other.reg == reg && (reg != -1); }

  bool occupies_reg(emitter::Register other_reg) const { return reg == other_reg && (reg != -1); }

  bool is_assigned() const { return kind != Kind::UNASSIGNED; }
};

class AssignmentRange {
 public:
  AssignmentRange(int start_instr,
                  const std::vector<bool>& live,
                  const std::vector<Assignment>& assignments)
      : m_start(start_instr), m_live(live), m_ass(assignments) {
    m_end = start_instr + live.size() - 1;
    ASSERT(m_live.size() == m_ass.size());
  }
  bool is_live_at_instr(int instr) const {
    return has_info_at(instr) && m_live.at(instr - m_start);
  }
  const Assignment& get(int instr) const { return m_ass.at(instr - m_start); }
  bool has_info_at(int instr) const { return instr >= m_start && instr <= m_end; }
  int stack_slot() const { return m_ass.at(0).stack_slot; }

 private:
  int m_start = -1;
  int m_end = -1;  // INCLUSIVE!
  std::vector<bool> m_live;
  std::vector<Assignment> m_ass;
};

/*!
 * Result of the allocate_registers algorithm
 */
struct AllocationResult {
  bool ok = false;  // did it work?
  // std::vector<std::vector<Assignment>> assignment;  // variable, instruction
  std::vector<AssignmentRange> ass_as_ranges;      // another format, maybe easier?
  std::vector<emitter::Register> used_saved_regs;  // which saved regs get clobbered?
  int stack_slots_for_spills = 0;                  // how many space on the stack do we need?
  int stack_slots_for_vars = 0;
  std::vector<StackOp> stack_ops;  // additional instructions to spill/restore
  bool needs_aligned_stack_for_spills = false;

  int num_spills = 0;
  int num_spilled_vars = 0;

  // we put the variables before the spills so the variables are 16-byte aligned.

  int total_stack_slots() const { return stack_slots_for_spills + stack_slots_for_vars; }

  int get_slot_for_var(int slot) const {
    ASSERT(slot < stack_slots_for_vars);
    return slot;
  }

  int get_slot_for_spill(int slot) const {
    ASSERT(slot < stack_slots_for_spills);
    return slot + stack_slots_for_vars;
  }
};

/*!
 * Input to the allocate_registers algorithm
 */
struct AllocationInput {
  std::vector<RegAllocInstr> instructions;           // all instructions in the function
  std::vector<IRegConstraint> constraints;           // all register constraints
  std::unordered_set<int> force_on_stack_regs;       // registers which must be on the stack
  int max_vars = -1;                                 // maximum register id.
  std::vector<std::string> debug_instruction_names;  // optional, for debug prints
  int stack_slots_for_stack_vars = 0;
  bool is_asm_function = false;

  std::string function_name;

  int allocator_version = 1;

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

struct ControlFlowAnalysisCache {
  std::vector<RegAllocBasicBlock> basic_blocks;
};

void print_allocate_input(const AllocationInput& in);
void print_result(const AllocationInput& in, const AllocationResult& result);
void find_basic_blocks(ControlFlowAnalysisCache* cache, const AllocationInput& in);