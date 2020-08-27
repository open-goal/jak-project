#ifndef JAK_X86_EMITTER_H
#define JAK_X86_EMITTER_H

#include <cstdint>
#include <stdexcept>
#include <memory>
#include <vector>
#include <unordered_map>
#include <string>
#include <array>
#include "goal/IR.h"
#include "shared_config.h"
#include "Instruction.h"
#include "CodegenOutput.h"

// Types of jumps to patch
enum StaticJumpType { SIGNED_32_RIP, INVALID_JUMP_TYPE };

// Record for a jump to be patched.
struct StaticJumpRecord {
  int instr_idx = -1;
  int target_ir_idx = -1;
  int offset_into_instr = 0;
  int function_offset = -1;
  StaticJumpType type = INVALID_JUMP_TYPE;
  bool resolved = false;
};

/*!
 * Record for generating link data for a memory access in the symbol table
 */
struct SymbolMemAccessRec {
  int offset;
  int instr_idx;

  // offset into the segment's code where the patch should be made
  int total_offset = INT32_MAX;

  // which segment the patch should be made
  int seg = -1;
};

/*!
 * Record for generating link to get the address of a static.
 */
struct StaticVarAddrRecord {
  // the object (which should know where it is eventually)
  std::shared_ptr<StaticPlace> place;

  // which instruction needs the patch
  int instr_idx;

  // how far into the instruction is the patch location
  int offset_into_instr;

  // what will rbp be at the instruction (relative to start of segment's code)
  int current_rbp_instr_idx;
  bool resolved = false;

  int size = -1;
};

/*!
 * Record for generating link to get the address of a function.
 * Very similar to StaticVarAddrRecord, but LambdaPlace isn't technically a StaticPlace
 * so it needs its own thing (this might be a sign that LambdaPlace should be a StaticPlace...)
 */
struct FuncAddrRecord {
  std::shared_ptr<LambdaPlace> place;
  int instr_idx;
  int offset_into_instr;
  int current_rbp_instr_idx;
  bool resolve = false;
};

struct Instruction;

class x86_Emitter {
 public:
  x86_Emitter() {}
  int run(FunctionEnv* func, int target_segment);
  void run(StaticObject* obj, int target_segment);
  CodegenOutput write();

 private:
  // linking
  void emit_link_table_header(CodegenOutput& out);
  void emit_link_table_data(CodegenOutput& out, std::vector<int>& instruction_offsets, int seg);
  void emit_link_table_symbol_mem_recs(CodegenOutput& out, int seg);
  void emit_link_table_type_ptrs(CodegenOutput& out, int seg);
  void emit_link_table_func_type_ptr(CodegenOutput& out, int seg);
  void emit_link_table_var_addr(CodegenOutput& out, std::vector<int>& instruction_offsets, int seg);
  void emit_link_table_func_addr(CodegenOutput& out,
                                 std::vector<int>& instruction_offsets,
                                 int seg);

  // utilities
  void emit_prologue();
  void emit_epilogue();
  void emit_static_objects(CodegenOutput& out, int segment);
  void patch_jumps_and_recs(CodegenOutput& out, std::vector<int>& offsets, int seg);

  void do_constvar(IR_LoadInteger& cv);
  void do_return(IR_Return& ret);
  void do_set(IR_Set& set);
  void do_goto_label(IR_Goto_Label& go_to);
  void do_set_symbol(IR_SetSymbolValue& set_symbol);
  void do_get_symbol(IR_GetSymbolValue& get_symbol);
  void do_get_symbol_object(IR_GetSymbolObj& get_sym);
  void do_function_call(IR_FunctionCall& fcall);
  void do_static_var_addr(IR_StaticVarAddr& var_addr);
  void do_static_var_32(IR_StaticVar32& var_addr);
  void do_function_addr(IR_FunctionAddr& func_addr);
  void do_integer_math(IR_IntegerMath& math);
  void do_cond_branch(IR_ConditionalBranch& br);
  void do_cmp_branch(IR_ConditionalBranch& br, Instruction jump_instr);
  void do_float_math(IR_FloatMath& fl);
  void do_load_const_offset(IR_LoadConstOffset& load);
  void do_store_const_offset(IR_StoreConstOffset& store);
  void do_get_ra_ptr(IR_GetReturnAddressPointer& get_ra);
  void do_asm(IR_Asm& asm_op);

  void do_float_to_int(IR_FloatToInt& f2i);
  void do_int_to_float(IR_IntToFloat& i2f);

  void load_u64_to_gpr(uint64_t value, std::shared_ptr<Place> var);

  void emit_mov_gpr64_gpr64_or_null(uint8_t dst, uint8_t src);
  void emit_mov_gpr64_gpr64_or_null(std::shared_ptr<Place> dst, std::shared_ptr<Place> src);
  void emit_mov_xmm32_xmm32_or_null(std::shared_ptr<Place> dst, std::shared_ptr<Place> src);

  void mov(ColoringAssignment dst, ColoringAssignment src);
  void push(ColoringAssignment src);
  void pop(ColoringAssignment dst);

  void link_function_type_ptr(int segment, int offset);

  void emit_instr(Instruction i) {
    instructions[current_seg].push_back(i);
    instructions[current_seg].back().ir_index = ir_idx;
  }

  uint8_t gpr_id(Place& var);
  uint8_t xmm_id(Place& var);
  ColoringAssignment get_ca(Place& var);

  // the current function being emitted
  FunctionEnv* f = nullptr;

  // the instructions per segment.  Note that one IR may expand into 0, 1, or multiple instructions
  std::array<std::vector<Instruction>, N_SEG> instructions;

  std::array<std::vector<StaticJumpRecord>, N_SEG> static_jumps;
  std::array<std::vector<uint8_t>, N_SEG> static_objects;
  std::array<std::vector<StaticVarAddrRecord>, N_SEG> static_var_addr_recs;
  std::array<std::vector<FuncAddrRecord>, N_SEG> func_addr_recs;
  int current_seg = -1;

  // map of symbol name -> list of mem access recs for each segment
  std::array<std::unordered_map<std::string, std::vector<SymbolMemAccessRec>>, N_SEG>
      symbol_mem_access_recs;

  // type pointers in statics: map of type name -> list of offsets
  std::array<std::unordered_map<std::string, std::vector<StaticLinkRecord>>, N_SEG>
      type_ptr_recs_in_statics;

  // per-segment, holds the offset into that segment's code where a function type pointer should go
  std::array<std::vector<int>, N_SEG> function_type_ptr_recs;

  // emitter state
  int ir_idx = -1;
  int current_rbp_instr_idx = -1;
  int function_offset = -1;
  int additional_stack_offset = -1;
  int stack_offset = -1;
  bool extra_push_sr0 = false;
};

#endif  // JAK_X86_EMITTER_H
