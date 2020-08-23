/*!
 * @file CodeTester
 * CodeTester is a utility which allows small segments of x86 code to be run, for the purpose of
 * testing the compiler's code emitter.  It is not suitable for testing compiled GOAL code.
 */

#ifndef JAK1_CODETESTER_H
#define JAK1_CODETESTER_H

#include <string>
#include "common/common_types.h"
#include "registers.h"
#include "Instruction.h"

namespace goal {
class CodeTester {
 public:
  std::string dump_to_hex_string();
  void init_code_buffer(int capacity);
  void emit_push_all_gprs(bool exclude_rax = false);
  void emit_pop_all_gprs(bool exclude_rax = false);
  void emit_return();
  void emit_set_gpr_as_return(X86R gpr);
  void emit(const Instruction& instr);
  u64 execute();
  void clear();
  ~CodeTester();
 private:


  int code_buffer_size = 0;
  int code_buffer_capacity = 0;
  u8* code_buffer = nullptr;
};
}  // namespace goal

#endif  // JAK1_CODETESTER_H
