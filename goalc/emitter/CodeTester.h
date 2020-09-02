#ifndef JAK_CODETESTER_H
#define JAK_CODETESTER_H

#include <string>
#include "common/common_types.h"
#include "Register.h"
#include "Instruction.h"

namespace emitter {
class CodeTester {
 public:
  CodeTester();
  std::string dump_to_hex_string(bool nospace = false);
  void init_code_buffer(int capacity);
  void emit_push_all_gprs(bool exclude_rax = false);
  void emit_pop_all_gprs(bool exclude_rax = false);
  void emit_push_all_xmms();
  void emit_pop_all_xmms();
  void emit_return();
  void emit_set_gpr_as_return(Register gpr);
  void emit(const Instruction& instr);
  u64 execute();
  u64 execute(u64 in0, u64 in1, u64 in2, u64 in3);

  template <typename T>
  T execute_ret(u64 in0, u64 in1, u64 in2, u64 in3) {
    u64 result_u64 = ((u64(*)(u64, u64, u64, u64))code_buffer)(in0, in1, in2, in3);
    T result_T;
    memcpy(&result_T, &result_u64, sizeof(T));
    return result_T;
  }

  template <typename T>
  int emit_data(T x) {
    auto ret = code_buffer_size;
    assert(int(sizeof(T)) + code_buffer_size <= code_buffer_capacity);
    memcpy(code_buffer + code_buffer_size, &x, sizeof(T));
    code_buffer_size += sizeof(T);
    return ret;
  }

  void clear();
  ~CodeTester();

  /*!
   * Should allow emitter tests which run code to do the right thing on windows.
   * Assumes RAX is return and RSP is stack pointer.
   */
  Register get_c_abi_arg_reg(int i) {
#ifdef _WIN32
    switch (i) {
      case 0:
        return RCX;
      case 1:
        return RDX;
      case 2:
        return R8;
      case 3:
        return R9;
      default:
        assert(false);
    }
#else
    switch (i) {
      case 0:
        return RDI;
      case 1:
        return RSI;
      case 2:
        return RDX;
      case 3:
        return RCX;
      default:
        assert(false);
    }
#endif
  }

  std::string reg_name(Register x) { return m_info.get_info(x).name; }

  int size() const { return code_buffer_size; }

  template <typename T>
  void write(T x, int at) {
    assert(at >= 0);
    assert(int(sizeof(T)) + at <= code_buffer_capacity);
    memcpy(code_buffer + at, &x, sizeof(T));
  }

  template <typename T>
  T read(int at) {
    assert(at >= 0);
    assert(int(sizeof(T)) + at <= code_buffer_capacity);
    T result;
    memcpy(&result, code_buffer + at, sizeof(T));
    return result;
  }

  const u8* data() const { return code_buffer; }

 private:
  int code_buffer_size = 0;
  int code_buffer_capacity = 0;
  u8* code_buffer = nullptr;
  RegisterInfo m_info;
};
}  // namespace emitter
#endif  // JAK_CODETESTER_H
