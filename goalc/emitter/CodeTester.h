#pragma once

/*!
 * @file CodeTester.h
 * The CodeTester is a utility to run the output of the compiler as part of a unit test.
 * This is effective for tests which try all combinations of registers, etc.
 *
 * The CodeTester can't be used for tests requiring the full GOAL language/linking.
 */

#ifndef JAK_CODETESTER_H
#define JAK_CODETESTER_H

#include <cstring>
#include <stdexcept>
#include <string>

#include "Instruction.h"
#include "Register.h"

#include "common/common_types.h"

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
  void emit(const Instruction& instr);
  u64 execute();
  u64 execute(u64 in0, u64 in1, u64 in2, u64 in3);

  /*!
   * Execute the function, get the return value in RAX, convert to a T, and return it.
   */
  template <typename T>
  T execute_ret(u64 in0, u64 in1, u64 in2, u64 in3) {
    u64 result_u64 = ((u64(*)(u64, u64, u64, u64))code_buffer)(in0, in1, in2, in3);
    T result_T;
    memcpy(&result_T, &result_u64, sizeof(T));
    return result_T;
  }

  /*!
   * Add data to the code buffer.
   */
  template <typename T>
  int emit_data(T x) {
    auto ret = code_buffer_size;
    ASSERT(int(sizeof(T)) + code_buffer_size <= code_buffer_capacity);
    memcpy(code_buffer + code_buffer_size, &x, sizeof(T));
    code_buffer_size += sizeof(T);
    return ret;
  }

  /*!
   * Should allow emitter tests which run code to do the right thing on windows.
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
        throw std::runtime_error("Invalid arg register index");
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
        throw std::runtime_error("Invaid arg register index");
    }
#endif
  }

  /*!
   * Get the name of the given register, for debugging.
   */
  std::string reg_name(Register x) { return m_info.get_info(x).name; }

  /*!
   * Get number of bytes currently in use (offset of the next thing to be added)
   */
  int size() const { return code_buffer_size; }
  const u8* data() const { return code_buffer; }

  /*!
   * Write over existing data at the given offset.
   */
  template <typename T>
  void write(T x, int at) {
    ASSERT(at >= 0);
    ASSERT(int(sizeof(T)) + at <= code_buffer_capacity);
    memcpy(code_buffer + at, &x, sizeof(T));
  }

  /*!
   * Read existing data at the given offset.
   */
  template <typename T>
  T read(int at) {
    ASSERT(at >= 0);
    ASSERT(int(sizeof(T)) + at <= code_buffer_capacity);
    T result;
    memcpy(&result, code_buffer + at, sizeof(T));
    return result;
  }

  void clear();
  ~CodeTester();

 private:
  int code_buffer_size = 0;
  int code_buffer_capacity = 0;
  u8* code_buffer = nullptr;
  RegisterInfo m_info;
};
}  // namespace emitter
#endif  // JAK_CODETESTER_H
