#pragma once

/*!
 * @file CodeTester.h
 * The CodeTester is a utility to run the output of the compiler as part of a unit test.
 * This is effective for tests which try all combinations of registers, etc.
 *
 * The CodeTester can't be used for tests requiring the full GOAL language/linking.
 */

#include <cstring>
#include <stdexcept>
#include <string>

#include "Instruction.h"
#include "Register.h"

#include "common/common_types.h"

#include "goalc/emitter/InstructionSet.h"
#include "goalc/emitter/ObjectGenerator.h"
#ifdef OS_POSIX
#include <sys/mman.h>
#elif _WIN32
#include "third-party/mman/mman.h"
#endif
#if defined(__APPLE__) && defined(__aarch64__)
#include <pthread.h>  // pthread_jit_write_protect_np
#endif
#if defined(__SWITCH__)
// libnx's switch/types.h does `typedef __uint128_t u128;`, which conflicts with the
// `struct u128` this codebase already declared above (common/common_types.h). Rename the
// token for the scope of this include only -- every internal libnx use of "u128" (the typedef
// itself and any struct fields of that type) becomes "nx_u128" consistently, and our own
// `u128` is untouched afterward.
#define u128 nx_u128
#include <switch.h>  // Jit: dual RW/RX aliased JIT memory
#undef u128
#endif

namespace emitter {
class CodeTester {
 private:
  int code_buffer_size = 0;
  int code_buffer_capacity = 0;
  // On most platforms this is both the writable and executable address. On Switch, libnx's Jit
  // gives separate RW/RX aliases of the same physical pages, so code_buffer is the RW one here
  // and code_buffer_rx (below) is the one actually called.
  u8* code_buffer = nullptr;
#if defined(__SWITCH__)
  Jit m_jit{};
  u8* code_buffer_rx = nullptr;
#endif
  RegisterInfo m_info;
  ObjectGenerator m_gen;

 public:
  struct DisasmLine {
    uint64_t address;
    std::string text;
  };

  CodeTester();
  CodeTester(InstructionSet instruction_set);
  std::string dump_to_hex_string(bool nospace = false);
  std::vector<DisasmLine> disassemble();
  std::string dump_to_asm_string();
  void print_hex_dump();
  void print_asm_dump();
  ObjectGenerator generator() const { return m_gen; }
  void init_code_buffer(int capacity);
  void emit_push_all_gprs(bool exclude_return_register = false);
  void emit_pop_all_gprs(bool exclude_return_register = false);
  void emit_push_all_simd();
  void emit_pop_all_simd();
  void emit_return();
  void emit(const Instruction& instr);
  u64 execute();
  u64 execute(u64 in0, u64 in1, u64 in2, u64 in3);

  /*!
   * Execute the function, get the return value in RAX, convert to a T, and return it.
   */
  template <typename T>
  T execute_ret(u64 in0, u64 in1, u64 in2, u64 in3) {
    u64 result_u64 = execute(in0, in1, in2, in3);
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

  int get_reg_count() {
    if (m_gen.instr_set() == InstructionSet::ARM64) {
      return 31;  // 32 = XZR which is a special case / zero register
    } else {
      return 16;
    }
  }

  int get_simd_reg_count() {
    if (m_gen.instr_set() == InstructionSet::ARM64) {
      return 32;
    } else {
      return -1;  // TODO
    }
  }

  Register get_stack_reg() {
    if (m_gen.instr_set() == InstructionSet::ARM64) {
      return SP;
    } else {
      return RSP;
    }
  }

  Register get_return_reg() {
    if (m_gen.instr_set() == InstructionSet::ARM64) {
      return X0;
    } else {
      return RAX;
    }
  }

  /*!
   * Should allow emitter tests which run code to do the right thing on windows.
   */
  Register get_c_abi_arg_reg(int i) {
    if (m_gen.instr_set() == InstructionSet::ARM64) {
      switch (i) {
        case 0:
          return X0;
        case 1:
          return X1;
        case 2:
          return X2;
        case 3:
          return X3;
        case 4:
          return X4;
        case 5:
          return X5;
        case 6:
          return X6;
        case 7:
          return X7;
        default:
          throw std::runtime_error("Invalid ARM64 arg register index");
      }
    }
    // x86 ABI registers differ by platform.
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

  uintptr_t code_address() const { return reinterpret_cast<uintptr_t>(code_buffer); }

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
};
}  // namespace emitter
