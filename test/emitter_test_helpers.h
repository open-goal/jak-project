#include "emitter_util.h"

template <typename T>
bool execute_ret_equals(emitter::CodeTester& tester, u64 val, T expected, T& actual) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    actual = tester.execute_ret<T>(val, 0, 0, 0);
    return actual == expected;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    actual = tester.execute_ret<T>(val, 0, 0, 0);
    return actual == expected;
#endif
  }
  return true;  // not testing this architecture
}

bool execute_equals(emitter::CodeTester& tester, u64 expected, u64& actual) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    actual = tester.execute();
    return actual == expected;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    actual = tester.execute();
    return actual == expected;
#endif
  }
  return true;  // not testing this architecture
}

bool execute_equals_4arg(emitter::CodeTester& tester,
                         u64 in0,
                         u64 in1,
                         u64 in2,
                         u64 in3,
                         u64 expected,
                         u64& actual) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    actual = tester.execute(in0, in1, in2, in3);
    return actual == expected;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    actual = tester.execute(in0, in1, in2, in3);
    return actual == expected;
#endif
  }
  return true;
}

bool execute_equals_4args_no_cmp(emitter::CodeTester& tester, u64 in0, u64 in1, u64 in2, u64 in3) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    tester.execute(in0, in1, in2, in3);
    return true;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    tester.execute(in0, in1, in2, in3);
    return true;
#endif
  }
  return true;  // not testing this architecture
}

template <typename T>
bool execute_ret_equals_4arg(emitter::CodeTester& tester,
                             u64 in0,
                             u64 in1,
                             u64 in2,
                             u64 in3,
                             T expected,
                             T& actual) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    actual = tester.execute_ret<T>(in0, in1, in2, in3);
    return actual == expected;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    actual = tester.execute_ret<T>(in0, in1, in2, in3);
    return actual == expected;
#endif
  }
  return true;
}

template <typename T>
bool execute_ret_float_equals_4arg(emitter::CodeTester& tester,
                                   u64 in0,
                                   u64 in1,
                                   u64 in2,
                                   u64 in3,
                                   T expected,
                                   T& actual) {
  if (tester.generator().instr_set() == emitter::InstructionSet::ARM64) {
#ifdef __aarch64__
    actual = tester.execute_ret<float>(in0, in1, in2, in3);
    return actual == expected;
#endif
  } else if (tester.generator().instr_set() == emitter::InstructionSet::X86) {
#ifndef __aarch64__
    actual = tester.execute_ret<float>(in0, in1, in2, in3);
    return actual == expected;
#endif
  }

  return true;
}

// clang-format off
#define EXPECT_EXECUTE_RET_EQ(tester, val, expected) \
  EXPECT_EXECUTE_RET_EQ_IMPL(tester, val, expected, "")

#define EXPECT_EXECUTE_RET_EQ_MSG(tester, val, expected, msg) \
  EXPECT_EXECUTE_RET_EQ_IMPL(tester, val, expected, msg)

#define EXPECT_EXECUTE_RET_EQ_IMPL(tester, val, expected, msg)                       \
  do {                                                                           \
    decltype(expected) actual{};                                                  \
    if (!execute_ret_equals(tester, val, expected, actual)) {                     \
      FAIL()                                                                      \
          << "\033[1;31mExecute mismatch\033[0m"                                  \
          << "\n  \033[33minput:    \033[0m" << val                               \
          << "\n  \033[32mexpected: \033[0m" << expected                          \
          << "\n  \033[31mactual:   \033[0m" << actual                            \
          << "\n  \033[36mcontext:  \033[0m" << msg                               \
          << "\n\033[1;36mGenerated code:\033[0m\n"                               \
          << tester.dump_to_asm_string()                             \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                               \
          << tester.dump_to_hex_string(true) << "\n";                             \
    }                                                                              \
  } while (0)

#define EXPECT_EXECUTE_EQ(tester, expected) \
  EXPECT_EXECUTE_EQ_IMPL(tester, expected, "")

#define EXPECT_EXECUTE_EQ_MSG(tester, expected, msg) \
  EXPECT_EXECUTE_EQ_IMPL(tester, expected, msg)

#define EXPECT_EXECUTE_EQ_IMPL(tester, expected, msg)                       \
  do {                                                                           \
    decltype(expected) actual{};                                                  \
    if (!execute_equals(tester, expected, actual)) {                     \
      FAIL()                                                                      \
          << "\033[1;31mExecute mismatch\033[0m"                                  \
          << "\n  \033[32mexpected: \033[0m" << expected                          \
          << "\n  \033[31mactual:   \033[0m" << actual                            \
          << "\n  \033[36mcontext:  \033[0m" << msg                               \
          << "\n\033[1;36mGenerated code:\033[0m\n"                               \
          << tester.dump_to_asm_string()                             \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                               \
          << tester.dump_to_hex_string(true) << "\n";                             \
    }                                                                              \
  } while (0)

#define EXPECT_EXECUTE_4ARG_NO_CMP(tester, val1, val2, val3, val4) \
  EXPECT_EXECUTE_4ARG_NO_CMP_IMPL(tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, "")

#define EXPECT_EXECUTE_4ARG_NO_CMP_MSG(tester, val1, val2, val3, val4, msg) \
  EXPECT_EXECUTE_4ARG_NO_CMP_IMPL(tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, msg)

#define EXPECT_EXECUTE_4ARG_NO_CMP_IMPL(tester, val1, val2, val3, val4, msg)                       \
  do {                                                                           \
    if (!execute_equals_4args_no_cmp(tester, val1, val2, val3, val4)) {                     \
      FAIL()                                                                      \
          << "\033[1;31mExecute mismatch\033[0m"                                  \
          << "\n  \033[33minput:    \033[0m" << val1 << ", " << val2 << ", " << val3 << ", " << val4                               \
          << "\n  \033[36mcontext:  \033[0m" << msg                               \
          << "\n\033[1;36mGenerated code:\033[0m\n"                               \
          << tester.dump_to_asm_string()                             \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                               \
          << tester.dump_to_hex_string(true) << "\n";                             \
    }                                                                              \
  } while (0)

#define EXPECT_EXECUTE_4ARG_EQ(tester, val1, val2, val3, val4,expected) \
  EXPECT_EXECUTE_4ARG_EQ_IMPL(tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, (u64)expected, "")

#define EXPECT_EXECUTE_4ARG_EQ_MSG(tester, val1, val2, val3, val4,expected, msg) \
  EXPECT_EXECUTE_4ARG_EQ_IMPL(tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, (u64)expected, msg)

#define EXPECT_EXECUTE_4ARG_EQ_IMPL(tester, val1, val2, val3, val4, expected, msg)                       \
  do {                                                                           \
    decltype(expected) actual{};                                                  \
    if (!execute_equals_4arg(tester, val1, val2, val3, val4, expected, actual)) {                     \
      FAIL()                                                                      \
          << "\033[1;31mExecute mismatch\033[0m"                                  \
          << "\n  \033[33minput:    \033[0m" << val1 << ", " << val2 << ", " << val3 << ", " << val4                               \
          << "\n  \033[32mexpected: \033[0m" << expected                          \
          << "\n  \033[31mactual:   \033[0m" << actual                            \
          << "\n  \033[36mcontext:  \033[0m" << msg                               \
          << "\n\033[1;36mGenerated code:\033[0m\n"                               \
          << tester.dump_to_asm_string()                             \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                               \
          << tester.dump_to_hex_string(true) << "\n";                             \
    }                                                                              \
  } while (0)

#define EXPECT_EXECUTE_RET_4ARG_EQ(tester, val1, val2, val3, val4, expected) \
  EXPECT_EXECUTE_RET_4ARG_EQ_IMPL(                                           \
      tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, expected, "")

#define EXPECT_EXECUTE_RET_4ARG_EQ_MSG(tester, val1, val2, val3, val4, expected, msg) \
  EXPECT_EXECUTE_RET_4ARG_EQ_IMPL(                                                       \
      tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, expected, msg)

#define EXPECT_EXECUTE_RET_4ARG_EQ_IMPL(tester, val1, val2, val3, val4, expected, msg) \
  do {                                                                                    \
    decltype(expected) actual{};                                                          \
    if (!execute_ret_equals_4arg(                                                         \
            tester, val1, val2, val3, val4, expected, actual)) {                          \
      FAIL()                                                                              \
          << "\033[1;31mExecute mismatch\033[0m"                                         \
          << "\n  \033[33minput:    \033[0m" << val1 << ", " << val2 << ", "            \
          << val3 << ", " << val4                                                         \
          << "\n  \033[32mexpected: \033[0m" << expected                                 \
          << "\n  \033[31mactual:   \033[0m" << actual                                   \
          << "\n  \033[36mcontext:  \033[0m" << msg                                      \
          << "\n\033[1;36mGenerated code:\033[0m\n"                                      \
          << tester.dump_to_asm_string()                                                  \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                               \
          << tester.dump_to_hex_string(true) << "\n";                                    \
    }                                                                                     \
  } while (0)

#define EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ(tester, val1, val2, val3, val4, expected) \
  EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ_IMPL(                                         \
      tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, expected, "")

#define EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ_MSG(                                     \
    tester, val1, val2, val3, val4, expected, msg)                                \
  EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ_IMPL(                                         \
      tester, (u64)val1, (u64)val2, (u64)val3, (u64)val4, expected, msg)

#define EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ_IMPL(                                    \
    tester, val1, val2, val3, val4, expected, msg)                                \
  do {                                                                            \
    float actual{};                                                               \
    if (!execute_ret_float_equals_4arg(                                           \
            tester, val1, val2, val3, val4, expected, actual)) {                  \
      FAIL()                                                                      \
          << "\033[1;31mExecute mismatch\033[0m"                                  \
          << "\n  \033[33minput:    \033[0m" << val1 << ", " << val2 << ", "       \
          << val3 << ", " << val4                                                 \
          << "\n  \033[32mexpected: \033[0m" << expected                          \
          << "\n  \033[31mactual:   \033[0m" << actual                            \
          << "\n  \033[36mcontext:  \033[0m" << msg                               \
          << "\n\033[1;36mGenerated code:\033[0m\n"                               \
          << tester.dump_to_asm_string()                                          \
          << "\n\033[1;36mInstruction encoding:\033[0m\n"                        \
          << tester.dump_to_hex_string(true) << "\n";                             \
    }                                                                             \
  } while (0)

#ifdef __aarch64__
#define EXPECT_EXECUTE_IF_NATIVE_ARM64(body) body
#else
#define EXPECT_EXECUTE_IF_NATIVE_ARM64(body)
#endif

#ifndef __aarch64__
#define EXPECT_EXECUTE_IF_NATIVE_X86(body) body
#else
#define EXPECT_EXECUTE_IF_NATIVE_X86(body)
#endif

#define EXPECT_EXECUTE_IF_NATIVE(tester, body)             \
  do {                                                     \
    if ((tester).generator().instr_set() ==                \
        emitter::InstructionSet::ARM64) {                  \
      EXPECT_EXECUTE_IF_NATIVE_ARM64(body)                \
    } else if ((tester).generator().instr_set() ==         \
               emitter::InstructionSet::X86) {             \
      EXPECT_EXECUTE_IF_NATIVE_X86(body)                  \
    }                                                        \
  } while (false)
// clang-format on
