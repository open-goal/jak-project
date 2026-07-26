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

// clang-format off
#define EXPECT_EXECUTE_EQ(tester, val, expected)                                  \
  do {                                                                             \
    decltype(expected) actual{};                                                    \
    if (!execute_ret_equals(tester, val, expected, actual)) {             \
      FAIL()                                                                       \
          << "\033[1;31mExecute mismatch\033[0m"                                   \
          << "\n  \033[33minput:    \033[0m" << val                                \
          << "\n  \033[32mexpected: \033[0m" << expected                           \
          << "\n  \033[31mactual:   \033[0m" << actual                            \
          << "\n\033[1;36mGenerated code:\033[0m\n"                             \
          << tester.dump_to_hex_string(true) << "\n";                                          \
    }                                                                              \
  } while (0)
// clang-format on