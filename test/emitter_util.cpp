#include "emitter_util.h"

#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"

namespace emitter {

void execute_ret_tester(CodeTester& tester, u64 val, s64 expected_return) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    auto result = tester.execute_ret<s64>(val, 0, 0, 0);
    EXPECT_EQ(result, expected_return);
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    auto result = tester.execute_ret<s64>(val, 0, 0, 0);
    EXPECT_EQ(result, expected_return);
#endif
  }
}

void execute_tester(CodeTester& tester, u64 expected_return) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    auto result = tester.execute();
    EXPECT_EQ(result, expected_return);
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    auto result = tester.execute();
    EXPECT_EQ(result, expected_return);
#endif
  }
}

void execute_tester(CodeTester& tester, u64 in0, u64 in1, u64 in2, u64 in3, u64 expected_return) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    auto result = tester.execute(in0, in1, in2, in3);
    EXPECT_EQ(result, expected_return);
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    auto result = tester.execute(in0, in1, in2, in3);
    EXPECT_EQ(result, expected_return);
#endif
  }
}

bool execute_tester_no_cmp(CodeTester& tester, u64 in0, u64 in1, u64 in2, u64 in3) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    tester.execute(in0, in1, in2, in3);
    return true;
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    tester.execute(in0, in1, in2, in3);
    return true;
#endif
  }
  return false;
}

void execute_ret_tester(CodeTester& tester,
                        u64 in0,
                        u64 in1,
                        u64 in2,
                        u64 in3,
                        float expected_return) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    auto result = tester.execute_ret<float>(in0, in1, in2, in3);
    EXPECT_FLOAT_EQ(result, expected_return);
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    auto result = tester.execute_ret<float>(in0, in1, in2, in3);
    EXPECT_FLOAT_EQ(result, expected_return);
#endif
  }
}
}  // namespace emitter