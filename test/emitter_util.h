#pragma once

#include "goalc/emitter/CodeTester.h"

namespace emitter {

void execute_ret_tester(CodeTester& tester, u64 val, s64 expected_return);

void execute_tester(CodeTester& tester, u64 expected_return);

void execute_tester(CodeTester& tester, u64 in0, u64 in1, u64 in2, u64 in3, u64 expected_return);

bool execute_tester_no_cmp(CodeTester& tester, u64 in0, u64 in1, u64 in2, u64 in3);

void execute_ret_tester(CodeTester& tester,
                        u64 in0,
                        u64 in1,
                        u64 in2,
                        u64 in3,
                        float expected_return);
}  // namespace emitter