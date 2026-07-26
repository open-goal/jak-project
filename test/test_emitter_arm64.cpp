#include <cstdio>

#include "emitter_test_helpers.h"
#include "emitter_util.h"

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"
#include <fmt/base.h>
#include <fmt/format.h>

using namespace emitter;

namespace {
const auto instr_set = InstructionSet::ARM64;

CodeTester create_tester(int code_capacity = 1024) {
  CodeTester tester(instr_set);
  tester.init_code_buffer(code_capacity);
  return tester;
}

};  // namespace

template <typename Fn>
void for_each_register_except(CodeTester& tester, Register excluded, Fn&& fn) {
  for (int i = 0; i < tester.get_reg_count(); i++) {
    if (::testing::Test::HasFatalFailure()) {
      return;
    }
    Register reg(i);
    if (reg.id() == excluded.id()) {
      continue;
    }
    fn(reg);
  }
}

template <typename Fn>
void for_each_register_except(CodeTester& tester,
                              std::initializer_list<Register> excluded,
                              Fn&& fn) {
  for (int i = 0; i < tester.get_reg_count(); i++) {
    Register reg(i);
    bool skip = false;
    for (Register ex : excluded) {
      if (reg.id() == ex.id()) {
        skip = true;
        break;
      }
    }
    if (skip) {
      continue;
    }
    fn(reg);
    // Stop iterating if a FAIL()/ASSERT_* occurred inside the lambda.
    if (::testing::Test::HasFatalFailure()) {
      return;
    }
  }
}

template <typename Fn>
void for_each_register_except_stack_and_scratch(CodeTester& tester, Fn&& fn) {
  for_each_register_except(tester, {tester.get_stack_reg(), Register(X16)}, std::forward<Fn>(fn));
}

TEST(ARM64EmitterIntegerMath, add_gpr64_imm8s) {
  auto tester = create_tester();

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX};

  // test the ones that aren't sp
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto val : vals) {
      for (auto imm : imms) {
        tester.clear();
        auto expected = val + imm;

        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), i, X0));
        // do the add
        tester.emit(IGen::add_gpr64_imm8s(tester.generator(), i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), X0, i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, val, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, add_gpr64_imm32s) {
  auto tester = create_tester();

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX, INT32_MIN, INT32_MAX};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto val : vals) {
      for (auto imm : imms) {
        tester.clear();
        auto expected = val + imm;

        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::add_gpr64_imm32s(tester.generator(), i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, val, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, sub_gpr64_imm8s) {
  auto tester = create_tester();

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto val : vals) {
      for (auto imm : imms) {
        tester.clear();
        auto expected = val - imm;

        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::sub_gpr64_imm8s(tester.generator(), i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, val, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, sub_gpr64_imm32s) {
  auto tester = create_tester();

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX, INT32_MIN, INT32_MAX};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto val : vals) {
      for (auto imm : imms) {
        tester.clear();
        auto expected = val - imm;

        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::sub_gpr64_imm32s(tester.generator(), i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, val, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, add_gpr64_gpr64) {
  auto tester = create_tester();

  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          tester.clear();
          auto expected = v1 + v2;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, v2));
          tester.emit(IGen::add_gpr64_gpr64(tester.generator(), i, j));
          tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), X0, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          EXPECT_EXECUTE_EQ(tester, 0, expected);
        }
      }
    });
  });
}

TEST(ARM64EmitterIntegerMath, sub_gpr64_gpr64) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          tester.clear();
          auto expected = v1 - v2;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, v2));
          tester.emit(IGen::sub_gpr64_gpr64(tester.generator(), i, j));
          tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();

          EXPECT_EXECUTE_EQ(tester, 0, expected);
        }
      }
    });
  });
}

TEST(ARM64EmitterIntegerMath, mul_gpr32_gpr32) {
  auto tester = create_tester();
  std::vector<s32> vals = {
      0, 1, -2, -20, 123123, INT32_MIN, INT32_MAX, INT32_MIN + 1, INT32_MAX - 1};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          // this is kind of weird behavior, but it's what the PS2 CPU does, I think.
          // the lower 32-bits of the result are sign extended, even if this sign doesn't match
          // the sign of the real product.  This is true for both signed and unsigned multiply.
          tester.clear();
          auto expected = ((s64(v1) * s64(v2)) << 32) >> 32;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, (s64)v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, (s64)v2));
          tester.emit(IGen::imul_gpr32_gpr32(tester.generator(), i, j));
          tester.emit(IGen::movsx_r64_r32(tester.generator(), tester.get_return_reg(),
                                          i));  // weird PS2 sign extend.
          tester.emit_pop_all_gprs(true);
          tester.emit_return();

          EXPECT_EXECUTE_EQ(tester, 0, expected);
        }
      }
    });
  });
}

TEST(ARM64EmitterIntegerMath, or_gpr64_gpr64) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          tester.clear();
          auto expected = v1 | v2;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, v2));
          tester.emit(IGen::or_gpr64_gpr64(tester.generator(), i, j));
          tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();

          EXPECT_EXECUTE_EQ(tester, 0, expected);
        }
      }
    });
  });
}

// TODO - make the failures strip out the push/pop (replace with something)
// can do this once integrating with capstone

TEST(ARM64EmitterIntegerMath, and_gpr64_gpr64) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          tester.clear();
          auto expected = v1 & v2;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, v2));
          tester.emit(IGen::and_gpr64_gpr64(tester.generator(), i, j));
          tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();

          EXPECT_EXECUTE_EQ_MSG(tester, 0, expected, fmt::format("{} & {}", v1, v2));
        }
      }
    });
  });
}

TEST(ARM64EmitterIntegerMath, xor_gpr64_gpr64) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          tester.clear();
          auto expected = v1 ^ v2;
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, v2));
          tester.emit(IGen::xor_gpr64_gpr64(tester.generator(), i, j));
          tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();

          EXPECT_EXECUTE_EQ(tester, 0, expected);
        }
      }
    });
  });
}

TEST(ARM64EmitterIntegerMath, not_gpr64) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v1 : vals) {
      auto expected = ~v1;
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v1));
      tester.emit(IGen::not_gpr64(tester.generator(), i));
      tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();

      EXPECT_EXECUTE_EQ(tester, 0, expected);
    }
  });
}

TEST(ARM64EmitterIntegerMath, shl_gpr64_cl) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v << sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
        tester.emit(IGen::shl_gpr64_reg(tester.generator(), i, 0));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, shr_gpr64_cl) {
  auto tester = create_tester();
  std::vector<u64> vals = {0,         1,   u64(-2), u64(INT32_MIN), INT32_MAX, u64(INT64_MIN),
                           INT64_MAX, 117, 32,      u64(-348473),   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
        tester.emit(IGen::shr_gpr64_reg(tester.generator(), i, 0));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, sar_gpr64_cl) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
        tester.emit(IGen::sar_gpr64_reg(tester.generator(), i, 0));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, shl_gpr64_u8) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v << sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::shl_gpr64_u8(tester.generator(), i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, shr_gpr64_u8) {
  auto tester = create_tester();
  std::vector<u64> vals = {0,         1,   u64(-2), u64(INT32_MIN), INT32_MAX, u64(INT64_MIN),
                           INT64_MAX, 117, 32,      u64(-348473),   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::shr_gpr64_u8(tester.generator(), i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, sar_gpr64_u8) {
  auto tester = create_tester();
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
        tester.emit(IGen::sar_gpr64_u8(tester.generator(), i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        EXPECT_EXECUTE_EQ(tester, 0, expected);
      }
    }
  });
}