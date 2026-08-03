#include <cstdio>

#include "emitter_test_helpers.h"
#include "emitter_util.h"

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "goalc/emitter/Register.h"
#include "gtest/gtest.h"
#include <capstone/arm.h>
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

        EXPECT_EXECUTE_RET_EQ(tester, val, expected);
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

        EXPECT_EXECUTE_RET_EQ(tester, val, expected);
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

        EXPECT_EXECUTE_RET_EQ(tester, val, expected);
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

        EXPECT_EXECUTE_RET_EQ(tester, val, expected);
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
          EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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

          EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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

          EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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

          EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
        }
      }
    });
  });
}

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

          EXPECT_EXECUTE_RET_EQ_MSG(tester, 0, expected, fmt::format("{} & {}", v1, v2));
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

          EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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

      EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
    }
  });
}

// TODO - not yet implemented
// TEST(ARM64EmitterIntegerMath, shl_gpr64_cl) {
//   auto tester = create_tester();
//   std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
//                            INT64_MAX, 117, 32, -348473,   83747382};
//   std::vector<u8> sas = {0, 1, 23, 53, 64};

//   for_each_register_except_stack_and_scratch(tester, [&](Register i) {
//     for (auto v : vals) {
//       for (auto sa : sas) {
//         auto expected = v << sa;
//         tester.clear();
//         tester.emit_push_all_gprs(true);
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
//         tester.emit(IGen::shl_gpr64_reg(tester.generator(), i, 0));
//         tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
//         tester.emit_pop_all_gprs(true);
//         tester.emit_return();

//         EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
//       }
//     }
//   });
// }

// TEST(ARM64EmitterIntegerMath, shr_gpr64_cl) {
//   auto tester = create_tester();
//   std::vector<u64> vals = {0,         1,   u64(-2), u64(INT32_MIN), INT32_MAX, u64(INT64_MIN),
//                            INT64_MAX, 117, 32,      u64(-348473),   83747382};
//   std::vector<u8> sas = {0, 1, 23, 53, 64};

//   for_each_register_except_stack_and_scratch(tester, [&](Register i) {
//     for (auto v : vals) {
//       for (auto sa : sas) {
//         auto expected = v >> sa;
//         tester.clear();
//         tester.emit_push_all_gprs(true);
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
//         tester.emit(IGen::shr_gpr64_reg(tester.generator(), i, 0));
//         tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
//         tester.emit_pop_all_gprs(true);
//         tester.emit_return();

//         EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
//       }
//     }
//   });
// }

// TEST(ARM64EmitterIntegerMath, sar_gpr64_cl) {
//   auto tester = create_tester();
//   std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
//                            INT64_MAX, 117, 32, -348473,   83747382};
//   std::vector<u8> sas = {0, 1, 23, 53, 64};

//   for_each_register_except_stack_and_scratch(tester, [&](Register i) {
//     for (auto v : vals) {
//       for (auto sa : sas) {
//         auto expected = v >> sa;
//         tester.clear();
//         tester.emit_push_all_gprs(true);
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, v));
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), RCX, sa));
//         tester.emit(IGen::sar_gpr64_reg(tester.generator(), i, 0));
//         tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
//         tester.emit_pop_all_gprs(true);
//         tester.emit_return();

//         EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
//       }
//     }
//   });
// }

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
        EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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
        EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
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

        EXPECT_EXECUTE_RET_EQ(tester, 0, expected);
      }
    }
  });
}

TEST(ARM64EmitterIntegerMath, jumps) {
  auto tester = create_tester();

  auto x = IGen::jmp_imm(tester.generator());
  tester.emit(x);
  // read the instruction we just emitted
  auto last_instr = tester.read<u32>(tester.size());
  // analyze it, ARM is nice in this way, every instruction is just 32bits
  // no need to defer and check the immediate like in the x86 tests.
  // this has an imm26, the rest are all imm19
  EXPECT_EQ(0, last_instr & 0x03ffffff);

  x = IGen::je_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jne_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jle_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jge_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jl_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jg_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jbe_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jae_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::jb_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);

  x = IGen::ja_imm(tester.generator());
  tester.emit(x);
  last_instr = tester.read<u32>(tester.size());
  EXPECT_EQ(0, (last_instr >> 5) & 0x7ffff);
}

TEST(ARM64EmitterIntegerMath, null) {
  CodeTester tester;
  auto instr = IGen::null(tester.generator());
  EXPECT_EQ(0, instr.emit(nullptr));
}

TEST(ARM64EmitterLoadsAndStores, load_constant_64_and_move_gpr_gpr_64) {
  std::vector<u64> u64_constants = {0, UINT64_MAX, INT64_MAX, 7, 12};

  // test we can load a 64-bit constant into all gprs, move it to any other gpr, and return it.
  // SP is skipping because that's the stack pointer and would prevent us from popping gprs after

  auto tester = create_tester();

  for (auto constant : u64_constants) {
    for_each_register_except_stack_and_scratch(tester, [&](Register i) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, constant));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), j, i));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), j));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        EXPECT_EXECUTE_EQ(tester, constant);
      });
    });
  }
}

TEST(ARM64EmitterLoadsAndStores, load_constant_32_unsigned) {
  std::vector<u64> u64_constants = {0, UINT32_MAX, INT32_MAX, 7, 12};

  // test loading 32-bit constants, with all upper 32-bits zero.
  // this uses a different opcode than 64-bit loads.
  auto tester = create_tester();

  for (auto constant : u64_constants) {
    for_each_register_except_stack_and_scratch(tester, [&](Register i) {
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, UINT64_MAX));
      tester.emit(IGen::mov_gpr64_u32(tester.generator(), i, constant));
      tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EXECUTE_EQ(tester, constant);
    });
  }
}

TEST(ARM64EmitterLoadsAndStores, load_constant_32_signed) {
  std::vector<s32> s32_constants = {0, 1, INT32_MAX, INT32_MIN, 12, -1};

  // test loading signed 32-bit constants.  for values < 0 this will sign extend.
  auto tester = create_tester();

  for (auto constant : s32_constants) {
    for_each_register_except_stack_and_scratch(tester, [&](Register i) {
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_s32(tester.generator(), i, constant));
      tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EXECUTE_EQ(tester, (u64)constant);
    });
  }
}

TEST(ARM64EmitterLoadsAndStores, load8s_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E8A238");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k.id() != i.id() && k.id() != j.id()) {
          // TODO - there is a bug here of some sort, the tests will fail if this junk
          // initialization is done makes no sense to me yet
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)3, (u64)0, (u64)0, (u64)-2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)2, (u64)0, (u64)0, (u64)-3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)4, (u64)0, (u64)0, (u64)-1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)5, (u64)0, (u64)0, (u64)0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load8s_gpr64_gpr64_gpr64_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D29F38");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)3 + 3, (u64)0, (u64)0, (u64)-2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)2 + 3, (u64)0, (u64)0, (u64)-3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)4 + 3, (u64)0, (u64)0, (u64)-1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)5 + 3, (u64)0, (u64)0, (u64)0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load8s_gpr64_gpr64_gpr64_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D100028039");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)3 + 3, (u64)0, (u64)0, (u64)-2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)2 + 3, (u64)0, (u64)0, (u64)-3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)4 + 3, (u64)0, (u64)0, (u64)-1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, (u64)5 + 3, (u64)0, (u64)0, (u64)0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load8u_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E86238");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 2, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 4, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 5, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load8u_gpr64_gpr64_gpr64_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D25F38");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 3 + 3, 0, 0, 0xfe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 2 + 3, 0, 0, 0xfd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4 + 3, 0, 0, 0xff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 5 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load8u_gpr64_gpr64_gpr64_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D100024039");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 3 + 3, 0, 0, 0xfe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 2 + 3, 0, 0, 0xfd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4 + 3, 0, 0, 0xff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 5 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16s_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E8A278");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 6, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 10, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16s_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D29F78");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 6 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 4 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 8 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, memory, 10 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16s_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D100028079");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 6 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 10 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16u_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E86278");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 6, 0, 0, 0xfffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4, 0, 0, 0xfffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8, 0, 0, 0xffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 10, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16u_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D25F78");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 6 + 3, 0, 0, 0xfffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4 + 3, 0, 0, 0xfffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, 0xffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 10 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load16u_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D100024079");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 6 + 3, 0, 0, 0xfffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 4 + 3, 0, 0, 0xfffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, 0xffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 10 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32s_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E8A2B8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32s_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D29FB8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32s_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D1000280B9");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32u_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E862B8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // printf("%s\n", tester.dump_to_asm_string().data());

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12, 0, 0, 0xfffffffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8, 0, 0, 0xfffffffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16, 0, 0, 0xffffffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32u_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D25FB8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12 + 3, 0, 0, 0xfffffffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, 0xfffffffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, 0xffffffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load32u_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D1000640B8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 12 + 3, 0, 0, 0xfffffffe);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 8 + 3, 0, 0, 0xfffffffd);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, 0xffffffff);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 20 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load64_gpr64_goal_ptr_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64(tester.generator(), X0, X1, X2));
  EXPECT_EQ(tester.dump_to_hex_string(true), "20E862F8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64(tester.generator(), k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // printf("%s\n", tester.dump_to_asm_string().data());

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 24, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 32, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 40, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load64_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "30E0228B00D25FF8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 24 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 32 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 40 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load64_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, X2, -3));

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D1000640F8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except_stack_and_scratch(tester, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          // TODO
          // tester.emit(IGen::mov_gpr64_u64(tester.generator(), k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 24 + 3, 0, 0, -2);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 16 + 3, 0, 0, -3);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 32 + 3, 0, 0, -1);
        EXPECT_EXECUTE_4ARG_EQ(tester, (u64)memory, 40 + 3, 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(
      IGen::store8_gpr64_gpr64_plus_gpr64(tester.generator(), tester.get_return_reg(), RCX, RDX));
  EXPECT_EQ(tester.dump_to_hex_string(true), "02E82138");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64(tester.generator(), i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], 7);
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(),
                                                          tester.get_return_reg(), RCX, RDX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "10E0218B02C20038");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 6, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], 7);
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(),
                                                           tester.get_return_reg(), RCX, RDX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B1032009102020039");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 6, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], 7);
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(
      IGen::store16_gpr64_gpr64_plus_gpr64(tester.generator(), RCX, tester.get_return_reg(), R8));
  EXPECT_EQ(tester.dump_to_hex_string(true), "28E82078");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64(tester.generator(), i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 6, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s16(0xff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(),
                                                           tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "10E0218B08C20078");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 6 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s16(0xff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(),
                                                            tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B1032009108020079");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 6 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s16(0xff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(
      IGen::store32_gpr64_gpr64_plus_gpr64(tester.generator(), RCX, tester.get_return_reg(), R8));
  EXPECT_EQ(tester.dump_to_hex_string(true), "28E820B8");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64(tester.generator(), i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 12, 0xffffffff12341234, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], 0x12341234);
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(),
                                                           tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B10320091080200B9");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 12 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s32(0xffffff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(),
                                                            tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B10320091080200B9");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 12 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s32(0xffffff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(
      IGen::store64_gpr64_gpr64_plus_gpr64(tester.generator(), RCX, tester.get_return_reg(), R8));
  EXPECT_EQ(tester.dump_to_hex_string(true), "28E820F8");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64(tester.generator(), i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 24, 0xffffffff12341234, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], 0xffffffff12341234);
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(),
                                                           tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B10320091080200F9");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 24 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s64(0xffffffffffffff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();

  tester.clear();
  tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(),
                                                            tester.get_return_reg(), RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B10320091080200F9");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(tester.generator(), k));  // k will have the value to store.

        // store
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        const auto did_execute =
            execute_tester_no_cmp(tester, (u64)memory, 24 + 3, 0xffffffffffffff07, 0);
        if (did_execute) {
          EXPECT_EQ(memory[2], 3);
          EXPECT_EQ(memory[3], s64(0xffffffffffffff07));
          EXPECT_EQ(memory[4], 1);
        }
      });
    });
  });
}

TEST(ARM64EmitterLoadsAndStores, load64_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load64_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "60000058");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load64_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "600000586100005862000058630000586400005865000058660000586700005868000058690000586A0000"
            "586B0000586C0000586D0000586E0000586F00005870000058710000587200005873000058740000587500"
            "0058760000587700005878000058790000587A0000587B0000587C0000587D0000587E0000587F000058");
}

TEST(ARM64EmitterLoadsAndStores, load32s_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load32s_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "60000098");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load32s_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "600000986100009862000098630000986400009865000098660000986700009868000098690000986A0000"
            "986B0000986C0000986D0000986E0000986F00009870000098710000987200009873000098740000987500"
            "0098760000987700009878000098790000987A0000987B0000987C0000987D0000987E0000987F000098");
}

TEST(ARM64EmitterLoadsAndStores, load32u_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load32u_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "60000018");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load32u_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "600000186100001862000018630000186400001865000018660000186700001868000018690000186A0000"
            "186B0000186C0000186D0000186E0000186F00001870000018710000187200001873000018740000187500"
            "0018760000187700001878000018790000187A0000187B0000187C0000187D0000187E0000187F000018");
}

TEST(ARM64EmitterLoadsAndStores, load16u_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load16u_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000324079");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load16u_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003240791000009001324079100000900232407910000090033240791000009004324079100000"
            "90053240791000009006324079100000900732407910000090083240791000009009324079100000900A32"
            "4079100000900B324079100000900C324079100000900D324079100000900E324079100000900F32407910"
            "00009010324079100000901132407910000090123240791000009013324079100000901432407910000090"
            "153240791000009016324079100000901732407910000090183240791000009019324079100000901A3240"
            "79100000901B324079100000901C324079100000901D324079100000901E324079100000901F324079");
}

TEST(ARM64EmitterLoadsAndStores, load16s_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load16s_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000328079");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load16s_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003280791000009001328079100000900232807910000090033280791000009004328079100000"
            "90053280791000009006328079100000900732807910000090083280791000009009328079100000900A32"
            "8079100000900B328079100000900C328079100000900D328079100000900E328079100000900F32807910"
            "00009010328079100000901132807910000090123280791000009013328079100000901432807910000090"
            "153280791000009016328079100000901732807910000090183280791000009019328079100000901A3280"
            "79100000901B328079100000901C328079100000901D328079100000901E328079100000901F328079");
}

TEST(ARM64EmitterLoadsAndStores, load8s_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load8s_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000328039");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load8s_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003280391000009001328039100000900232803910000090033280391000009004328039100000"
            "90053280391000009006328039100000900732803910000090083280391000009009328039100000900A32"
            "8039100000900B328039100000900C328039100000900D328039100000900E328039100000900F32803910"
            "00009010328039100000901132803910000090123280391000009013328039100000901432803910000090"
            "153280391000009016328039100000901732803910000090183280391000009019328039100000901A3280"
            "39100000901B328039100000901C328039100000901D328039100000901E328039100000901F328039");
}

TEST(ARM64EmitterLoadsAndStores, load8u_rip) {
  auto tester = create_tester();
  tester.emit(IGen::load8u_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000324039");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::load8u_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003240391000009001324039100000900232403910000090033240391000009004324039100000"
            "90053240391000009006324039100000900732403910000090083240391000009009324039100000900A32"
            "4039100000900B324039100000900C324039100000900D324039100000900E324039100000900F32403910"
            "00009010324039100000901132403910000090123240391000009013324039100000901432403910000090"
            "153240391000009016324039100000901732403910000090183240391000009019324039100000901A3240"
            "39100000901B324039100000901C324039100000901D324039100000901E324039100000901F324039");
}

TEST(ARM64EmitterLoadsAndStores, store64_rip_s32) {
  auto tester = create_tester();
  tester.emit(IGen::store64_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "10000090003200F9");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::store64_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003200F910000090013200F910000090023200F910000090033200F910000090043200F9100000"
            "90053200F910000090063200F910000090073200F910000090083200F910000090093200F9100000900A32"
            "00F9100000900B3200F9100000900C3200F9100000900D3200F9100000900E3200F9100000900F3200F910"
            "000090103200F910000090113200F910000090123200F910000090133200F910000090143200F910000090"
            "153200F910000090163200F910000090173200F910000090183200F910000090193200F9100000901A3200"
            "F9100000901B3200F9100000901C3200F9100000901D3200F9100000901E3200F9100000901F3200F9");
}

TEST(ARM64EmitterLoadsAndStores, store32_rip_s32) {
  auto tester = create_tester();
  tester.emit(IGen::store32_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "10000090003200B9");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::store32_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003200B910000090013200B910000090023200B910000090033200B910000090043200B9100000"
            "90053200B910000090063200B910000090073200B910000090083200B910000090093200B9100000900A32"
            "00B9100000900B3200B9100000900C3200B9100000900D3200B9100000900E3200B9100000900F3200B910"
            "000090103200B910000090113200B910000090123200B910000090133200B910000090143200B910000090"
            "153200B910000090163200B910000090173200B910000090183200B910000090193200B9100000901A3200"
            "B9100000901B3200B9100000901C3200B9100000901D3200B9100000901E3200B9100000901F3200B9");
}

TEST(ARM64EmitterLoadsAndStores, store16_rip_s32) {
  auto tester = create_tester();
  tester.emit(IGen::store16_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000320079");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::store16_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003200791000009001320079100000900232007910000090033200791000009004320079100000"
            "90053200791000009006320079100000900732007910000090083200791000009009320079100000900A32"
            "0079100000900B320079100000900C320079100000900D320079100000900E320079100000900F32007910"
            "00009010320079100000901132007910000090123200791000009013320079100000901432007910000090"
            "153200791000009016320079100000901732007910000090183200791000009019320079100000901A3200"
            "79100000901B320079100000901C320079100000901D320079100000901E320079100000901F320079");
}

TEST(ARM64EmitterLoadsAndStores, store8_rip_s32) {
  auto tester = create_tester();
  tester.emit(IGen::store8_rip_s32(tester.generator(), tester.get_return_reg(), 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000009000320039");

  tester.clear();
  for_each_register_except(tester, {}, [&](Register i) {
    tester.emit(IGen::store8_rip_s32(tester.generator(), i, 12));
  });

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "10000090003200391000009001320039100000900232003910000090033200391000009004320039100000"
            "90053200391000009006320039100000900732003910000090083200391000009009320039100000900A32"
            "0039100000900B320039100000900C320039100000900D320039100000900E320039100000900F32003910"
            "00009010320039100000901132003910000090123200391000009013320039100000901432003910000090"
            "153200391000009016320039100000901732003910000090183200391000009019320039100000901A3200"
            "39100000901B320039100000901C320039100000901D320039100000901E320039100000901F320039");
}

TEST(ARM64EmitterLoadsAndStores, static_addr) {
  auto tester = create_tester();

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, 12345));  // load test reg with junk

    int start_of_adr = tester.size();
    auto adr_instr = IGen::static_addr(tester.generator(), i, 1);
    tester.emit(adr_instr);

    // Patch ADR to point at tester.data() + 1
    const s64 target = (s64)(tester.code_address() + 1);
    const s64 pc = (s64)(tester.code_address() + start_of_adr);
    const s64 offset = target - pc;

    ASSERT(offset >= -(1 << 20));
    ASSERT(offset < (1 << 20));

    u32 imm = static_cast<u32>(offset) & 0x1fffff;
    u32 immlo = imm & 0x3;
    u32 immhi = (imm >> 2) & 0x7ffff;

    u32 instr = tester.read<u32>(start_of_adr);
    instr &= ~((0x3 << 29) | (0x7ffff << 5));
    instr |= (immlo << 29);
    instr |= (immhi << 5);

    tester.write<u32>(instr, start_of_adr);

    tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), tester.get_return_reg(), i));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();

    EXPECT_EXECUTE_EQ(tester, (u64)(tester.data()) + 1);
  });
}

TEST(ARM64Emitter, LEA) {
  auto tester = create_tester();
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X4, SP, -3));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X4, X12, -3));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X13, SP, -3));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X13, X12, -3));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X4, SP, -300));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X4, X12, -300));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X13, SP, -300));
  tester.emit(IGen::lea_reg_plus_off(tester.generator(), X13, X12, -300));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "E4030091840C00D184010091840C00D1ED030091AD0D00D18D010091AD0D00D1E403009184B004D1840100"
            "9184B004D1ED030091ADB104D18D010091ADB104D1");
}

TEST(ARM64EmitterXMM, StackLoad32) {
  auto tester = create_tester();
  tester.emit(IGen::load32_xmm32_gpr64_plus_s32(tester.generator(), V0 + 3, SP, -1234));
  tester.emit(IGen::load32_xmm32_gpr64_plus_s32(tester.generator(), V0 + 13, SP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D1030240BDF0030091104A13D10D0240BD");
}

TEST(ARM64EmitterXMM, StackLoad8) {
  auto tester = create_tester();
  tester.emit(IGen::load32_xmm32_gpr64_plus_s8(tester.generator(), V0 + 3, SP, -12));
  tester.emit(IGen::load32_xmm32_gpr64_plus_s8(tester.generator(), V0 + 13, SP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D1030240BDF0030091103200D10D0240BD");
}

TEST(ARM64EmitterXMM, StackLoadFull32) {
  auto tester = create_tester();
  tester.emit(IGen::load128_simd128_gpr64_s32(tester.generator(), V0 + 3, SP, -1234));
  tester.emit(IGen::load128_simd128_gpr64_s32(tester.generator(), V0 + 13, SP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D10302C03DF0030091104A13D10D02C03D");
}

TEST(ARM64EmitterXMM, StackLoadFull8) {
  auto tester = create_tester();
  tester.emit(IGen::load128_simd128_gpr64_s8(tester.generator(), V0 + 3, SP, -12));
  tester.emit(IGen::load128_simd128_gpr64_s8(tester.generator(), V0 + 13, SP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D10302C03DF0030091103200D10D02C03D");
}

TEST(ARM64EmitterXMM, StackStore32) {
  auto tester = create_tester();
  tester.emit(IGen::store32_xmm32_gpr64_plus_s32(tester.generator(), SP, V0 + 3, -1234));
  tester.emit(IGen::store32_xmm32_gpr64_plus_s32(tester.generator(), SP, V0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D1030600BDF0030091104A13D10D0600BD");
}

TEST(ARM64EmitterXMM, StackStore8) {
  auto tester = create_tester();
  tester.emit(IGen::store32_xmm32_gpr64_plus_s8(tester.generator(), SP, V0 + 3, -12));
  tester.emit(IGen::store32_xmm32_gpr64_plus_s8(tester.generator(), SP, V0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D1030600BDF0030091103200D10D0600BD");
}

TEST(ARM64EmitterXMM, StackStoreFull32) {
  auto tester = create_tester();
  tester.emit(IGen::store128_gpr64_simd128_s32(tester.generator(), SP, V0 + 3, -1234));
  tester.emit(IGen::store128_gpr64_simd128_s32(tester.generator(), SP, V0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D103DA0300F0030091104A13D10DDA0300");
}

TEST(ARM64EmitterXMM, StackStoreFull8) {
  auto tester = create_tester();
  tester.emit(IGen::store128_gpr64_simd128_s8(tester.generator(), SP, V0 + 3, -12));
  tester.emit(IGen::store128_gpr64_simd128_s8(tester.generator(), SP, V0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D103DA0300F0030091103200D10DDA0300");
}

TEST(ARM64EmitterXMM, SqrtS) {
  auto tester = create_tester();
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 1, V0 + 2));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 11, V0 + 2));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 1, V0 + 12));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 11, V0 + 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "40C0211E40C02B1E80C1211E80C12B1E");
}

TEST(ARM64EmitterXmm32, load32_xmm32_gpr64_plus_gpr64) {
  auto tester = create_tester();
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64(tester.generator(), XMM3, RAX, RBX));
  EXPECT_EQ(tester.dump_to_hex_string(true), "73E860BC");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, (iter & 1) ? 0 : UINT64_MAX));
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // load into k
        tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64(tester.generator(), V0 + k.id(), i, j));
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 3 * sizeof(float), 0, 0, 3.45f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 2 * sizeof(float), 0, 0, 1.23f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 4 * sizeof(float), 0, 0, 5.67f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 5 * sizeof(float), 0, 0, 0);
        iter++;
      });
    });
  });
}

TEST(ARM64EmitterXmm32, load32_xmm32_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s8(tester.generator(), XMM3, RAX, RBX, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000038B100600D1130240BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, (iter & 1) ? 0 : UINT64_MAX));
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        // load into k
        tester.emit(
            IGen::load32_xmm32_gpr64_plus_gpr64_plus_s8(tester.generator(), V0 + k.id(), i, j, -3));
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 3 * sizeof(float) + 3, 0, 0, 3.45f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 2 * sizeof(float) + 3, 0, 0, 1.23f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 4 * sizeof(float) + 3, 0, 0, 5.67f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 5 * sizeof(float) + 3, 0, 0, 0);

        iter++;
      });
    });
  });
}

TEST(ARM64EmitterXmm32, load32_xmm32_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s32(tester.generator(), XMM3, RAX, RBX, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000038B100600D1130240BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), i, (iter & 1) ? 0 : UINT64_MAX));
        printf("a\n");
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));
        printf("b\n");

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(tester.generator(), i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(tester.generator(), j));  // j will have offset 1

        s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

        // load into k
        printf("c\n");
        tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s32(tester.generator(), V0 + k.id(), i,
                                                                 j, offset));
        printf("d\n");
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + k.id()));
        printf("e\n");

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 3 * sizeof(float) - offset, 0, 0, 3.45f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 2 * sizeof(float) - offset, 0, 0, 1.23f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 4 * sizeof(float) - offset, 0, 0, 5.67f);
        EXPECT_EXECUTE_RET_4ARG_EQ(tester, (u64)memory, 5 * sizeof(float) - offset, 0, 0, 0);
        iter++;
      });
    });
  });
}

namespace {
template <typename T>
float as_float(T x) {
  float result;
  memcpy(&result, &x, sizeof(float));
  return result;
}

u32 as_u32(float x) {
  u32 result;
  memcpy(&result, &x, 4);
  return result;
}
}  // namespace

TEST(ARM64EmitterXmm32, store32_xmm32_gpr64_plus_gpr64) {
  auto tester = create_tester();
  tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64(tester.generator(), RAX, RBX, XMM7));
  EXPECT_EQ(tester.dump_to_hex_string(true), "f3 0f 11 3c 03");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_register_except(tester, {}, [&](Register k) {
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        // push args to the stack

        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));  // addr2
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));  // addr1
        tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));  // value

        // pop value into addr1 GPR
        tester.emit(IGen::pop_gpr64(tester.generator(), i));
        // move to XMM
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(tester.generator(), i));
        tester.emit(IGen::pop_gpr64(tester.generator(), j));

        // store
        tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64(tester.generator(), i, j, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_NO_CMP(tester, (u64)memory, 12, as_u32(1.234f), 0);
        EXPECT_FLOAT_EQ(memory[2], 1.23f);
        EXPECT_FLOAT_EQ(memory[3], 1.234f);
        EXPECT_FLOAT_EQ(memory[4], 5.67f);
      });
    });
  });
}

// TEST(ARM64EmitterXmm32, store32_xmm32_gpr64_plus_gpr64_plus_s8) {
//   auto tester = create_tester();
//   tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s8(tester.generator(), RAX, RBX, XMM3,
//   -1)); EXPECT_EQ(tester.dump_to_hex_string(true), "f3 0f 11 5c 03 ff");

//   int iter = 0;
//   for_each_register_except_stack_and_scratch(tester, [&](Register i) {
//     for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
//       for_each_register_except(tester, {}, [&](Register k) {
//         tester.clear();
//         tester.emit_push_all_simd();
//         tester.emit_push_all_gprs(true);
//         // push args to the stack
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));  // addr2
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));  // addr1
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));  // value

//         // pop value into addr1 GPR
//         tester.emit(IGen::pop_gpr64(tester.generator(), i));
//         // move to XMM
//         tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

//         // pop addrs
//         tester.emit(IGen::pop_gpr64(tester.generator(), i));
//         tester.emit(IGen::pop_gpr64(tester.generator(), j));

//         s64 offset = (iter & 1) ? INT8_MAX : INT8_MIN;

//         // load into k
//         tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j,
//                                                                  V0 + k.id(), offset));

//         // move to return
//         tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + k.id()));

//         // return!
//         tester.emit_pop_all_gprs(true);
//         tester.emit_pop_all_simd();
//         tester.emit_return();

//         // prepare the memory:
//         float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

//         // run!
//         execute_tester_no_cmp(tester, (u64)memory, 12 - offset, as_u32(1.234f), 0);
//         EXPECT_FLOAT_EQ(memory[2], 1.23f);
//         EXPECT_FLOAT_EQ(memory[3], 1.234f);
//         EXPECT_FLOAT_EQ(memory[4], 5.67f);
//       });
//     });
//   });
// }

// TEST(ARM64EmitterXmm32, store32_xmm32_gpr64_plus_gpr64_plus_s32) {
//   auto tester = create_tester();
//   tester.emit(
//       IGen::store32_xmm32_gpr64_plus_gpr64_plus_s32(tester.generator(), RAX, RBX, XMM3, -1));
//   EXPECT_EQ(tester.dump_to_hex_string(true), "f3 0f 11 9c 03 ff ff ff ff");

//   int iter = 0;
//   for_each_register_except_stack_and_scratch(tester, [&](Register i) {
//     for_each_register_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
//       for_each_register_except(tester, {}, [&](Register k) {
//         tester.clear();
//         tester.emit_push_all_simd();
//         tester.emit_push_all_gprs(true);
//         // push args to the stack
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(1)));  // addr2
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(0)));  // addr1
//         tester.emit(IGen::push_gpr64(tester.generator(), tester.get_c_abi_arg_reg(2)));  // value

//         // pop value into addr1 GPR
//         tester.emit(IGen::pop_gpr64(tester.generator(), i));
//         // move to XMM
//         tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

//         // pop addrs
//         tester.emit(IGen::pop_gpr64(tester.generator(), i));
//         tester.emit(IGen::pop_gpr64(tester.generator(), j));

//         s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

//         // load into k
//         tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j,
//                                                                   V0 + k.id(), offset));

//         // move to return
//         tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + k.id()));

//         // return!
//         tester.emit_pop_all_gprs(true);
//         tester.emit_pop_all_simd();
//         tester.emit_return();

//         // prepare the memory:
//         float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

//         // run!
//         execute_tester_no_cmp(tester, (u64)memory, 12 - offset, as_u32(1.234f), 0);
//         EXPECT_FLOAT_EQ(memory[2], 1.23f);
//         EXPECT_FLOAT_EQ(memory[3], 1.234f);
//         EXPECT_FLOAT_EQ(memory[4], 5.67f);
//       });
//     });
//   });
// }

// TEST(ARM64EmitterXmm32, static_load_xmm32) {
//   auto tester = create_tester();
//   for (int i = 0; i < 16; i++) {
//     tester.clear();
//     tester.emit_push_all_simd();
//     tester.emit_push_all_gprs(true);

//     auto loc_of_load = tester.size();
//     auto load_instr = IGen::static_load_f32(tester.generator(), V0 + i, INT32_MAX);

//     tester.emit(load_instr);
//     tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + i));
//     tester.emit_pop_all_gprs(true);
//     tester.emit_pop_all_simd();
//     tester.emit_return();
//     auto loc_of_float = tester.emit_data(float(1.2345f));

//     // patch offset
//     tester.write<s32>(loc_of_float - loc_of_load - load_instr.length(),
//                       loc_of_load + load_instr.offset_of_disp());

//     execute_ret_tester(tester, 0, 0, 0, 0, 1.2345f);
//   }
// }

// TEST(ARM64EmitterXmm32, static_store_xmm32) {
//   auto tester = create_tester();
//   for (int i = 0; i < 16; i++) {
//     tester.clear();
//     tester.emit_push_all_simd();
//     tester.emit_push_all_gprs(true);
//     tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, tester.get_c_abi_arg_reg(0)));

//     auto loc_of_store = tester.size();
//     auto store_instr = IGen::static_store_f32(tester.generator(), V0 + i, INT32_MAX);

//     tester.emit(store_instr);
//     tester.emit_pop_all_gprs(true);
//     tester.emit_pop_all_simd();
//     tester.emit_return();
//     auto loc_of_float = tester.emit_data(float(1.2345f));

//     tester.write<s32>(loc_of_float - loc_of_store - store_instr.length(),
//                       loc_of_store + store_instr.offset_of_disp());
//     execute_tester_no_cmp(tester, as_u32(-44.567f), 0, 0, 0);
//     EXPECT_FLOAT_EQ(-44.567f, tester.read<float>(loc_of_float));
//   }
// }

// TEST(ARM64EmitterXmm32, ucomiss) {
//   auto tester = create_tester();
//   tester.emit(IGen::cmp_f32_f32(tester.generator(), XMM13, XMM14));
//   EXPECT_EQ("45 0f 2e ee", tester.dump_to_hex_string(true));
// }

// TEST(ARM64EmitterXmm32, mul) {
//   auto tester = create_tester();
//   std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

//   for (auto f : vals) {
//     for (auto g : vals) {
//       for (int i = 0; i < 16; i++) {
//         for (int j = 0; j < 16; j++) {
//           if (i == j) {
//             continue;
//           }
//           auto expected = f * g;
//           tester.clear();
//           tester.emit_push_all_simd();
//           tester.emit_push_all_gprs(true);
//           u64 val = 0;
//           memcpy(&val, &f, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, RAX));
//           memcpy(&val, &g, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j, RAX));
//           tester.emit(IGen::mul_f32_f32(tester.generator(), V0 + j, V0 + i));
//           tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + j));
//           tester.emit_pop_all_gprs(true);
//           tester.emit_pop_all_simd();
//           tester.emit_return();

//           execute_ret_tester(tester, 0, 0, 0, 0, expected);
//         }
//       }
//     }
//   }
// }

// TEST(ARM64EmitterXmm32, div) {
//   auto tester = create_tester();
//   std::vector<float> vals = {1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

//   for (auto f : vals) {
//     for (auto g : vals) {
//       for (int i = 0; i < 16; i++) {
//         for (int j = 0; j < 16; j++) {
//           if (i == j) {
//             continue;
//           }
//           auto expected = g / f;
//           tester.clear();
//           tester.emit_push_all_simd();
//           tester.emit_push_all_gprs(true);
//           u64 val = 0;
//           memcpy(&val, &f, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, RAX));
//           memcpy(&val, &g, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j, RAX));
//           tester.emit(IGen::div_f32_f32(tester.generator(), V0 + j, V0 + i));
//           tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + j));
//           tester.emit_pop_all_gprs(true);
//           tester.emit_pop_all_simd();
//           tester.emit_return();

//           execute_ret_tester(tester, 0, 0, 0, 0, expected);
//         }
//       }
//     }
//   }
// }

// TEST(ARM64EmitterXmm32, add) {
//   auto tester = create_tester();
//   std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};
//   for (auto f : vals) {
//     for (auto g : vals) {
//       for (int i = 0; i < 16; i++) {
//         for (int j = 0; j < 16; j++) {
//           if (i == j) {
//             continue;
//           }
//           auto expected = g + f;
//           tester.clear();
//           tester.emit_push_all_simd();
//           tester.emit_push_all_gprs(true);
//           u64 val = 0;
//           memcpy(&val, &f, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, RAX));
//           memcpy(&val, &g, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j, RAX));
//           tester.emit(IGen::add_f32_f32(tester.generator(), V0 + j, V0 + i));
//           tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + j));
//           tester.emit_pop_all_gprs(true);
//           tester.emit_pop_all_simd();
//           tester.emit_return();

//           execute_ret_tester(tester, 0, 0, 0, 0, expected);
//         }
//       }
//     }
//   }
// }

// TEST(ARM64EmitterXmm32, sub) {
//   auto tester = create_tester();
//   std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

//   for (auto f : vals) {
//     for (auto g : vals) {
//       for (int i = 0; i < 16; i++) {
//         for (int j = 0; j < 16; j++) {
//           if (i == j) {
//             continue;
//           }
//           auto expected = g - f;
//           tester.clear();
//           tester.emit_push_all_simd();
//           tester.emit_push_all_gprs(true);
//           u64 val = 0;
//           memcpy(&val, &f, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, RAX));
//           memcpy(&val, &g, sizeof(float));
//           tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//           tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j, RAX));
//           tester.emit(IGen::sub_f32_f32(tester.generator(), V0 + j, V0 + i));
//           tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + j));
//           tester.emit_pop_all_gprs(true);
//           tester.emit_pop_all_simd();
//           tester.emit_return();

//           execute_ret_tester(tester, 0, 0, 0, 0, expected);
//         }
//       }
//     }
//   }
// }

// TEST(ARM64EmitterXmm32, float_to_int) {
//   auto tester = create_tester();
//   std::vector<float> vals = {0.f,    1.f,  0.2f, -1.f,  1235423.2f, -3457343.3f,
//                              7.545f, 0.1f, 0.9f, -0.1f, -0.9f};

//   for (auto g : vals) {
//     for (int i = 0; i < 16; i++) {
//       for (int j = 0; j < 16; j++) {
//         if (j == SP) {
//           continue;
//         }
//         s32 expected = g;
//         tester.clear();
//         tester.emit_push_all_simd();
//         tester.emit_push_all_gprs(true);
//         u64 val = 0;
//         memcpy(&val, &g, sizeof(float));
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), RAX, val));
//         tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i, RAX));
//         tester.emit(IGen::f32_to_int32(tester.generator(), j, V0 + i));
//         tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), RAX, j));
//         tester.emit_pop_all_gprs(true);
//         tester.emit_pop_all_simd();
//         tester.emit_return();
//         // TODO - no idea why the function call doesn't work here
//         // execute_ret_tester(tester, 0, 0, 0, 0, expected);
//         if (tester.generator().instr_set() == InstructionSet::ARM64) {
// #ifdef __aarch64__
//           auto result = tester.execute_ret<s32>(0, 0, 0, 0);
//           EXPECT_FLOAT_EQ(result, expected);
// #endif
//         } else if (tester.generator().instr_set() == InstructionSet::X86) {
// #ifndef __aarch64__
//           auto result = tester.execute_ret<s32>(0, 0, 0, 0);
//           EXPECT_FLOAT_EQ(result, expected);
// #endif
//         }
//       }
//     }
//   }
// }

// TEST(ARM64EmitterXmm32, int_to_float) {
//   auto tester = create_tester();
//   std::vector<s64> vals = {0, 1, -1, INT32_MAX, -3457343, 7, INT32_MIN};

//   for (auto g : vals) {
//     for (int i = 0; i < 16; i++) {
//       for (int j = 0; j < 16; j++) {
//         if (j == SP) {
//           continue;
//         }
//         float expected = g;
//         tester.clear();
//         tester.emit_push_all_simd();
//         tester.emit_push_all_gprs(true);
//         tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, g));
//         tester.emit(IGen::int32_to_f32(tester.generator(), V0 + i, j));
//         tester.emit(IGen::movd_gpr32_f32(tester.generator(), RAX, V0 + i));
//         tester.emit_pop_all_gprs(true);
//         tester.emit_pop_all_simd();
//         tester.emit_return();

//         execute_ret_tester(tester, 0, 0, 0, 0, expected);
//       }
//     }
//   }
// }

// TEST(ARM64EmitterSlow, xmm32_move) {
//   std::vector<u32> u32_constants = {0, INT32_MAX, UINT32_MAX, 17};

//   // test moving between xmms (32-bit) and gprs.
//   auto tester = create_tester();

//   for (auto constant : u32_constants) {
//     for (int r1 = 0; r1 < 16; r1++) {
//       if (r1 == SP) {
//         continue;
//       }
//       for (int r2 = 0; r2 < 16; r2++) {
//         if (r2 == SP) {
//           continue;
//         }
//         for (int r3 = 0; r3 < 16; r3++) {
//           for (int r4 = 0; r4 < 16; r4++) {
//             tester.clear();
//             tester.emit_push_all_simd();
//             tester.emit_push_all_gprs(true);
//             // move constant to gpr
//             tester.emit(IGen::mov_gpr64_u32(tester.generator(), r1, constant));
//             // move gpr to xmm
//             tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + r3, r1));
//             // move xmm to xmm
//             tester.emit(IGen::mov_f32_f32(tester.generator(), V0 + r4, V0 + r3));
//             // move xmm to gpr
//             tester.emit(IGen::movd_gpr32_f32(tester.generator(), r2, V0 + r4));
//             // return!
//             tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), RAX, r2));
//             tester.emit_pop_all_gprs(true);
//             tester.emit_pop_all_simd();
//             tester.emit_return();
//           }
//         }
//       }
//     }
//   }
//   // todo - finish this test
// }