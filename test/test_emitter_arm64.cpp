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
  // rsp is skipping because that's the stack pointer and would prevent us from popping gprs after

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