#include <algorithm>
#include <cstdio>

#include "emitter_test_helpers.h"
#include "emitter_util.h"

#include "common/link_types.h"
#include "common/type_system/TypeSystem.h"

#include "goalc/compiler/IR.h"
#include "goalc/debugger/DebugInfo.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "goalc/emitter/IGenARM64.h"
#include "goalc/emitter/ObjectGenerator.h"
#include "goalc/emitter/Register.h"
#include "goalc/regalloc/Allocator.h"
#include "goalc/regalloc/Allocator_v2.h"
#include "gtest/gtest.h"
#include <capstone/arm.h>
#include <fmt/base.h>
#include <fmt/format.h>

#include "fmt/format.h"

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
void for_each_gpr_except(CodeTester& tester, std::initializer_list<Register> excluded, Fn&& fn) {
  for_each_register_except(tester, excluded, [&](Register reg) {
    if (reg.id() != X18) {
      fn(reg);
    }
  });
}

template <typename Fn>
void for_each_register_except_stack_and_scratch(CodeTester& tester, Fn&& fn) {
  for_each_gpr_except(tester, {tester.get_stack_reg(), Register(X16)}, std::forward<Fn>(fn));
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D1000240B8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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

  EXPECT_EQ(tester.dump_to_hex_string(true), "3000028B100E00D1000240F8");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
      for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i, j}, [&](Register k) {
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
            "0058760000587700005878000058790000587A0000587B0000587C0000587D0000587E000058");
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
            "0098760000987700009878000098790000987A0000987B0000987C0000987D0000987E000098");
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
            "0018760000187700001878000018790000187A0000187B0000187C0000187D0000187E000018");
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
            "79100000901B324079100000901C324079100000901D324079100000901E324079");
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
            "79100000901B328079100000901C328079100000901D328079100000901E328079");
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
            "39100000901B328039100000901C328039100000901D328039100000901E328039");
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
            "39100000901B324039100000901C324039100000901D324039100000901E324039");
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
            "F9100000901B3200F9100000901C3200F9100000901D3200F9100000901E3200F9");
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
            "B9100000901B3200B9100000901C3200B9100000901D3200B9100000901E3200B9");
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
            "79100000901B320079100000901C320079100000901D320079100000901E320079");
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
            "39100000901B320039100000901C320039100000901D320039100000901E320039");
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

TEST(ARM64EmitterSIMD, StackLoad32) {
  auto tester = create_tester();
  tester.emit(IGen::load32_simd32_gpr64_plus_s32(tester.generator(), V0 + 3, SP, -1234));
  tester.emit(IGen::load32_simd32_gpr64_plus_s32(tester.generator(), V0 + 13, SP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "F0030091519A80D2100211CB030240BDF0030091519A80D2100211CB0D0240BD");
}

TEST(ARM64EmitterSIMD, StackLoad8) {
  auto tester = create_tester();
  tester.emit(IGen::load32_simd32_gpr64_plus_s8(tester.generator(), V0 + 3, SP, -12));
  tester.emit(IGen::load32_simd32_gpr64_plus_s8(tester.generator(), V0 + 13, SP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "F0030091910180D2100211CB030240BDF0030091910180D2100211CB0D0240BD");
}

TEST(ARM64EmitterSIMD, StackLoadFull32) {
  auto tester = create_tester();
  tester.emit(IGen::load128_simd128_gpr64_s32(tester.generator(), V0 + 3, SP, -1234));
  tester.emit(IGen::load128_simd128_gpr64_s32(tester.generator(), V0 + 13, SP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D10302C03DF0030091104A13D10D02C03D");
}

TEST(ARM64EmitterSIMD, StackLoadFull8) {
  auto tester = create_tester();
  tester.emit(IGen::load128_simd128_gpr64_s8(tester.generator(), V0 + 3, SP, -12));
  tester.emit(IGen::load128_simd128_gpr64_s8(tester.generator(), V0 + 13, SP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D10302C03DF0030091103200D10D02C03D");
}

TEST(ARM64EmitterSIMD, StackStore32) {
  auto tester = create_tester();
  tester.emit(IGen::store32_simd32_gpr64_plus_s32(tester.generator(), SP, V0 + 3, -1234));
  tester.emit(IGen::store32_simd32_gpr64_plus_s32(tester.generator(), SP, V0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "F0030091519A80D2100211CB030200BDF0030091519A80D2100211CB0D0200BD");
}

TEST(ARM64EmitterSIMD, StackStore8) {
  auto tester = create_tester();
  tester.emit(IGen::store32_simd32_gpr64_plus_s8(tester.generator(), SP, V0 + 3, -12));
  tester.emit(IGen::store32_simd32_gpr64_plus_s8(tester.generator(), SP, V0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "F0030091910180D2100211CB030200BDF0030091910180D2100211CB0D0200BD");
}

TEST(ARM64EmitterSIMD, StackStoreFull32) {
  auto tester = create_tester();
  tester.emit(IGen::store128_gpr64_simd128_s32(tester.generator(), SP, V0 + 3, -1234));
  tester.emit(IGen::store128_gpr64_simd128_s32(tester.generator(), SP, V0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091104A13D10302803DF0030091104A13D10D02803D");
}

TEST(ARM64EmitterSIMD, StackStoreFull8) {
  auto tester = create_tester();
  tester.emit(IGen::store128_gpr64_simd128_s8(tester.generator(), SP, V0 + 3, -12));
  tester.emit(IGen::store128_gpr64_simd128_s8(tester.generator(), SP, V0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F0030091103200D10302803DF0030091103200D10D02803D");
}

TEST(ARM64EmitterSIMD, SqrtS) {
  auto tester = create_tester();
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 1, V0 + 2));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 11, V0 + 2));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 1, V0 + 12));
  tester.emit(IGen::sqrt_f32(tester.generator(), V0 + 11, V0 + 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "41C0211E4BC0211E81C1211E8BC1211E");
}

TEST(ARM64EmitterFloat32, load32_simd32_gpr64_plus_gpr64) {
  auto tester = create_tester();
  tester.emit(IGen::load32_simd32_gpr64_plus_gpr64(tester.generator(), V3, X0, X1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "03E861BC");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
        tester.emit(IGen::load32_simd32_gpr64_plus_gpr64(tester.generator(), V0 + k.id(), i, j));
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + k.id()));

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

TEST(ARM64EmitterFloat32, load32_simd32_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();
  tester.emit(IGen::load32_simd32_gpr64_plus_gpr64_plus_s8(tester.generator(), V3, X0, X1, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B310080D2100211CB030240BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
        tester.emit(IGen::load32_simd32_gpr64_plus_gpr64_plus_s8(tester.generator(), V0 + k.id(), i,
                                                                 j, -3));
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + k.id()));

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

TEST(ARM64EmitterFloat32, load32_simd32_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();
  tester.emit(IGen::load32_simd32_gpr64_plus_gpr64_plus_s32(tester.generator(), V3, X0, X1, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B310080D2100211CB030240BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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

        s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

        // load into k
        tester.emit(IGen::load32_simd32_gpr64_plus_gpr64_plus_s32(tester.generator(), V0 + k.id(),
                                                                  i, j, offset));
        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + k.id()));

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

TEST(ARM64EmitterFloat32, store32_simd32_gpr64_plus_gpr64) {
  auto tester = create_tester();
  tester.emit(IGen::store32_simd32_gpr64_plus_gpr64(tester.generator(), X0, X1, V7));
  EXPECT_EQ(tester.dump_to_hex_string(true), "07E821BC");

  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
        // move to SIMD
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(tester.generator(), i));
        tester.emit(IGen::pop_gpr64(tester.generator(), j));

        // store
        tester.emit(IGen::store32_simd32_gpr64_plus_gpr64(tester.generator(), i, j, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_NO_CMP(tester, (u64)memory, 12, as_u32(1.234f), 0);
        EXPECT_EXECUTE_IF_NATIVE(tester, {
          EXPECT_FLOAT_EQ(memory[2], 1.23f);
          EXPECT_FLOAT_EQ(memory[3], 1.234f);
          EXPECT_FLOAT_EQ(memory[4], 5.67f);
        });
      });
    });
  });
}

TEST(ARM64EmitterFloat32, store32_simd32_gpr64_plus_gpr64_plus_s8) {
  auto tester = create_tester();
  tester.emit(IGen::store32_simd32_gpr64_plus_gpr64_plus_s8(tester.generator(), X0, X1, V3, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B310080D2100211CB030200BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
        // move to SIMD
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(tester.generator(), i));
        tester.emit(IGen::pop_gpr64(tester.generator(), j));

        s64 offset = (iter & 1) ? INT8_MAX : INT8_MIN;

        // load into k
        tester.emit(IGen::store32_simd32_gpr64_plus_gpr64_plus_s8(tester.generator(), i, j,
                                                                  V0 + k.id(), offset));

        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_NO_CMP(tester, (u64)memory, 12 - offset, as_u32(1.234f), 0);
        EXPECT_EXECUTE_IF_NATIVE(tester, {
          EXPECT_FLOAT_EQ(memory[2], 1.23f);
          EXPECT_FLOAT_EQ(memory[3], 1.234f);
          EXPECT_FLOAT_EQ(memory[4], 5.67f);
        });
      });
    });
  });
}

TEST(ARM64EmitterFloat32, store32_simd32_gpr64_plus_gpr64_plus_s32) {
  auto tester = create_tester();
  tester.emit(IGen::store32_simd32_gpr64_plus_gpr64_plus_s32(tester.generator(), X0, X1, V3, -1));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1000018B310080D2100211CB030200BD");

  int iter = 0;
  for_each_register_except_stack_and_scratch(tester, [&](Register i) {
    for_each_gpr_except(tester, {tester.get_stack_reg(), X16, i}, [&](Register j) {
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
        // move to SIMD
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + k.id(), i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(tester.generator(), i));
        tester.emit(IGen::pop_gpr64(tester.generator(), j));

        s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

        // load into k
        tester.emit(IGen::store32_simd32_gpr64_plus_gpr64_plus_s32(tester.generator(), i, j,
                                                                   V0 + k.id(), offset));

        // move to return
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + k.id()));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_EXECUTE_4ARG_NO_CMP(tester, (u64)memory, 12 - offset, as_u32(1.234f), 0);
        EXPECT_EXECUTE_IF_NATIVE(tester, {
          EXPECT_FLOAT_EQ(memory[2], 1.23f);
          EXPECT_FLOAT_EQ(memory[3], 1.234f);
          EXPECT_FLOAT_EQ(memory[4], 5.67f);
        });
      });
    });
  });
}

// TEST(ARM64EmitterFloat32, static_load_xmm32) {
//   // TODO - int32 max is not supported in current arm64 impl because
//   // the assumption is that we don't need that much range
//   auto tester = create_tester();
//   for_each_register_except(tester, {}, [&](Register i) {
//     tester.clear();
//     tester.emit_push_all_simd();
//     tester.emit_push_all_gprs(true);

//     auto loc_of_load = tester.size();
//     auto load_instr = IGen::static_load_f32(tester.generator(), V0 + i.id(), INT32_MAX);

//     tester.emit(load_instr);
//     tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + i.id()));
//     tester.emit_pop_all_gprs(true);
//     tester.emit_pop_all_simd();
//     tester.emit_return();
//     auto loc_of_float = tester.emit_data(float(1.2345f));

//     // patch offset
//     tester.write<s32>(loc_of_float - loc_of_load - load_instr.length(),
//                       loc_of_load + load_instr.offset_of_disp());

//     EXPECT_EXECUTE_RET_4ARG_EQ(tester, 0, 0, 0, 0, 1.2345f);
//   });
// }

// TEST(ARM64EmitterFloat32, static_store_xmm32) {
//   // TODO - int32 max is not supported in current arm64 impl because
//   // the assumption is that we don't need that much range
//   auto tester = create_tester();
//   for_each_register_except(tester, {}, [&](Register i) {
//     tester.clear();
//     tester.emit_push_all_simd();
//     tester.emit_push_all_gprs(true);
//     tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(),
//     tester.get_c_abi_arg_reg(0)));

//     auto loc_of_store = tester.size();
//     auto store_instr = IGen::static_store_f32(tester.generator(), V0 + i.id(), INT32_MAX);

//     tester.emit(store_instr);
//     tester.emit_pop_all_gprs(true);
//     tester.emit_pop_all_simd();
//     tester.emit_return();
//     auto loc_of_float = tester.emit_data(float(1.2345f));

//     tester.write<s32>(loc_of_float - loc_of_store - store_instr.length(),
//                       loc_of_store + store_instr.offset_of_disp());
//     EXPECT_EXECUTE_4ARG_NO_CMP(tester, as_u32(-44.567f), 0, 0, 0);
//     EXPECT_FLOAT_EQ(-44.567f, tester.read<float>(loc_of_float));
//   });
// }

TEST(ARM64EmitterFloat32, ucomiss) {
  auto tester = create_tester();
  tester.emit(IGen::cmp_f32_f32(tester.generator(), V13, V14));
  EXPECT_EQ("A0212E1E", tester.dump_to_hex_string(true));
}

TEST(ARM64EmitterFloat32, mul) {
  auto tester = create_tester();
  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for_each_register_except(tester, {}, [&](Register i) {
        for_each_register_except(tester, {i}, [&](Register j) {
          auto expected = f * g;
          tester.clear();
          tester.emit_push_all_simd();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(), X0));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j.id(), X0));
          tester.emit(IGen::mul_f32_f32(tester.generator(), V0 + j.id(), V0 + i.id()));
          tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + j.id()));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_simd();
          tester.emit_return();

          EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ(tester, 0, 0, 0, 0, expected);
        });
      });
    }
  }
}

namespace {
// quotient and optional remainder used by IR_IntegerMath
void emit_div_sequence(CodeTester& tester, bool is_signed, bool wants_remainder) {
  const Register dest(X0), arg(X1), scratch(X16);
  auto quotient = wants_remainder ? scratch : dest;
  tester.emit(is_signed ? IGen::ARM64::sdiv_gpr32(quotient, dest, arg)
                        : IGen::ARM64::udiv_gpr32(quotient, dest, arg));
  if (wants_remainder) {
    tester.emit(IGen::ARM64::msub_gpr32(dest, quotient, arg, dest));
  }
  tester.emit(IGen::movsx_r64_r32(tester.generator(), dest, dest));
  tester.emit_return();
}
}  // namespace

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64EmitterDivide, signed_and_unsigned_division) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  std::vector<std::pair<s32, s32>> cases = {{7, 2},    {-7, 2},        {7, -2},  {-7, -2},
                                            {1, 1},    {0, 5},         {-1, 3},  {100, 7},
                                            {-100, 7}, {INT32_MIN, 2}, {-1, -1}, {12345, 100}};

  for (auto [a, b] : cases) {
    auto in0 = u64(u32(a)), in1 = u64(u32(b));

    tester.clear();
    emit_div_sequence(tester, true, false);
    EXPECT_EQ(tester.execute_ret<s64>(in0, in1, 0, 0), s64(a / b)) << a << " / " << b;

    tester.clear();
    emit_div_sequence(tester, true, true);
    EXPECT_EQ(tester.execute_ret<s64>(in0, in1, 0, 0), s64(a % b)) << a << " % " << b;

    tester.clear();
    emit_div_sequence(tester, false, false);
    EXPECT_EQ(tester.execute_ret<s64>(in0, in1, 0, 0), s64(s32(u32(a) / u32(b))))
        << u32(a) << " u/ " << u32(b);

    tester.clear();
    emit_div_sequence(tester, false, true);
    EXPECT_EQ(tester.execute_ret<s64>(in0, in1, 0, 0), s64(s32(u32(a) % u32(b))))
        << u32(a) << " u% " << u32(b);
  }
  tester.clear();
}
#endif  // __aarch64__

// division by zero and signed overflow
#ifdef __aarch64__  // runs the code it emits
TEST(ARM64EmitterDivide, division_edge_cases) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  tester.clear();
  emit_div_sequence(tester, true, false);
  EXPECT_EQ(tester.execute_ret<s64>(5, 0, 0, 0), 0) << "5 / 0";

  tester.clear();
  emit_div_sequence(tester, true, true);
  EXPECT_EQ(tester.execute_ret<s64>(5, 0, 0, 0), 5) << "5 % 0";

  tester.clear();
  emit_div_sequence(tester, true, false);
  EXPECT_EQ(tester.execute_ret<s64>(u64(u32(INT32_MIN)), u64(u32(-1)), 0, 0), INT32_MIN)
      << "INT32_MIN / -1";

  tester.clear();
  emit_div_sequence(tester, true, true);
  EXPECT_EQ(tester.execute_ret<s64>(u64(u32(INT32_MIN)), u64(u32(-1)), 0, 0), 0)
      << "INT32_MIN % -1";
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64EmitterVF, blend_and_swizzle) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);

  // in0 and in1 are the sources, in2 is the destination
  auto run = [&tester](const std::array<u32, 4>& a, const std::array<u32, 4>& b,
                       const Instruction& op, Register output = Register(V5)) {
    alignas(16) std::array<u32, 4> src1 = a, src2 = b, out = {0, 0, 0, 0};
    tester.clear();
    // x3 stays zero so the loads use the base directly
    tester.emit(IGen::loadvf_gpr64_plus_gpr64(tester.generator(), Register(V3), Register(X0),
                                              Register(X3)));
    tester.emit(IGen::loadvf_gpr64_plus_gpr64(tester.generator(), Register(V7), Register(X1),
                                              Register(X3)));
    tester.emit(op);
    tester.emit(
        IGen::storevf_gpr64_plus_gpr64(tester.generator(), output, Register(X2), Register(X3)));
    tester.emit_return();
    tester.execute_ret<u64>((u64)src1.data(), (u64)src2.data(), (u64)out.data(), 0);
    return out;
  };

  const std::array<u32, 4> a = {0xa0, 0xa1, 0xa2, 0xa3};
  const std::array<u32, 4> b = {0xb0, 0xb1, 0xb2, 0xb3};

  for (u8 mask = 0; mask < 16; mask++) {
    auto got = run(a, b, IGen::ARM64::blend_vf(Register(V5), Register(V3), Register(V7), mask));
    for (int lane = 0; lane < 4; lane++) {
      u32 want = (mask & (1 << lane)) ? b[lane] : a[lane];
      EXPECT_EQ(got[lane], want) << "blend mask " << int(mask) << " lane " << lane;
    }
  }

  for (int ctrl = 0; ctrl < 256; ctrl++) {
    auto got = run(a, b, IGen::ARM64::swizzle_vf(Register(V5), Register(V3), u8(ctrl)));
    for (int lane = 0; lane < 4; lane++) {
      EXPECT_EQ(got[lane], a[(ctrl >> (lane * 2)) & 3]) << "swizzle " << ctrl << " lane " << lane;
    }
  }

  // destination aliases src1
  auto same = run(a, b, IGen::ARM64::blend_vf(Register(V3), Register(V3), Register(V7), 0b0101),
                  Register(V3));
  for (int lane = 0; lane < 4; lane++) {
    EXPECT_EQ(same[lane], (0b0101 & (1 << lane)) ? b[lane] : a[lane]);
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64EmitterVF, halfword_shuffles) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);

  alignas(16) std::array<u16, 8> src = {0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17};

  auto run = [&tester, &src](const Instruction& op) {
    alignas(16) std::array<u16, 8> out = {0, 0, 0, 0, 0, 0, 0, 0};
    tester.clear();
    tester.emit(IGen::loadvf_gpr64_plus_gpr64(tester.generator(), Register(V3), Register(X0),
                                              Register(X3)));
    tester.emit(op);
    tester.emit(IGen::storevf_gpr64_plus_gpr64(tester.generator(), Register(V5), Register(X1),
                                               Register(X3)));
    tester.emit_return();
    tester.execute_ret<u64>((u64)src.data(), (u64)out.data(), 0, 0);
    return out;
  };

  for (int imm = 0; imm < 256; imm++) {
    auto lo = run(IGen::ARM64::vpshuflw(Register(V5), Register(V3), u8(imm)));
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(lo[i], src[(imm >> (i * 2)) & 3]) << "pshuflw " << imm << " lane " << i;
      EXPECT_EQ(lo[4 + i], src[4 + i]) << "pshuflw " << imm << " kept lane " << (4 + i);
    }

    auto hi = run(IGen::ARM64::vpshufhw(Register(V5), Register(V3), u8(imm)));
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(hi[i], src[i]) << "pshufhw " << imm << " kept lane " << i;
      EXPECT_EQ(hi[4 + i], src[4 + ((imm >> (i * 2)) & 3)]) << "pshufhw " << imm << " lane " << i;
    }
  }
  tester.clear();
}
#endif  // __aarch64__

TEST(ARM64EmitterStackPointer, sp_encodings) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  const Register sp(SP), x5(X5), x22(X22);
  struct {
    Instruction instr;
    u32 expected;
    const char* asm_text;
  } cases[] = {
      {IGen::mov_gpr64_gpr64(tester.generator(), x5, sp), 0x910003e5, "mov x5, sp"},
      {IGen::mov_gpr64_gpr64(tester.generator(), sp, x5), 0x910000bf, "mov sp, x5"},
      {IGen::add_gpr64_gpr64(tester.generator(), sp, x22), 0x8b3663ff, "add sp, sp, x22"},
      {IGen::sub_gpr64_gpr64(tester.generator(), sp, x22), 0xcb3663ff, "sub sp, sp, x22"},
      // regular register encodings
      {IGen::mov_gpr64_gpr64(tester.generator(), x5, Register(X30)), 0xaa1e03e5, "mov x5, x30"},
      {IGen::add_gpr64_gpr64(tester.generator(), x5, x22), 0x8b1600a5, "add x5, x5, x22"},
      {IGen::sub_gpr64_gpr64(tester.generator(), x5, x22), 0xcb1600a5, "sub x5, x5, x22"},
  };

  for (auto& c : cases) {
    tester.clear();
    tester.emit(c.instr);
    ASSERT_EQ(tester.size(), 4) << c.asm_text;
    EXPECT_EQ(tester.read<u32>(0), c.expected) << c.asm_text;
  }
  tester.clear();
}

TEST(ARM64EmitterCompare, cmp_encodings) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  struct {
    Register a, b;
    u32 expected;
  } cases[] = {
      {Register(X1), Register(X2), 0xeb02003f},  {Register(X0), Register(X0), 0xeb00001f},
      {Register(X9), Register(X21), 0xeb15013f}, {Register(X28), Register(X7), 0xeb07039f},
      {Register(V1), Register(V2), 0x1e222020},  {Register(V13), Register(V14), 0x1e2e21a0},
  };

  for (auto& c : cases) {
    tester.clear();
    if (c.a.is_gpr(InstructionSet::ARM64)) {
      tester.emit(IGen::cmp_gpr64_gpr64(tester.generator(), c.a, c.b));
    } else {
      tester.emit(IGen::cmp_f32_f32(tester.generator(), c.a, c.b));
    }
    EXPECT_EQ(tester.read<u32>(0), c.expected);
  }
  tester.clear();
}

TEST(ARM64ObjectGenerator, branch_patching) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  ObjectGenerator gen(GameVersion::Jak1, InstructionSet::ARM64);
  FunctionDebugInfo debug;
  auto func = gen.add_function_to_seg(MAIN_SEGMENT, &debug);

  auto ir0 = gen.add_ir(func);
  auto fwd = gen.add_instr(IGen::jmp_imm(gen), ir0);
  gen.link_instruction_jump(fwd, gen.get_future_ir_record_in_same_func(ir0, 2));

  auto ir1 = gen.add_ir(func);
  gen.add_instr(IGen::mov_gpr64_gpr64(gen, Register(X0), Register(X1)), ir1);

  auto ir2 = gen.add_ir(func);
  auto back = gen.add_instr(IGen::je_imm(gen), ir2);
  gen.link_instruction_jump(back, gen.get_future_ir_record_in_same_func(ir2, 0));
  gen.add_instr(IGen::ret(gen), ir2);

  auto obj = gen.generate_data_v3(&ts);

  // forward skips one instruction, conditional jumps back to the start
  std::vector<u32> expected = {0x14000002, 0xaa0103e0, 0x54ffffc0, 0xd65f03c0};
  ASSERT_EQ(debug.generated_code.size(), expected.size() * 4);
  for (size_t i = 0; i < expected.size(); i++) {
    u32 word;
    memcpy(&word, debug.generated_code.data() + i * 4, 4);
    EXPECT_EQ(word, expected[i]) << "word " << i;
  }
}

TEST(ARM64ObjectGenerator, symbol_link) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  ObjectGenerator gen(GameVersion::Jak1, InstructionSet::ARM64);
  const auto& info = reg_info(InstructionSet::ARM64);
  FunctionDebugInfo debug;
  auto func = gen.add_function_to_seg(MAIN_SEGMENT, &debug);

  auto ir0 = gen.add_ir(func);
  auto mov = gen.add_instr(IGen::ARM64::mov_gpr32_link_imm32(X16, LINK_SYM_NO_OFFSET_FLAG), ir0);
  gen.link_instruction_symbol_mem(mov, "*foo*");
  gen.add_instr(IGen::ARM64::add_gpr64_gpr64_sxtw(X16, info.get_st_reg(), X16), ir0);
  gen.add_instr(IGen::load32u_gpr64_gpr64_plus_gpr64(gen, Register(X3), info.get_offset_reg(), X16),
                ir0);

  auto obj = gen.generate_data_v3(&ts);

  // use both halves of the movz/movk pair
  std::vector<u32> expected = {0x5297ddf0, 0x72a175b0, 0x8b30c2b0, 0xb870eac3};
  ASSERT_EQ(debug.generated_code.size(), expected.size() * 4);
  for (size_t i = 0; i < expected.size(); i++) {
    u32 word;
    memcpy(&word, debug.generated_code.data() + i * 4, 4);
    EXPECT_EQ(word, expected[i]) << "word " << i;
  }

  const auto& link = obj.link_tables.at(MAIN_SEGMENT);
  ASSERT_GE(link.size(), 1u);
  EXPECT_EQ(link.at(0), LINK_ARM64_SYMBOL_MOV32);
  EXPECT_EQ(std::string((const char*)link.data() + 1), "*foo*");
  u32 count, patch_loc;
  memcpy(&count, link.data() + 1 + 6, 4);
  memcpy(&patch_loc, link.data() + 1 + 6 + 4, 4);
  EXPECT_EQ(count, 1u);
  EXPECT_EQ(patch_loc, debug.offset_in_seg);

  // round trip through the runtime linker
  auto seg = obj.segment_data.at(MAIN_SEGMENT);
  EXPECT_EQ(arm64_read_mov32((u32*)(seg.data() + patch_loc)), LINK_SYM_NO_OFFSET_FLAG);
  arm64_write_mov32((u32*)(seg.data() + patch_loc), u32(-0x1234));
  EXPECT_EQ(arm64_read_mov32((u32*)(seg.data() + patch_loc)), u32(-0x1234));
}

TEST(ARM64ObjectGenerator, other_segment_link) {
  TypeSystem ts;
  ts.add_builtin_types(GameVersion::Jak1);

  ObjectGenerator gen(GameVersion::Jak1, InstructionSet::ARM64);
  FunctionDebugInfo caller_debug, callee_debug;
  auto caller = gen.add_function_to_seg(MAIN_SEGMENT, &caller_debug);
  auto callee = gen.add_function_to_seg(DEBUG_SEGMENT, &callee_debug);

  auto ir0 = gen.add_ir(caller);
  auto mov = gen.add_instr(IGen::ARM64::mov_gpr32_link_imm32(Register(X0), 0), ir0);
  gen.link_instruction_to_function(mov, callee);
  gen.add_instr(IGen::ret(gen), ir0);

  auto ir1 = gen.add_ir(callee);
  gen.add_instr(IGen::ret(gen), ir1);

  auto obj = gen.generate_data_v3(&ts);

  std::vector<u32> expected = {0x52800000, 0x72a00000, 0xd65f03c0};
  ASSERT_EQ(caller_debug.generated_code.size(), expected.size() * 4);
  for (size_t i = 0; i < expected.size(); i++) {
    u32 word;
    memcpy(&word, caller_debug.generated_code.data() + i * 4, 4);
    EXPECT_EQ(word, expected[i]) << "word " << i;
  }

  // skip the type pointer links
  const auto& link = obj.link_tables.at(MAIN_SEGMENT);
  size_t at = 0;
  while (at < link.size() && link[at] != LINK_ARM64_OTHER_SEG_MOV32) {
    at++;
  }
  ASSERT_LT(at, link.size()) << "ARM64 cross-segment link is missing";
  EXPECT_EQ(link.at(at + 1), DEBUG_SEGMENT);
  u32 target, patch_loc;
  memcpy(&target, link.data() + at + 2, 4);
  memcpy(&patch_loc, link.data() + at + 6, 4);
  EXPECT_EQ(target, callee_debug.offset_in_seg);
  EXPECT_EQ(patch_loc, caller_debug.offset_in_seg);
}

TEST(ARM64RegisterAllocator, register_classes) {
  AllocationInput in;
  in.instr_set = InstructionSet::ARM64;
  in.max_vars = 2;
  in.function_name = "arm64-regalloc-test";

  IRegister gpr{RegClass::GPR_64, 0};
  IRegister flt{RegClass::FLOAT, 1};
  IRegister vf{RegClass::VECTOR_FLOAT, 2};
  in.max_vars = 3;

  RegAllocInstr write;
  write.write = {gpr, flt, vf};
  in.add_instruction(write);

  RegAllocInstr read;
  read.read = {gpr, flt, vf};
  read.fallthrough = false;
  in.add_instruction(read);

  const auto& info = reg_info(InstructionSet::ARM64);
  for (bool use_v2 : {false, true}) {
    auto result = use_v2 ? allocate_registers_v2(in) : allocate_registers(in);
    ASSERT_TRUE(result.ok) << (use_v2 ? "v2" : "v1");

    auto check = [&info, use_v2](Register r, bool want_simd, const char* what) {
      EXPECT_EQ(r.is_128bit_simd(InstructionSet::ARM64), want_simd)
          << (use_v2 ? "v2 " : "v1 ") << what << " has id " << r.id();
      EXPECT_EQ(r.is_gpr(InstructionSet::ARM64), !want_simd)
          << (use_v2 ? "v2 " : "v1 ") << what << " has id " << r.id();
      EXPECT_FALSE(info.get_info(r).special)
          << (use_v2 ? "v2 " : "v1 ") << what << " is a special register";
    };
    check(result.ass_as_ranges.at(0).get(0).reg, false, "gpr");
    check(result.ass_as_ranges.at(1).get(0).reg, true, "float");
    check(result.ass_as_ranges.at(2).get(0).reg, true, "vector float");
  }
}

TEST(ARM64RegisterAllocator, uses_high_simd_registers) {
  AllocationInput in;
  in.instr_set = InstructionSet::ARM64;
  in.max_vars = 17;
  in.function_name = "arm64-high-simd-reg-test";

  RegAllocInstr write;
  RegAllocInstr read;
  for (int id = 0; id < in.max_vars; id++) {
    IRegister reg{RegClass::FLOAT, id};
    write.write.push_back(reg);
    read.read.push_back(reg);
  }
  in.add_instruction(write);
  read.fallthrough = false;
  in.add_instruction(read);

  for (bool use_v2 : {false, true}) {
    const auto result = use_v2 ? allocate_registers_v2(in) : allocate_registers(in);
    ASSERT_TRUE(result.ok) << (use_v2 ? "v2" : "v1");

    bool used_high_reg = false;
    for (int id = 0; id < in.max_vars; id++) {
      const auto& assignment = result.ass_as_ranges.at(id).get(0);
      ASSERT_EQ(assignment.kind, Assignment::Kind::REGISTER);
      EXPECT_NE(assignment.reg, Register(V16));
      used_high_reg |= assignment.reg.id() >= V17;
    }
    EXPECT_TRUE(used_high_reg) << (use_v2 ? "v2" : "v1");
  }
}

TEST(ARM64RegisterAllocator, function_calls_use_preserved_width) {
  RegVal function({RegClass::GPR_64, 0}, TypeSpec("function"));
  RegVal result({RegClass::GPR_64, 1}, TypeSpec("object"));

  IR_FunctionCall arm_call(&function, &result, {}, {}, std::nullopt);
  auto arm_rai = arm_call.to_rai();
  ASSERT_TRUE(arm_rai.is_call);
  const auto& arm_info = reg_info(InstructionSet::ARM64);
  for (int id = V8; id <= V15; id++) {
    Register reg(id);
    EXPECT_EQ(arm_info.get_info(reg).call_preserved_bytes, 4);
    EXPECT_TRUE(arm_info.is_preserved_across_call(reg, RegClass::FLOAT));
    EXPECT_FALSE(arm_info.is_preserved_across_call(reg, RegClass::VECTOR_FLOAT));
    EXPECT_FALSE(arm_info.is_preserved_across_call(reg, RegClass::INT_128));
    EXPECT_FALSE(arm_rai.clobbers(reg, RegClass::FLOAT, InstructionSet::ARM64));
    EXPECT_TRUE(arm_rai.clobbers(reg, RegClass::VECTOR_FLOAT, InstructionSet::ARM64));
    EXPECT_TRUE(arm_rai.clobbers(reg, RegClass::INT_128, InstructionSet::ARM64));
  }
  EXPECT_TRUE(arm_rai.clobbers(Register(V7), RegClass::FLOAT, InstructionSet::ARM64));
  EXPECT_TRUE(arm_rai.clobbers(Register(V17), RegClass::FLOAT, InstructionSet::ARM64));
  EXPECT_TRUE(arm_rai.clobbers(Register(V31), RegClass::FLOAT, InstructionSet::ARM64));
  EXPECT_FALSE(arm_rai.clobbers(Register(X19), RegClass::GPR_64, InstructionSet::ARM64));
  EXPECT_TRUE(arm_rai.clobbers(Register(X0), RegClass::GPR_64, InstructionSet::ARM64));

  IR_FunctionCall x86_call(&function, &result, {}, {}, std::nullopt);
  auto x86_rai = x86_call.to_rai();
  ASSERT_TRUE(x86_rai.is_call);
  EXPECT_FALSE(x86_rai.clobbers(Register(XMM8), RegClass::VECTOR_FLOAT, InstructionSet::X86));
  EXPECT_TRUE(x86_rai.clobbers(Register(XMM7), RegClass::VECTOR_FLOAT, InstructionSet::X86));
}

namespace {
RegAllocInstr arm64_call_for_regalloc_test() {
  RegAllocInstr call;
  call.is_call = true;
  return call;
}
}  // namespace

TEST(ARM64RegisterAllocator, live_across_call_uses_compatible_storage) {
  AllocationInput in;
  in.instr_set = InstructionSet::ARM64;
  in.max_vars = 4;
  in.function_name = "arm64-saved-reg-test";

  IRegister gpr{RegClass::GPR_64, 0};
  IRegister flt{RegClass::FLOAT, 1};
  IRegister vf{RegClass::VECTOR_FLOAT, 2};
  IRegister i128{RegClass::INT_128, 3};

  RegAllocInstr write;
  write.write = {gpr, flt, vf, i128};
  in.add_instruction(write);

  in.add_instruction(arm64_call_for_regalloc_test());

  RegAllocInstr read;
  read.read = {gpr, flt, vf, i128};
  read.fallthrough = false;
  in.add_instruction(read);

  const auto& info = reg_info(InstructionSet::ARM64);
  for (bool use_v2 : {false, true}) {
    auto result = use_v2 ? allocate_registers_v2(in) : allocate_registers(in);
    ASSERT_TRUE(result.ok) << (use_v2 ? "v2" : "v1");

    for (int var : {gpr.id, flt.id}) {
      EXPECT_EQ(result.ass_as_ranges.at(var).get(1).kind, Assignment::Kind::REGISTER)
          << (use_v2 ? "v2" : "v1");
      Register reg = result.ass_as_ranges.at(var).get(1).reg;
      EXPECT_TRUE(info.get_info(reg).saved) << (use_v2 ? "v2 " : "v1 ") << info.get_info(reg).name;
      EXPECT_NE(std::find(result.used_saved_regs.begin(), result.used_saved_regs.end(), reg),
                result.used_saved_regs.end())
          << (use_v2 ? "v2 " : "v1 ") << info.get_info(reg).name;
    }

    for (int var : {vf.id, i128.id}) {
      const auto& assignment = result.ass_as_ranges.at(var).get(1);
      EXPECT_EQ(assignment.kind, Assignment::Kind::STACK) << (use_v2 ? "v2" : "v1");
      EXPECT_EQ(assignment.stack_slot & 1, 0) << (use_v2 ? "v2" : "v1");
    }
  }
}

TEST(ARM64RegisterAllocator, reuses_disjoint_spill_slots) {
  auto make_input = [](RegClass reg_class, bool overlap) {
    AllocationInput in;
    in.instr_set = InstructionSet::ARM64;
    in.max_vars = 2;
    in.function_name = "spill-slot-reuse-test";

    IRegister first{reg_class, 0};
    IRegister second{reg_class, 1};

    RegAllocInstr write_first;
    write_first.write = {first};
    in.add_instruction(write_first);

    if (overlap) {
      RegAllocInstr write_second;
      write_second.write = {second};
      in.add_instruction(write_second);
      in.add_instruction(arm64_call_for_regalloc_test());

      RegAllocInstr read_both;
      read_both.read = {first, second};
      read_both.fallthrough = false;
      in.add_instruction(read_both);
    } else {
      in.add_instruction(arm64_call_for_regalloc_test());

      RegAllocInstr read_first;
      read_first.read = {first};
      in.add_instruction(read_first);

      RegAllocInstr write_second;
      write_second.write = {second};
      in.add_instruction(write_second);
      in.add_instruction(arm64_call_for_regalloc_test());

      RegAllocInstr read_second;
      read_second.read = {second};
      read_second.fallthrough = false;
      in.add_instruction(read_second);
    }
    return in;
  };

  for (auto reg_class : {RegClass::INT_128, RegClass::VECTOR_FLOAT}) {
    auto disjoint = allocate_registers_v2(make_input(reg_class, false));
    ASSERT_TRUE(disjoint.ok);
    EXPECT_EQ(disjoint.stack_slots_for_spills, 2);
    ASSERT_EQ(disjoint.ass_as_ranges.at(0).get(1).kind, Assignment::Kind::STACK);
    ASSERT_EQ(disjoint.ass_as_ranges.at(1).get(4).kind, Assignment::Kind::STACK);
    EXPECT_EQ(disjoint.ass_as_ranges.at(0).get(1).stack_slot,
              disjoint.ass_as_ranges.at(1).get(4).stack_slot);

    auto overlapping = allocate_registers_v2(make_input(reg_class, true));
    ASSERT_TRUE(overlapping.ok);
    EXPECT_EQ(overlapping.stack_slots_for_spills, 4);
    ASSERT_EQ(overlapping.ass_as_ranges.at(0).get(2).kind, Assignment::Kind::STACK);
    ASSERT_EQ(overlapping.ass_as_ranges.at(1).get(2).kind, Assignment::Kind::STACK);
    EXPECT_NE(overlapping.ass_as_ranges.at(0).get(2).stack_slot,
              overlapping.ass_as_ranges.at(1).get(2).stack_slot);
  }

  AllocationInput touching;
  touching.instr_set = InstructionSet::ARM64;
  touching.max_vars = 2;
  touching.function_name = "touching-spill-slot-test";
  IRegister first{RegClass::INT_128, 0};
  IRegister second{RegClass::INT_128, 1};
  RegAllocInstr write_first;
  write_first.write = {first};
  touching.add_instruction(write_first);
  touching.add_instruction(arm64_call_for_regalloc_test());
  RegAllocInstr handoff;
  handoff.read = {first};
  handoff.write = {second};
  touching.add_instruction(handoff);
  touching.add_instruction(arm64_call_for_regalloc_test());
  RegAllocInstr read_second;
  read_second.read = {second};
  read_second.fallthrough = false;
  touching.add_instruction(read_second);
  auto touching_result = allocate_registers_v2(touching);
  ASSERT_TRUE(touching_result.ok);
  EXPECT_EQ(touching_result.stack_slots_for_spills, 4);
  EXPECT_NE(touching_result.ass_as_ranges.at(0).get(1).stack_slot,
            touching_result.ass_as_ranges.at(1).get(3).stack_slot);

  auto address_taken = make_input(RegClass::INT_128, false);
  address_taken.force_on_stack_regs = {0, 1};
  auto address_taken_result = allocate_registers_v2(address_taken);
  ASSERT_TRUE(address_taken_result.ok);
  EXPECT_EQ(address_taken_result.stack_slots_for_spills, 4);
  ASSERT_EQ(address_taken_result.ass_as_ranges.at(0).get(1).kind, Assignment::Kind::STACK);
  ASSERT_EQ(address_taken_result.ass_as_ranges.at(1).get(4).kind, Assignment::Kind::STACK);
  EXPECT_NE(address_taken_result.ass_as_ranges.at(0).get(1).stack_slot,
            address_taken_result.ass_as_ranges.at(1).get(4).stack_slot);

  address_taken.instr_set = InstructionSet::X86;
  auto x86 = allocate_registers_v2(address_taken);
  ASSERT_TRUE(x86.ok);
  EXPECT_EQ(x86.stack_slots_for_spills, 4);
  ASSERT_EQ(x86.ass_as_ranges.at(0).get(1).kind, Assignment::Kind::STACK);
  ASSERT_EQ(x86.ass_as_ranges.at(1).get(4).kind, Assignment::Kind::STACK);
  EXPECT_NE(x86.ass_as_ranges.at(0).get(1).stack_slot, x86.ass_as_ranges.at(1).get(4).stack_slot);
}

TEST(ARM64RegisterAllocator, full_width_call_arguments_and_returns) {
  AllocationInput in;
  in.instr_set = InstructionSet::ARM64;
  in.max_vars = 2;
  in.function_name = "arm64-vector-call-boundary-test";

  IRegister arg{RegClass::VECTOR_FLOAT, 0};
  IRegister ret{RegClass::VECTOR_FLOAT, 1};

  RegAllocInstr write;
  write.write = {arg};
  in.add_instruction(write);

  auto call = arm64_call_for_regalloc_test();
  call.read = {arg};
  call.write = {ret};
  in.add_instruction(call);

  RegAllocInstr read;
  read.read = {ret};
  read.fallthrough = false;
  in.add_instruction(read);

  in.constraints.push_back({arg, 1, false, Register(V8)});
  in.constraints.push_back({ret, 1, false, Register(V9)});

  for (bool use_v2 : {false, true}) {
    auto result = use_v2 ? allocate_registers_v2(in) : allocate_registers(in);
    ASSERT_TRUE(result.ok) << (use_v2 ? "v2" : "v1");
    EXPECT_EQ(result.ass_as_ranges.at(arg.id).get(1).reg, Register(V8));
    EXPECT_EQ(result.ass_as_ranges.at(ret.id).get(1).reg, Register(V9));
  }
}

TEST(ARM64RegisterAllocator, saved_simd_constraints_match_value_width) {
  for (auto reg_class : {RegClass::FLOAT, RegClass::VECTOR_FLOAT, RegClass::INT_128}) {
    AllocationInput in;
    in.instr_set = InstructionSet::ARM64;
    in.max_vars = 1;
    in.function_name = "arm64-saved-simd-constraint-test";

    IRegister value{reg_class, 0};
    RegAllocInstr write;
    write.write = {value};
    in.add_instruction(write);
    in.add_instruction(arm64_call_for_regalloc_test());
    RegAllocInstr read;
    read.read = {value};
    read.fallthrough = false;
    in.add_instruction(read);
    in.constraints.push_back({value, 0, true, Register(V8)});

    for (bool use_v2 : {false, true}) {
      auto result = use_v2 ? allocate_registers_v2(in) : allocate_registers(in);
      const bool scalar = reg_class == RegClass::FLOAT;
      EXPECT_EQ(result.ok, scalar) << (use_v2 ? "v2" : "v1") << " class " << int(reg_class);
    }
  }
}

TEST(ARM64RegisterInfo, role_and_return_registers) {
  const auto& info = reg_info(InstructionSet::ARM64);
  EXPECT_EQ(info.get_process_reg(), Register(X20));
  EXPECT_EQ(info.get_st_reg(), Register(X21));
  EXPECT_EQ(info.get_offset_reg(), Register(X22));
  EXPECT_EQ(info.get_exec_base_reg(), Register(X27));
  EXPECT_EQ(info.get_gpr_ret_reg(), Register(X0));
  EXPECT_EQ(info.get_simd_ret_reg(), Register(V0));
}

TEST(ARM64RegisterInfo, gpr_and_vector_ids) {
  EXPECT_NE(Register(X0), Register(V0));
  EXPECT_TRUE(Register(X0).is_gpr(InstructionSet::ARM64));
  EXPECT_FALSE(Register(X0).is_128bit_simd(InstructionSet::ARM64));
  EXPECT_TRUE(Register(V0).is_128bit_simd(InstructionSet::ARM64));
  EXPECT_FALSE(Register(V0).is_gpr(InstructionSet::ARM64));
}

TEST(ARM64RegisterInfo, allocation_orders) {
  auto& info = const_cast<RegisterInfo&>(reg_info(InstructionSet::ARM64));
  for (auto r : info.get_gpr_alloc_order()) {
    EXPECT_FALSE(info.get_info(r).special) << info.get_info(r).name << " is special";
  }
  for (auto r : info.get_simd_alloc_order()) {
    EXPECT_FALSE(info.get_info(r).special) << info.get_info(r).name << " is special";
  }
  EXPECT_EQ(info.get_simd_alloc_order().size(), 31);
  for (int id = V0; id <= V31; id++) {
    const Register reg(id);
    const bool allocatable =
        std::find(info.get_simd_alloc_order().begin(), info.get_simd_alloc_order().end(), reg) !=
        info.get_simd_alloc_order().end();
    EXPECT_EQ(allocatable, reg != Register(V16)) << info.get_info(reg).name;
  }
  for (auto r : info.get_gpr_spill_alloc_order()) {
    EXPECT_FALSE(info.get_info(r).special) << info.get_info(r).name << " is special";
  }
  // keep x16 through x18 out of allocation
  EXPECT_TRUE(info.get_info(Register(X16)).special);
  EXPECT_TRUE(info.get_info(Register(X17)).special);
  EXPECT_TRUE(info.get_info(Register(X18)).special);
  EXPECT_TRUE(info.get_info(Register(X20)).special);
  EXPECT_TRUE(info.get_info(Register(X21)).special);
  EXPECT_TRUE(info.get_info(Register(X22)).special);
  EXPECT_TRUE(info.get_info(Register(X27)).special);
  EXPECT_TRUE(info.get_info(Register(X28)).special);
  EXPECT_TRUE(info.get_info(Register(X29)).special);
  EXPECT_TRUE(info.get_info(Register(X30)).special);
  EXPECT_TRUE(info.get_info(Register(V16)).special);
}

TEST(ARM64RegisterInfo, temporary_allocation_orders) {
  auto& info = const_cast<RegisterInfo&>(reg_info(InstructionSet::ARM64));
  for (auto r : info.get_gpr_temp_alloc_order()) {
    EXPECT_FALSE(info.get_info(r).saved) << info.get_info(r).name << " is callee-saved";
  }
  for (auto r : info.get_simd_temp_alloc_order()) {
    EXPECT_FALSE(info.get_info(r).saved) << info.get_info(r).name << " is callee-saved";
    EXPECT_FALSE(info.get_info(r).special) << info.get_info(r).name << " is special";
  }
  EXPECT_EQ(info.get_simd_temp_alloc_order().size(), 23);
}

TEST(ARM64CodeTester, saves_all_simd_registers) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  EXPECT_EQ(tester.get_simd_reg_count(), 32);

  tester.emit_push_all_simd();
  tester.emit_pop_all_simd();

  EXPECT_EQ(tester.size(), 512);
  EXPECT_EQ(tester.dump_to_asm_string(),
            "\033[2m<push all SIMDs>\033[0m\n\033[2m<pop all SIMDs>\033[0m\n");
}

TEST(ARM64RegisterInfo, saved_allocation_orders) {
  for (auto target_set : {InstructionSet::ARM64, InstructionSet::X86}) {
    const auto& info = reg_info(target_set);
    auto all_saved = info.get_all_saved();
    auto is_in_saved_list = [&all_saved](Register r) {
      return std::find(all_saved.begin(), all_saved.end(), r) != all_saved.end();
    };
    for (int id = 0; id < RegisterInfo::N_REGS; id++) {
      Register r(id);
      if (info.get_info(r).saved) {
        EXPECT_TRUE(is_in_saved_list(r))
            << info.get_info(r).name << " is missing from the prologue save list";
      }
    }
    for (auto r : info.get_gpr_alloc_order()) {
      if (info.get_info(r).saved) {
        EXPECT_TRUE(is_in_saved_list(r))
            << info.get_info(r).name << " is missing from the prologue save list";
      }
    }
    for (auto r : info.get_simd_alloc_order()) {
      if (info.get_info(r).saved) {
        EXPECT_TRUE(is_in_saved_list(r))
            << info.get_info(r).name << " is missing from the prologue save list";
      }
    }
  }
}

TEST(ARM64EmitterFloat32, div) {
  auto tester = create_tester();
  std::vector<float> vals = {1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for_each_register_except(tester, {}, [&](Register i) {
        for_each_register_except(tester, {i}, [&](Register j) {
          auto expected = g / f;
          tester.clear();
          tester.emit_push_all_simd();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(), X0));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j.id(), X0));
          tester.emit(IGen::div_f32_f32(tester.generator(), V0 + j.id(), V0 + i.id()));
          tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + j.id()));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_simd();
          tester.emit_return();

          EXPECT_EXECUTE_RET_4ARG_EQ(tester, 0, 0, 0, 0, expected);
        });
      });
    }
  }
}

TEST(ARM64EmitterFloat32, add) {
  auto tester = create_tester();
  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};
  for (auto f : vals) {
    for (auto g : vals) {
      for_each_register_except(tester, {}, [&](Register i) {
        for_each_register_except(tester, {i}, [&](Register j) {
          auto expected = g + f;
          tester.clear();
          tester.emit_push_all_simd();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(), X0));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j.id(), X0));
          tester.emit(IGen::add_f32_f32(tester.generator(), V0 + j.id(), V0 + i.id()));
          tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + j.id()));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_simd();
          tester.emit_return();

          EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ(tester, 0, 0, 0, 0, expected);
        });
      });
    }
  }
}

TEST(ARM64EmitterFloat32, sub) {
  auto tester = create_tester();
  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for_each_register_except(tester, {}, [&](Register i) {
        for_each_register_except(tester, {i}, [&](Register j) {
          auto expected = g - f;
          tester.clear();
          tester.emit_push_all_simd();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(), X0));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
          tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + j.id(), X0));
          tester.emit(IGen::sub_f32_f32(tester.generator(), V0 + j.id(), V0 + i.id()));
          tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + j.id()));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_simd();
          tester.emit_return();

          EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ(tester, 0, 0, 0, 0, expected);
        });
      });
    }
  }
}

TEST(ARM64EmitterFloat32, float_to_int) {
  auto tester = create_tester();
  std::vector<float> vals = {0.f,    1.f,  0.2f, -1.f,  1235423.2f, -3457343.3f,
                             7.545f, 0.1f, 0.9f, -0.1f, -0.9f};

  for (auto g : vals) {
    for_each_register_except(tester, {}, [&](Register i) {
      for_each_gpr_except(tester, {X0, i}, [&](Register j) {
        s32 expected = g;
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        u64 val = 0;
        memcpy(&val, &g, sizeof(float));
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), X0, val));
        tester.emit(IGen::movd_f32_gpr32(tester.generator(), V0 + i.id(), X0));
        tester.emit(IGen::f32_to_int32(tester.generator(), j, V0 + i.id()));
        tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), X0, j));
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        EXPECT_EXECUTE_RET_4ARG_EQ(tester, 0, 0, 0, 0, expected);
      });
    });
  }
}

TEST(ARM64EmitterFloat32, int_to_float) {
  auto tester = create_tester();
  std::vector<s64> vals = {0, 1, -1, INT32_MAX, -3457343, 7, INT32_MIN};

  for (auto g : vals) {
    for_each_register_except(tester, {}, [&](Register i) {
      for_each_gpr_except(tester, {i}, [&](Register j) {
        float expected = g;
        tester.clear();
        tester.emit_push_all_simd();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(tester.generator(), j, g));
        tester.emit(IGen::int32_to_f32(tester.generator(), V0 + i.id(), j));
        tester.emit(IGen::movd_gpr32_f32(tester.generator(), X0, V0 + i.id()));
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_simd();
        tester.emit_return();

        EXPECT_EXECUTE_RET_4ARG_FLOAT_EQ(tester, 0, 0, 0, 0, expected);
      });
    });
  }
}

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
//             tester.emit(IGen::mov_gpr64_gpr64(tester.generator(), X0, r2));
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

TEST(ARM64RegisterInfo, saved_register_list) {
  auto& info = const_cast<RegisterInfo&>(reg_info(InstructionSet::ARM64));
  for (auto r : info.get_all_saved()) {
    EXPECT_TRUE(info.get_info(r).saved) << info.get_info(r).name << " is not marked as saved";
    EXPECT_FALSE(info.get_info(r).special) << info.get_info(r).name << " is special";
  }
}

TEST(ARM64RegisterInfo, role_and_argument_registers) {
  const auto& x86 = reg_info(InstructionSet::X86);
  const auto& arm = reg_info(InstructionSet::ARM64);

  EXPECT_EQ(x86.get_process_reg(), Register(R13));
  EXPECT_EQ(arm.get_process_reg(), Register(X20));
  EXPECT_EQ(x86.get_st_reg(), Register(R14));
  EXPECT_EQ(arm.get_st_reg(), Register(X21));
  EXPECT_EQ(x86.get_offset_reg(), Register(R15));
  EXPECT_EQ(arm.get_offset_reg(), Register(X22));
  EXPECT_EQ(x86.get_stack_reg(), Register(RSP));
  EXPECT_EQ(arm.get_stack_reg(), Register(SP));

  const Register x86_cargs[] = {Register(RDI), Register(RSI), Register(RDX), Register(RCX)};
  const Register arm_cargs[] = {Register(X0), Register(X1), Register(X2), Register(X3)};
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(x86.get_gpr_arg_reg(i), x86_cargs[i]) << "carg" << i;
    EXPECT_EQ(arm.get_gpr_arg_reg(i), arm_cargs[i]) << "carg" << i;
  }
}

TEST(ARM64RegisterInfo, x86_role_and_return_registers) {
  const auto& info = reg_info(InstructionSet::X86);
  EXPECT_EQ(info.get_process_reg(), Register(R13));
  EXPECT_EQ(info.get_st_reg(), Register(R14));
  EXPECT_EQ(info.get_offset_reg(), Register(R15));
  EXPECT_EQ(info.get_gpr_ret_reg(), Register(RAX));
  EXPECT_EQ(info.get_simd_ret_reg(), Register(XMM0));
  for (int id = XMM15 + 1; id < RegisterInfo::N_REGS; id++) {
    EXPECT_TRUE(info.get_info(Register(id)).special);
  }
}

TEST(ARM64EmitterIntegerMath, immediate_shift_boundaries) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  const Register x9(X9);
  struct {
    u8 sa;
    u32 shl, shr, sar;
  } cases[] = {
      {0, 0xd340fd29, 0xd340fd29, 0x9340fd29},  {1, 0xd37ff929, 0xd341fd29, 0x9341fd29},
      {31, 0xd3618129, 0xd35ffd29, 0x935ffd29}, {32, 0xd3607d29, 0xd360fd29, 0x9360fd29},
      {62, 0xd3420529, 0xd37efd29, 0x937efd29}, {63, 0xd3410129, 0xd37ffd29, 0x937ffd29},
  };

  for (auto& c : cases) {
    tester.clear();
    tester.emit(IGen::shl_gpr64_u8(tester.generator(), x9, c.sa));
    EXPECT_EQ(tester.read<u32>(0), c.shl) << "lsl x9, x9, #" << int(c.sa);
    tester.clear();
    tester.emit(IGen::shr_gpr64_u8(tester.generator(), x9, c.sa));
    EXPECT_EQ(tester.read<u32>(0), c.shr) << "lsr x9, x9, #" << int(c.sa);
    tester.clear();
    tester.emit(IGen::sar_gpr64_u8(tester.generator(), x9, c.sa));
    EXPECT_EQ(tester.read<u32>(0), c.sar) << "asr x9, x9, #" << int(c.sa);
  }
  tester.clear();
}

TEST(ARM64EmitterVF, vector_shift_amounts) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  const Register v1(V1), v2(V2);
  struct {
    Instruction instr;
    u32 expected;
    const char* asm_text;
  } cases[] = {
      // 32-bit right shifts use 1 through 32
      {IGen::pw_sra(tester.generator(), v1, v2, 1), 0x4f3f0441, "sshr.4s v1, v2, #1"},
      {IGen::pw_sra(tester.generator(), v1, v2, 6), 0x4f3a0441, "sshr.4s v1, v2, #6"},
      {IGen::pw_sra(tester.generator(), v1, v2, 10), 0x4f360441, "sshr.4s v1, v2, #10"},
      {IGen::pw_sra(tester.generator(), v1, v2, 16), 0x4f300441, "sshr.4s v1, v2, #16"},
      {IGen::pw_sra(tester.generator(), v1, v2, 31), 0x4f210441, "sshr.4s v1, v2, #31"},
      {IGen::pw_sra(tester.generator(), v1, v2, 32), 0x4f200441, "sshr.4s v1, v2, #32"},
      {IGen::pw_srl(tester.generator(), v1, v2, 1), 0x6f3f0441, "ushr.4s v1, v2, #1"},
      {IGen::pw_srl(tester.generator(), v1, v2, 6), 0x6f3a0441, "ushr.4s v1, v2, #6"},
      {IGen::pw_srl(tester.generator(), v1, v2, 32), 0x6f200441, "ushr.4s v1, v2, #32"},
      // 32-bit left shifts use 0 through 31
      {IGen::pw_sll(tester.generator(), v1, v2, 0), 0x4f205441, "shl.4s v1, v2, #0"},
      {IGen::pw_sll(tester.generator(), v1, v2, 6), 0x4f265441, "shl.4s v1, v2, #6"},
      {IGen::pw_sll(tester.generator(), v1, v2, 16), 0x4f305441, "shl.4s v1, v2, #16"},
      {IGen::pw_sll(tester.generator(), v1, v2, 31), 0x4f3f5441, "shl.4s v1, v2, #31"},
      // halfword right shifts use 1 through 16
      // halfword left shifts use 0 through 15
      {IGen::ph_srl(tester.generator(), v1, v2, 1), 0x6f1f0441, "ushr.8h v1, v2, #1"},
      {IGen::ph_srl(tester.generator(), v1, v2, 8), 0x6f180441, "ushr.8h v1, v2, #8"},
      {IGen::ph_srl(tester.generator(), v1, v2, 16), 0x6f100441, "ushr.8h v1, v2, #16"},
      {IGen::ph_sll(tester.generator(), v1, v2, 0), 0x4f105441, "shl.8h v1, v2, #0"},
      {IGen::ph_sll(tester.generator(), v1, v2, 8), 0x4f185441, "shl.8h v1, v2, #8"},
      {IGen::ph_sll(tester.generator(), v1, v2, 15), 0x4f1f5441, "shl.8h v1, v2, #15"},
  };

  for (auto& c : cases) {
    tester.clear();
    tester.emit(c.instr);
    ASSERT_EQ(tester.size(), 4) << c.asm_text;
    EXPECT_EQ(tester.read<u32>(0), c.expected) << c.asm_text;
  }
  tester.clear();
}

TEST(ARM64EmitterVF, vector_stack_offsets) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  const Register sp(SP), x16(X16), v8(V8), v9(V9);
  // zero offset uses the base directly
  tester.clear();
  tester.emit(IGen::store128_simd128_reg_offset(tester.generator(), sp, v8, 0));
  ASSERT_EQ(tester.size(), 4);
  EXPECT_EQ(tester.read<u32>(0), 0x3d8003e8u) << "str q8, [sp]";

  tester.clear();
  tester.emit(IGen::load128_simd128_reg_offset(tester.generator(), v8, sp, 0));
  ASSERT_EQ(tester.size(), 4);
  EXPECT_EQ(tester.read<u32>(0), 0x3dc003e8u) << "ldr q8, [sp]";

  // nonzero offsets go through x16
  tester.clear();
  tester.emit(IGen::store128_simd128_reg_offset(tester.generator(), sp, v9, 16));
  ASSERT_EQ(tester.size(), 12) << "mov x16, sp / add x16, x16, #16 / str q9, [x16]";
  EXPECT_EQ(tester.read<u32>(8), 0x3d800209u) << "str q9, [x16]";

  tester.clear();
  tester.emit(IGen::load128_simd128_reg_offset(tester.generator(), v9, sp, 16));
  ASSERT_EQ(tester.size(), 12) << "mov x16, sp / add x16, x16, #16 / ldr q9, [x16]";
  EXPECT_EQ(tester.read<u32>(8), 0x3dc00209u) << "ldr q9, [x16]";

  tester.clear();
}

TEST(ARM64EmitterVF, sqrt_f32_destination) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  struct {
    Register dst, src;
    u32 expected;
    const char* asm_text;
  } cases[] = {
      {Register(V0), Register(V7), 0x1e21c0e0, "fsqrt s0, s7"},
      {Register(V10), Register(V7), 0x1e21c0ea, "fsqrt s10, s7"},
      {Register(V3), Register(V1), 0x1e21c023, "fsqrt s3, s1"},
  };

  for (auto& c : cases) {
    tester.clear();
    tester.emit(IGen::sqrt_f32(tester.generator(), c.dst, c.src));
    ASSERT_EQ(tester.size(), 4) << c.asm_text;
    EXPECT_EQ(tester.read<u32>(0), c.expected) << c.asm_text;
  }
  tester.clear();
}

TEST(ARM64EmitterVF, indexed_vector_accesses) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  // add the bases before LDUR or STUR
  tester.clear();
  tester.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s8(tester.generator(), Register(V7), Register(X19),
                                                    Register(X22), 124));
  ASSERT_EQ(tester.size(), 8);
  EXPECT_EQ(tester.read<u32>(0), 0x8b160270u) << "add x16, x19, x22";
  EXPECT_EQ(tester.read<u32>(4), 0x3cc7c207u) << "ldur q7, [x16, #124]";

  tester.clear();
  tester.emit(IGen::storevf_gpr64_plus_gpr64_plus_s8(tester.generator(), Register(V7),
                                                     Register(X19), Register(X22), 124));
  ASSERT_EQ(tester.size(), 8);
  EXPECT_EQ(tester.read<u32>(0), 0x8b160270u) << "add x16, x19, x22";
  EXPECT_EQ(tester.read<u32>(4), 0x3c87c207u) << "stur q7, [x16, #124]";

  tester.clear();
  tester.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s8(tester.generator(), Register(V7), Register(X19),
                                                    Register(X22), -8));
  ASSERT_EQ(tester.size(), 8);
  EXPECT_EQ(tester.read<u32>(4), 0x3cdf8207u) << "ldur q7, [x16, #-8]";

  // larger offsets use x16 with a zero LDUR or STUR offset
  tester.clear();
  tester.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s32(tester.generator(), Register(V7),
                                                     Register(X19), Register(X22), 0x10c));
  EXPECT_EQ(tester.read<u32>(0), 0x8b160270u) << "add x16, x19, x22";
  EXPECT_EQ(tester.read<u32>(tester.size() - 4), 0x3cc00207u) << "ldur q7, [x16]";

  tester.clear();
  tester.emit(IGen::storevf_gpr64_plus_gpr64_plus_s32(tester.generator(), Register(V7),
                                                      Register(X19), Register(X22), 0x10c));
  EXPECT_EQ(tester.read<u32>(0), 0x8b160270u) << "add x16, x19, x22";
  EXPECT_EQ(tester.read<u32>(tester.size() - 4), 0x3c800207u) << "stur q7, [x16]";
  tester.clear();
}

TEST(ARM64EmitterTrap, trap) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(256);

  tester.clear();
  tester.emit(IGen::trap(tester.generator()));
  ASSERT_EQ(tester.size(), 4);
  EXPECT_EQ(tester.read<u32>(0), 0xd4200020u) << "brk #1";

  // BRK #0 belongs to the debugger
  EXPECT_NE(tester.read<u32>(0), 0xd4200000u) << "brk #0 is reserved for the debugger";
  tester.clear();
}

TEST(X86EmitterTrap, trap) {
  CodeTester tester(InstructionSet::X86);
  tester.init_code_buffer(256);

  tester.clear();
  tester.emit(IGen::trap(tester.generator()));
  ASSERT_EQ(tester.size(), 2);
  EXPECT_EQ(tester.read<u8>(0), 0x0f);
  EXPECT_EQ(tester.read<u8>(1), 0x0b);
  tester.clear();
}
