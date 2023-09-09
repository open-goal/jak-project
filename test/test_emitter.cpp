#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"

using namespace emitter;

TEST(EmitterIntegerMath, add_gpr64_imm8s) {
  CodeTester tester;
  tester.init_code_buffer(256);

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX};

  // test the ones that aren't rsp
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (auto val : vals) {
      for (auto imm : imms) {
        auto expected = val + imm;

        tester.clear();
        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::add_gpr64_imm8s(i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        auto result = tester.execute_ret<s64>(val, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }

  tester.clear();
  tester.emit(IGen::add_gpr64_imm8s(RSP, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 83 c4 0c");
}

TEST(EmitterIntegerMath, add_gpr64_imm32s) {
  CodeTester tester;
  tester.init_code_buffer(256);

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX, INT32_MIN, INT32_MAX};

  // test the ones that aren't rsp
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (auto val : vals) {
      for (auto imm : imms) {
        auto expected = val + imm;

        tester.clear();
        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::add_gpr64_imm32s(i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        auto result = tester.execute_ret<s64>(val, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }

  tester.clear();
  tester.emit(IGen::add_gpr64_imm32s(RSP, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 81 c4 0c 00 00 00");
}

TEST(EmitterIntegerMath, sub_gpr64_imm8s) {
  CodeTester tester;
  tester.init_code_buffer(256);

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX};

  // test the ones that aren't rsp
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (auto val : vals) {
      for (auto imm : imms) {
        auto expected = val - imm;

        tester.clear();
        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::sub_gpr64_imm8s(i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        auto result = tester.execute_ret<s64>(val, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }

  tester.clear();
  tester.emit(IGen::sub_gpr64_imm8s(RSP, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 83 ec 0c");
}

TEST(EmitterIntegerMath, sub_gpr64_imm32s) {
  CodeTester tester;
  tester.init_code_buffer(256);

  std::vector<s64> vals = {0, 1, -1, INT32_MIN, INT32_MAX, INT64_MIN, INT64_MAX};
  std::vector<s64> imms = {0, 1, -1, INT8_MIN, INT8_MAX, INT32_MIN, INT32_MAX};

  // test the ones that aren't rsp
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (auto val : vals) {
      for (auto imm : imms) {
        auto expected = val - imm;

        tester.clear();
        tester.emit_push_all_gprs(true);

        // move initial value to register
        tester.emit(IGen::mov_gpr64_gpr64(i, tester.get_c_abi_arg_reg(0)));
        // do the add
        tester.emit(IGen::sub_gpr64_imm32s(i, imm));
        // move for return
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));

        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        auto result = tester.execute_ret<s64>(val, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }

  tester.clear();
  tester.emit(IGen::sub_gpr64_imm32s(RSP, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 81 ec 0c 00 00 00");
}

TEST(EmitterIntegerMath, add_gpr64_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          auto expected = v1 + v2;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, v1));
          tester.emit(IGen::mov_gpr64_u64(j, v2));
          tester.emit(IGen::add_gpr64_gpr64(i, j));
          tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, sub_gpr64_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          auto expected = v1 - v2;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, v1));
          tester.emit(IGen::mov_gpr64_u64(j, v2));
          tester.emit(IGen::sub_gpr64_gpr64(i, j));
          tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, mul_gpr32_gpr32) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s32> vals = {
      0, 1, -2, -20, 123123, INT32_MIN, INT32_MAX, INT32_MIN + 1, INT32_MAX - 1};

  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          // this is kind of weird behavior, but it's what the PS2 CPU does, I think.
          // the lower 32-bits of the result are sign extended, even if this sign doesn't match
          // the sign of the real product.  This is true for both signed and unsigned multiply.
          auto expected = ((s64(v1) * s64(v2)) << 32) >> 32;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, (s64)v1));
          tester.emit(IGen::mov_gpr64_u64(j, (s64)v2));
          tester.emit(IGen::imul_gpr32_gpr32(i, j));
          tester.emit(IGen::movsx_r64_r32(RAX, i));  // weird PS2 sign extend.
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, or_gpr64_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          auto expected = v1 | v2;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, v1));
          tester.emit(IGen::mov_gpr64_u64(j, v2));
          tester.emit(IGen::or_gpr64_gpr64(i, j));
          tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, and_gpr64_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          auto expected = v1 & v2;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, v1));
          tester.emit(IGen::mov_gpr64_u64(j, v2));
          tester.emit(IGen::and_gpr64_gpr64(i, j));
          tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, xor_gpr64_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (auto v1 : vals) {
        for (auto v2 : vals) {
          auto expected = v1 ^ v2;
          tester.clear();
          tester.emit_push_all_gprs(true);
          tester.emit(IGen::mov_gpr64_u64(i, v1));
          tester.emit(IGen::mov_gpr64_u64(j, v2));
          tester.emit(IGen::xor_gpr64_gpr64(i, j));
          tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
          tester.emit_pop_all_gprs(true);
          tester.emit_return();
          auto result = tester.execute_ret<s64>(0, 0, 0, 0);
          EXPECT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterIntegerMath, not_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (auto v1 : vals) {
      auto expected = ~v1;
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_u64(i, v1));
      tester.emit(IGen::not_gpr64(i));
      tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      auto result = tester.execute_ret<s64>(0, 0, 0, 0);
      EXPECT_EQ(result, expected);
    }
  }
}

TEST(EmitterIntegerMath, shl_gpr64_cl) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP || i == RCX) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v << sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::mov_gpr64_u64(RCX, sa));
        tester.emit(IGen::shl_gpr64_cl(i));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, shr_gpr64_cl) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<u64> vals = {0,         1,   u64(-2), u64(INT32_MIN), INT32_MAX, u64(INT64_MIN),
                           INT64_MAX, 117, 32,      u64(-348473),   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP || i == RCX) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::mov_gpr64_u64(RCX, sa));
        tester.emit(IGen::shr_gpr64_cl(i));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, sar_gpr64_cl) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP || i == RCX) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::mov_gpr64_u64(RCX, sa));
        tester.emit(IGen::sar_gpr64_cl(i));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, shl_gpr64_u8) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v << sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::shl_gpr64_u8(i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, shr_gpr64_u8) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<u64> vals = {0,         1,   u64(-2), u64(INT32_MIN), INT32_MAX, u64(INT64_MIN),
                           INT64_MAX, 117, 32,      u64(-348473),   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::shr_gpr64_u8(i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, sar_gpr64_u8) {
  CodeTester tester;
  tester.init_code_buffer(256);
  std::vector<s64> vals = {0,         1,   -2, INT32_MIN, INT32_MAX, INT64_MIN,
                           INT64_MAX, 117, 32, -348473,   83747382};
  std::vector<u8> sas = {0, 1, 23, 53, 64};

  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (auto v : vals) {
      for (auto sa : sas) {
        auto expected = v >> sa;
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(i, v));
        tester.emit(IGen::sar_gpr64_u8(i, sa));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        auto result = tester.execute_ret<s64>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterIntegerMath, jumps) {
  CodeTester tester;
  tester.init_code_buffer(256);

  std::vector<int> reads;

  auto x = IGen::jmp_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::je_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jne_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jle_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jge_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jl_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jg_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jbe_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jae_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::jb_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  x = IGen::ja_32();
  reads.push_back(tester.size() + x.offset_of_imm());
  tester.emit(x);

  for (auto off : reads) {
    EXPECT_EQ(0, tester.read<s32>(off));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "E9000000000F84000000000F85000000000F8E000000000F8D000000000F8C000000000F8F000000000F86"
            "000000000F83000000000F82000000000F8700000000");
}

TEST(EmitterIntegerMath, null) {
  auto instr = IGen::null();
  EXPECT_EQ(0, instr.emit(nullptr));
}

TEST(EmitterLoadsAndStores, load_constant_64_and_move_gpr_gpr_64) {
  std::vector<u64> u64_constants = {0, UINT64_MAX, INT64_MAX, 7, 12};

  // test we can load a 64-bit constant into all gprs, move it to any other gpr, and return it.
  // rsp is skipping because that's the stack pointer and would prevent us from popping gprs after

  CodeTester tester;
  tester.init_code_buffer(256);

  for (auto constant : u64_constants) {
    for (int r1 = 0; r1 < 16; r1++) {
      if (r1 == RSP) {
        continue;
      }

      for (int r2 = 0; r2 < 16; r2++) {
        if (r2 == RSP) {
          continue;
        }
        tester.clear();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(r1, constant));
        tester.emit(IGen::mov_gpr64_gpr64(r2, r1));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, r2));
        tester.emit_pop_all_gprs(true);
        tester.emit_return();
        EXPECT_EQ(tester.execute(), constant);
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load_constant_32_unsigned) {
  std::vector<u64> u64_constants = {0, UINT32_MAX, INT32_MAX, 7, 12};

  // test loading 32-bit constants, with all upper 32-bits zero.
  // this uses a different opcode than 64-bit loads.
  CodeTester tester;
  tester.init_code_buffer(256);

  for (auto constant : u64_constants) {
    for (int r1 = 0; r1 < 16; r1++) {
      if (r1 == RSP) {
        continue;
      }

      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_u64(r1, UINT64_MAX));
      tester.emit(IGen::mov_gpr64_u32(r1, constant));
      tester.emit(IGen::mov_gpr64_gpr64(RAX, r1));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute(), constant);
    }
  }
}

TEST(EmitterLoadsAndStores, load_constant_32_signed) {
  std::vector<s32> s32_constants = {0, 1, INT32_MAX, INT32_MIN, 12, -1};

  // test loading signed 32-bit constants.  for values < 0 this will sign extend.
  CodeTester tester;
  tester.init_code_buffer(256);

  for (auto constant : s32_constants) {
    for (int r1 = 0; r1 < 16; r1++) {
      if (r1 == RSP) {
        continue;
      }

      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::mov_gpr64_s32(r1, constant));
      tester.emit(IGen::mov_gpr64_gpr64(RAX, r1));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute(), constant);
    }
  }
}

TEST(EmitterLoadsAndStores, load8s_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f be 04 1e");

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(R12, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 0f be 24 1e");

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(R12, R15, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4e 0f be 24 3e");

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(R12, R15, R14));
  EXPECT_EQ(tester.dump_to_hex_string(), "4f 0f be 24 3e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load8s_gpr64_gpr64_gpr64_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f be 44 1e fd");

  auto instr = IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load8s_gpr64_gpr64_gpr64_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f be 84 1e fd ff ff ff");

  auto instr = IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load8u_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b6 04 1e");

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64(R12, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 0f b6 24 1e");

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64(R12, R15, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4e 0f b6 24 3e");

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64(R12, R15, R14));
  EXPECT_EQ(tester.dump_to_hex_string(), "4f 0f b6 24 3e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8s_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load8u_gpr64_gpr64_gpr64_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b6 44 1e fd");

  auto instr = IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3 + 3, 0, 0)), 0xfe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2 + 3, 0, 0)), 0xfd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), 0xff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load8u_gpr64_gpr64_gpr64_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b6 84 1e fd ff ff ff");

  auto instr = IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load8u_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u8 memory[8] = {0, 0, 0xfd, 0xfe, 0xff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 3 + 3, 0, 0)), 0xfe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 2 + 3, 0, 0)), 0xfd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), 0xff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 5 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16s_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f bf 04 1e");

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(R12, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 0f bf 24 1e");

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(R12, R15, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4e 0f bf 24 3e");

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(R12, R15, R14));
  EXPECT_EQ(tester.dump_to_hex_string(), "4f 0f bf 24 3e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16s_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f bf 44 1e fd");

  auto instr = IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16s_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f bf 84 1e fd ff ff ff");

  auto instr = IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16s_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16u_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b7 04 1e");

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(R12, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 0f b7 24 1e");

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(R12, R15, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "4e 0f b7 24 3e");

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(R12, R15, R14));
  EXPECT_EQ(tester.dump_to_hex_string(), "4f 0f b7 24 3e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6, 0, 0)), 0xfffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4, 0, 0)), 0xfffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8, 0, 0)), 0xffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16u_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b7 44 1e fd");

  auto instr = IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6 + 3, 0, 0)), 0xfffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), 0xfffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), 0xffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load16u_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b7 84 1e fd ff ff ff");

  auto instr = IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load16u_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u16 memory[8] = {0, 0, 0xfffd, 0xfffe, 0xffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 6 + 3, 0, 0)), 0xfffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 4 + 3, 0, 0)), 0xfffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), 0xffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 10 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32s_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 63 04 1e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32s_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 63 44 1e fd");

  auto instr = IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32s_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 63 84 1e fd ff ff ff");

  auto instr = IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32u_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "8b 04 1e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12, 0, 0)), 0xfffffffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8, 0, 0)), 0xfffffffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16, 0, 0)), 0xffffffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32u_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "8b 44 1e fd");

  auto instr = IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12 + 3, 0, 0)), 0xfffffffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), 0xfffffffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), 0xffffffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load32u_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "8b 84 1e fd ff ff ff");

  auto instr = IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        u32 memory[8] = {0, 0, 0xfffffffd, 0xfffffffe, 0xffffffff, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 12 + 3, 0, 0)), 0xfffffffe);
        EXPECT_EQ(s64(tester.execute((u64)memory, 8 + 3, 0, 0)), 0xfffffffd);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), 0xffffffff);
        EXPECT_EQ(s64(tester.execute((u64)memory, 20 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load64_gpr64_goal_ptr_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64(RAX, RBX, RSI));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 8b 04 1e");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64(k, i, j));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 24, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 32, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 40, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load64_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 8b 44 1e fd");

  auto instr = IGen::load64_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s8(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 24 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 32 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 40 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load64_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3));

  EXPECT_EQ(tester.dump_to_hex_string(), "48 8b 84 1e fd ff ff ff");

  auto instr = IGen::load64_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // fill k with junk
        if (k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter & 1) ? 0 : UINT64_MAX));
        }

        // load into k
        tester.emit(IGen::load64_gpr64_gpr64_plus_gpr64_plus_s32(k, i, j, -3));

        // move k to return register
        tester.emit(IGen::mov_gpr64_gpr64(RAX, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, -3, -2, -1, 0, 0, 0};

        // run!
        EXPECT_EQ(s64(tester.execute((u64)memory, 24 + 3, 0, 0)), -2);
        EXPECT_EQ(s64(tester.execute((u64)memory, 16 + 3, 0, 0)), -3);
        EXPECT_EQ(s64(tester.execute((u64)memory, 32 + 3, 0, 0)), -1);
        EXPECT_EQ(s64(tester.execute((u64)memory, 40 + 3, 0, 0)), 0);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64(RAX, RCX, RDX));
  EXPECT_EQ(tester.dump_to_hex_string(), "88 14 01");

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64(i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], 7);
        EXPECT_EQ(memory[4], 1);
        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, RDX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "88 54 01 0c");

  auto instr = IGen::store8_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s8(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 6, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], 7);
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store8_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, RDX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "88 94 01 0c 00 00 00");

  auto instr = IGen::store8_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store8_gpr64_gpr64_plus_gpr64_plus_s32(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s8 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 6, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], 7);
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64(RCX, RAX, R8));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 89 04 08");

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64(i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 6, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s16(0xff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 89 44 01 0c");

  auto instr = IGen::store16_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s8(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 6 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s16(0xff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store16_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 89 84 01 0c 00 00 00");

  auto instr = IGen::store16_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store16_gpr64_gpr64_plus_gpr64_plus_s32(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s16 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 6 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s16(0xff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64(RCX, RAX, R8));
  EXPECT_EQ(tester.dump_to_hex_string(), "44 89 04 08");

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64(i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12, 0xffffffff12341234, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], 0x12341234);
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "44 89 44 01 0c");

  auto instr = IGen::store32_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s8(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s32(0xffffff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store32_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "44 89 84 01 0c 00 00 00");

  auto instr = IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s32 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s32(0xffffff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64(RCX, RAX, R8));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 89 04 08");

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store!
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64(i, j, k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 24, 0xffffffff12341234, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], 0xffffffff12341234);
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 89 44 01 0c");

  auto instr = IGen::store64_gpr64_gpr64_plus_gpr64_plus_s8(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s8(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 24 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s64(0xffffffffffffff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, store64_gpr64_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);

  tester.clear();
  tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, R8, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "4c 89 84 01 0c 00 00 00");

  auto instr = IGen::store64_gpr64_gpr64_plus_gpr64_plus_s32(RAX, RCX, RDX, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s8*)(buff + instr.offset_of_disp()), -3);

  [[maybe_unused]] int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        if (k == RSP || k == j || k == i) {
          continue;
        }

        tester.clear();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1
        tester.emit(IGen::pop_gpr64(k));  // k will have the value to store.

        // store
        tester.emit(IGen::store64_gpr64_gpr64_plus_gpr64_plus_s32(i, j, k, -3));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_return();

        // prepare the memory:
        s64 memory[8] = {0, 0, 3, -2, 1, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 24 + 3, 0xffffffffffffff07, 0);
        EXPECT_EQ(memory[2], 3);
        EXPECT_EQ(memory[3], s64(0xffffffffffffff07));
        EXPECT_EQ(memory[4], 1);

        iter++;
      }
    }
  }
}

TEST(EmitterLoadsAndStores, load64_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load64_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 8b 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load64_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "488B050C000000488B0D0C000000488B150C000000488B1D0C000000488B250C000000488B2D0C00000048"
            "8B350C000000488B3D0C0000004C8B050C0000004C8B0D0C0000004C8B150C0000004C8B1D0C0000004C8B"
            "250C0000004C8B2D0C0000004C8B350C0000004C8B3D0C000000");
}

TEST(EmitterLoadsAndStores, load32s_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load32s_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 63 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load32s_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4863050C00000048630D0C0000004863150C00000048631D0C0000004863250C00000048632D0C00000048"
            "63350C00000048633D0C0000004C63050C0000004C630D0C0000004C63150C0000004C631D0C0000004C63"
            "250C0000004C632D0C0000004C63350C0000004C633D0C000000");
}

TEST(EmitterLoadsAndStores, load32u_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load32u_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "8b 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load32u_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "8B050C0000008B0D0C0000008B150C0000008B1D0C0000008B250C0000008B2D0C0000008B350C0000008B"
            "3D0C000000448B050C000000448B0D0C000000448B150C000000448B1D0C000000448B250C000000448B2D"
            "0C000000448B350C000000448B3D0C000000");
}

TEST(EmitterLoadsAndStores, load16u_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load16u_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b7 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load16u_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "480FB7050C000000480FB70D0C000000480FB7150C000000480FB71D0C000000480FB7250C000000480FB7"
            "2D0C000000480FB7350C000000480FB73D0C0000004C0FB7050C0000004C0FB70D0C0000004C0FB7150C00"
            "00004C0FB71D0C0000004C0FB7250C0000004C0FB72D0C0000004C0FB7350C0000004C0FB73D0C000000");
}

TEST(EmitterLoadsAndStores, load16s_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load16s_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f bf 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load16s_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "480FBF050C000000480FBF0D0C000000480FBF150C000000480FBF1D0C000000480FBF250C000000480FBF"
            "2D0C000000480FBF350C000000480FBF3D0C0000004C0FBF050C0000004C0FBF0D0C0000004C0FBF150C00"
            "00004C0FBF1D0C0000004C0FBF250C0000004C0FBF2D0C0000004C0FBF350C0000004C0FBF3D0C000000");
}

TEST(EmitterLoadsAndStores, load8s_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load8s_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f be 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load8s_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "480FBE050C000000480FBE0D0C000000480FBE150C000000480FBE1D0C000000480FBE250C000000480FBE"
            "2D0C000000480FBE350C000000480FBE3D0C0000004C0FBE050C0000004C0FBE0D0C0000004C0FBE150C00"
            "00004C0FBE1D0C0000004C0FBE250C0000004C0FBE2D0C0000004C0FBE350C0000004C0FBE3D0C000000");
}

TEST(EmitterLoadsAndStores, load8u_rip) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::load8u_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 0f b6 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::load8u_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "480FB6050C000000480FB60D0C000000480FB6150C000000480FB61D0C000000480FB6250C000000480FB6"
            "2D0C000000480FB6350C000000480FB63D0C0000004C0FB6050C0000004C0FB60D0C0000004C0FB6150C00"
            "00004C0FB61D0C0000004C0FB6250C0000004C0FB62D0C0000004C0FB6350C0000004C0FB63D0C000000");
}

TEST(EmitterLoadsAndStores, store64_rip_s32) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::store64_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "48 89 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::store64_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4889050C00000048890D0C0000004889150C00000048891D0C0000004889250C00000048892D0C00000048"
            "89350C00000048893D0C0000004C89050C0000004C890D0C0000004C89150C0000004C891D0C0000004C89"
            "250C0000004C892D0C0000004C89350C0000004C893D0C000000");
}

TEST(EmitterLoadsAndStores, store32_rip_s32) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::store32_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "89 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::store32_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "89050C000000890D0C00000089150C000000891D0C00000089250C000000892D0C00000089350C00000089"
            "3D0C0000004489050C00000044890D0C0000004489150C00000044891D0C0000004489250C00000044892D"
            "0C0000004489350C00000044893D0C000000");
}

TEST(EmitterLoadsAndStores, store16_rip_s32) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::store16_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 89 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::store16_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "6689050C00000066890D0C0000006689150C00000066891D0C0000006689250C00000066892D0C00000066"
            "89350C00000066893D0C000000664489050C0000006644890D0C000000664489150C0000006644891D0C00"
            "0000664489250C0000006644892D0C000000664489350C0000006644893D0C000000");
}

TEST(EmitterLoadsAndStores, store8_rip_s32) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit(IGen::store8_rip_s32(RAX, 12));
  EXPECT_EQ(tester.dump_to_hex_string(), "88 05 0c 00 00 00");

  tester.clear();
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::store8_rip_s32(i, 12));
  }

  EXPECT_EQ(tester.dump_to_hex_string(true),
            "88050C000000880D0C00000088150C000000881D0C0000004088250C00000040882D0C0000004088350C00"
            "000040883D0C0000004488050C00000044880D0C0000004488150C00000044881D0C0000004488250C0000"
            "0044882D0C0000004488350C00000044883D0C000000");
}

TEST(EmitterLoadsAndStores, static_addr) {
  CodeTester tester;
  tester.init_code_buffer(512);

  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::mov_gpr64_u64(i, 12345));  // load test reg with junk
    int start_of_lea = tester.size();
    auto lea_instr = IGen::static_addr(i, INT32_MAX);
    tester.emit(lea_instr);
    // patch instruction to lea the start of this code + 1.
    tester.write<s32>(-start_of_lea - lea_instr.length() + 1,
                      start_of_lea + lea_instr.offset_of_disp());
    tester.emit(IGen::mov_gpr64_gpr64(RAX, i));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();

    auto result = tester.execute();
    EXPECT_EQ(result, (u64)(tester.data()) + 1);
  }
}

#ifdef __linux__
TEST(EmitterXmm32, load32_xmm32_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64(XMM3, RAX, RBX));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 10 1c 03");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(i, (iter & 1) ? 0 : UINT64_MAX));
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // load into k
        tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64(XMM0 + k, i, j));
        // move to return
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float), 0, 0), 3.45f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float), 0, 0), 1.23f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float), 0, 0), 5.67f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float), 0, 0), 0);

        iter++;
      }
    }
  }
}

TEST(EmitterXmm32, load32_xmm32_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s8(XMM3, RAX, RBX, -1));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 10 5c 03 ff");

  auto instr = IGen::load32_xmm32_gpr64_plus_gpr64_plus_s8(XMM3, RBX, RSI, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(i, (iter & 1) ? 0 : UINT64_MAX));
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        // load into k
        tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s8(XMM0 + k, i, j, -3));
        // move to return
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float) + 3, 0, 0), 3.45f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float) + 3, 0, 0), 1.23f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float) + 3, 0, 0), 5.67f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float) + 3, 0, 0), 0);

        iter++;
      }
    }
  }
}

TEST(EmitterXmm32, load32_xmm32_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s32(XMM3, RAX, RBX, -1));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 10 9c 03 ff ff ff ff");

  auto instr = IGen::load32_xmm32_gpr64_plus_gpr64_plus_s32(XMM3, RBX, RSI, -1234);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -1234);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));

        // fill k with junk
        tester.emit(IGen::mov_gpr64_u64(i, (iter & 1) ? 0 : UINT64_MAX));
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop args into appropriate register
        tester.emit(IGen::pop_gpr64(i));  // i will have offset 0
        tester.emit(IGen::pop_gpr64(j));  // j will have offset 1

        s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

        // load into k
        tester.emit(IGen::load32_xmm32_gpr64_plus_gpr64_plus_s32(XMM0 + k, i, j, offset));
        // move to return
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float) - offset, 0, 0),
                        3.45f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float) - offset, 0, 0),
                        1.23f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float) - offset, 0, 0),
                        5.67f);
        EXPECT_FLOAT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float) - offset, 0, 0),
                        0);
        iter++;
      }
    }
  }
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

TEST(EmitterXmm32, store32_xmm32_gpr64_plus_gpr64) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64(RAX, RBX, XMM7));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 11 3c 03");

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }

    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }

      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack

        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));  // addr2
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));  // addr1
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));  // value

        // pop value into addr1 GPR
        tester.emit(IGen::pop_gpr64(i));
        // move to XMM
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(i));
        tester.emit(IGen::pop_gpr64(j));

        // store
        tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64(i, j, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12, as_u32(1.234f), 0);
        EXPECT_FLOAT_EQ(memory[2], 1.23f);
        EXPECT_FLOAT_EQ(memory[3], 1.234f);
        EXPECT_FLOAT_EQ(memory[4], 5.67f);

        iter++;
      }
    }
  }
}

TEST(EmitterXmm32, store32_xmm32_gpr64_plus_gpr64_plus_s8) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s8(RAX, RBX, XMM3, -1));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 11 5c 03 ff");

  auto instr = IGen::store32_xmm32_gpr64_plus_gpr64_plus_s8(RBX, RSI, XMM3, -3);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(s8(buff[instr.offset_of_disp()]), -3);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));  // addr2
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));  // addr1
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));  // value

        // pop value into addr1 GPR
        tester.emit(IGen::pop_gpr64(i));
        // move to XMM
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(i));
        tester.emit(IGen::pop_gpr64(j));

        s64 offset = (iter & 1) ? INT8_MAX : INT8_MIN;

        // load into k
        tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s8(i, j, XMM0 + k, offset));

        // move to return
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12 - offset, as_u32(1.234f), 0);
        EXPECT_FLOAT_EQ(memory[2], 1.23f);
        EXPECT_FLOAT_EQ(memory[3], 1.234f);
        EXPECT_FLOAT_EQ(memory[4], 5.67f);

        iter++;
      }
    }
  }
}

TEST(EmitterXmm32, store32_xmm32_gpr64_plus_gpr64_plus_s32) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s32(RAX, RBX, XMM3, -1));
  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 11 9c 03 ff ff ff ff");

  auto instr = IGen::store32_xmm32_gpr64_plus_gpr64_plus_s32(RBX, RSI, XMM3, -1234);
  u8 buff[256];
  instr.emit(buff);
  EXPECT_EQ(*(s32*)(buff + instr.offset_of_disp()), -1234);

  int iter = 0;
  for (int i = 0; i < 16; i++) {
    if (i == RSP) {
      continue;
    }
    for (int j = 0; j < 16; j++) {
      if (j == RSP || j == i) {
        continue;
      }
      for (int k = 0; k < 16; k++) {
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        // push args to the stack
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(1)));  // addr2
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(0)));  // addr1
        tester.emit(IGen::push_gpr64(tester.get_c_abi_arg_reg(2)));  // value

        // pop value into addr1 GPR
        tester.emit(IGen::pop_gpr64(i));
        // move to XMM
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + k, i));

        // pop addrs
        tester.emit(IGen::pop_gpr64(i));
        tester.emit(IGen::pop_gpr64(j));

        s64 offset = (iter & 1) ? INT32_MAX : INT32_MIN;

        // load into k
        tester.emit(IGen::store32_xmm32_gpr64_plus_gpr64_plus_s32(i, j, XMM0 + k, offset));

        // move to return
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + k));

        // return!
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();

        // prepare the memory:
        float memory[8] = {0, 0, 1.23f, 3.45f, 5.67f, 0, 0, 0};

        // run!
        tester.execute((u64)memory, 12 - offset, as_u32(1.234f), 0);
        EXPECT_FLOAT_EQ(memory[2], 1.23f);
        EXPECT_FLOAT_EQ(memory[3], 1.234f);
        EXPECT_FLOAT_EQ(memory[4], 5.67f);

        iter++;
      }
    }
  }
}

TEST(EmitterXmm32, static_load_xmm32) {
  CodeTester tester;
  tester.init_code_buffer(512);
  for (int i = 0; i < 16; i++) {
    tester.clear();
    tester.emit_push_all_xmms();
    tester.emit_push_all_gprs(true);

    auto loc_of_load = tester.size();
    auto load_instr = IGen::static_load_xmm32(XMM0 + i, INT32_MAX);

    tester.emit(load_instr);
    tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + i));
    tester.emit_pop_all_gprs(true);
    tester.emit_pop_all_xmms();
    tester.emit_return();
    auto loc_of_float = tester.emit_data(float(1.2345f));

    // patch offset
    tester.write<s32>(loc_of_float - loc_of_load - load_instr.length(),
                      loc_of_load + load_instr.offset_of_disp());

    auto result = tester.execute_ret<float>(0, 0, 0, 0);
    EXPECT_FLOAT_EQ(result, 1.2345f);
  }
}

TEST(EmitterXmm32, static_store_xmm32) {
  CodeTester tester;
  tester.init_code_buffer(512);
  for (int i = 0; i < 16; i++) {
    tester.clear();
    tester.emit_push_all_xmms();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, tester.get_c_abi_arg_reg(0)));

    auto loc_of_store = tester.size();
    auto store_instr = IGen::static_store_xmm32(XMM0 + i, INT32_MAX);

    tester.emit(store_instr);
    tester.emit_pop_all_gprs(true);
    tester.emit_pop_all_xmms();
    tester.emit_return();
    auto loc_of_float = tester.emit_data(float(1.2345f));

    tester.write<s32>(loc_of_float - loc_of_store - store_instr.length(),
                      loc_of_store + store_instr.offset_of_disp());
    tester.execute(as_u32(-44.567f), 0, 0, 0);
    EXPECT_FLOAT_EQ(-44.567f, tester.read<float>(loc_of_float));
  }
}

TEST(EmitterXmm32, ucomiss) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit(IGen::cmp_flt_flt(XMM13, XMM14));
  EXPECT_EQ("45 0f 2e ee", tester.dump_to_hex_string());
}

TEST(EmitterXmm32, mul) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for (int i = 0; i < 16; i++) {
        for (int j = 0; j < 16; j++) {
          if (i == j) {
            continue;
          }
          auto expected = f * g;
          tester.clear();
          tester.emit_push_all_xmms();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, RAX));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + j, RAX));
          tester.emit(IGen::mulss_xmm_xmm(XMM0 + j, XMM0 + i));
          tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + j));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_xmms();
          tester.emit_return();
          auto result = tester.execute_ret<float>(0, 0, 0, 0);
          EXPECT_FLOAT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterXmm32, div) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<float> vals = {1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for (int i = 0; i < 16; i++) {
        for (int j = 0; j < 16; j++) {
          if (i == j) {
            continue;
          }
          auto expected = g / f;
          tester.clear();
          tester.emit_push_all_xmms();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, RAX));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + j, RAX));
          tester.emit(IGen::divss_xmm_xmm(XMM0 + j, XMM0 + i));
          tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + j));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_xmms();
          tester.emit_return();
          auto result = tester.execute_ret<float>(0, 0, 0, 0);
          EXPECT_FLOAT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterXmm32, add) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};
  for (auto f : vals) {
    for (auto g : vals) {
      for (int i = 0; i < 16; i++) {
        for (int j = 0; j < 16; j++) {
          if (i == j) {
            continue;
          }
          auto expected = g + f;
          tester.clear();
          tester.emit_push_all_xmms();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, RAX));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + j, RAX));
          tester.emit(IGen::addss_xmm_xmm(XMM0 + j, XMM0 + i));
          tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + j));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_xmms();
          tester.emit_return();
          auto result = tester.execute_ret<float>(0, 0, 0, 0);
          EXPECT_FLOAT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterXmm32, sub) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<float> vals = {0.f, 1.f, 0.2f, -1.f, 1235423.2f, -3457343.3f, 7.545f};

  for (auto f : vals) {
    for (auto g : vals) {
      for (int i = 0; i < 16; i++) {
        for (int j = 0; j < 16; j++) {
          if (i == j) {
            continue;
          }
          auto expected = g - f;
          tester.clear();
          tester.emit_push_all_xmms();
          tester.emit_push_all_gprs(true);
          u64 val = 0;
          memcpy(&val, &f, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, RAX));
          memcpy(&val, &g, sizeof(float));
          tester.emit(IGen::mov_gpr64_u64(RAX, val));
          tester.emit(IGen::movd_xmm32_gpr32(XMM0 + j, RAX));
          tester.emit(IGen::subss_xmm_xmm(XMM0 + j, XMM0 + i));
          tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + j));
          tester.emit_pop_all_gprs(true);
          tester.emit_pop_all_xmms();
          tester.emit_return();
          auto result = tester.execute_ret<float>(0, 0, 0, 0);
          EXPECT_FLOAT_EQ(result, expected);
        }
      }
    }
  }
}

TEST(EmitterXmm32, float_to_int) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<float> vals = {0.f,    1.f,  0.2f, -1.f,  1235423.2f, -3457343.3f,
                             7.545f, 0.1f, 0.9f, -0.1f, -0.9f};

  for (auto g : vals) {
    for (int i = 0; i < 16; i++) {
      for (int j = 0; j < 16; j++) {
        if (j == RSP) {
          continue;
        }
        s32 expected = g;
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        u64 val = 0;
        memcpy(&val, &g, sizeof(float));
        tester.emit(IGen::mov_gpr64_u64(RAX, val));
        tester.emit(IGen::movd_xmm32_gpr32(XMM0 + i, RAX));
        tester.emit(IGen::float_to_int32(j, XMM0 + i));
        tester.emit(IGen::mov_gpr64_gpr64(RAX, j));
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();
        auto result = tester.execute_ret<s32>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterXmm32, int_to_float) {
  CodeTester tester;
  tester.init_code_buffer(512);

  std::vector<s64> vals = {0, 1, -1, INT32_MAX, -3457343, 7, INT32_MIN};

  for (auto g : vals) {
    for (int i = 0; i < 16; i++) {
      for (int j = 0; j < 16; j++) {
        if (j == RSP) {
          continue;
        }
        float expected = g;
        tester.clear();
        tester.emit_push_all_xmms();
        tester.emit_push_all_gprs(true);
        tester.emit(IGen::mov_gpr64_u64(j, g));
        tester.emit(IGen::int32_to_float(XMM0 + i, j));
        tester.emit(IGen::movd_gpr32_xmm32(RAX, XMM0 + i));
        tester.emit_pop_all_gprs(true);
        tester.emit_pop_all_xmms();
        tester.emit_return();
        auto result = tester.execute_ret<float>(0, 0, 0, 0);
        EXPECT_EQ(result, expected);
      }
    }
  }
}

TEST(EmitterSlow, xmm32_move) {
  std::vector<u32> u32_constants = {0, INT32_MAX, UINT32_MAX, 17};

  // test moving between xmms (32-bit) and gprs.
  CodeTester tester;
  tester.init_code_buffer(512);

  for (auto constant : u32_constants) {
    for (int r1 = 0; r1 < 16; r1++) {
      if (r1 == RSP) {
        continue;
      }
      for (int r2 = 0; r2 < 16; r2++) {
        if (r2 == RSP) {
          continue;
        }
        for (int r3 = 0; r3 < 16; r3++) {
          for (int r4 = 0; r4 < 16; r4++) {
            tester.clear();
            tester.emit_push_all_xmms();
            tester.emit_push_all_gprs(true);
            // move constant to gpr
            tester.emit(IGen::mov_gpr64_u32(r1, constant));
            // move gpr to xmm
            tester.emit(IGen::movd_xmm32_gpr32(XMM0 + r3, r1));
            // move xmm to xmm
            tester.emit(IGen::mov_xmm32_xmm32(XMM0 + r4, XMM0 + r3));
            // move xmm to gpr
            tester.emit(IGen::movd_gpr32_xmm32(r2, XMM0 + r4));
            // return!
            tester.emit(IGen::mov_gpr64_gpr64(RAX, r2));
            tester.emit_pop_all_gprs(true);
            tester.emit_pop_all_xmms();
            tester.emit_return();
          }
        }
      }
    }
  }
  // todo - finish this test
}
#endif

TEST(Emitter, LEA) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::lea_reg_plus_off(RDI, RSP, -3));
  tester.emit(IGen::lea_reg_plus_off(RDI, R12, -3));
  tester.emit(IGen::lea_reg_plus_off(R13, RSP, -3));
  tester.emit(IGen::lea_reg_plus_off(R13, R12, -3));
  tester.emit(IGen::lea_reg_plus_off(RDI, RSP, -300));
  tester.emit(IGen::lea_reg_plus_off(RDI, R12, -300));
  tester.emit(IGen::lea_reg_plus_off(R13, RSP, -300));
  tester.emit(IGen::lea_reg_plus_off(R13, R12, -300));
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "488D7C24FD498D7C24FD4C8D6C24FD4D8D6C24FD488DBC24D4FEFFFF498DBC24D4FEFFFF4C8DAC24D4FEFF"
            "FF4D8DAC24D4FEFFFF");
}

TEST(EmitterXMM, StackLoad32) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::load32_xmm32_gpr64_plus_s32(XMM0 + 3, RSP, -1234));
  tester.emit(IGen::load32_xmm32_gpr64_plus_s32(XMM0 + 13, RSP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F30F109C242EFBFFFFF3440F10AC242EFBFFFF");
}

TEST(EmitterXMM, StackLoad8) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::load32_xmm32_gpr64_plus_s8(XMM0 + 3, RSP, -12));
  tester.emit(IGen::load32_xmm32_gpr64_plus_s8(XMM0 + 13, RSP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F30F105C24F4F3440F106C24F4");
}

TEST(EmitterXMM, StackLoadFull32) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::load128_xmm128_gpr64_s32(XMM0 + 3, RSP, -1234));
  tester.emit(IGen::load128_xmm128_gpr64_s32(XMM0 + 13, RSP, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "660F6F9C242EFBFFFF66440F6FAC242EFBFFFF");
}

TEST(EmitterXMM, StackLoadFull8) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::load128_xmm128_gpr64_s8(XMM0 + 3, RSP, -12));
  tester.emit(IGen::load128_xmm128_gpr64_s8(XMM0 + 13, RSP, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "660F6F5C24F466440F6F6C24F4");
}

TEST(EmitterXMM, StackStore32) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::store32_xmm32_gpr64_plus_s32(RSP, XMM0 + 3, -1234));
  tester.emit(IGen::store32_xmm32_gpr64_plus_s32(RSP, XMM0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F30F119C242EFBFFFFF3440F11AC242EFBFFFF");
}

TEST(EmitterXMM, StackStore8) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::store32_xmm32_gpr64_plus_s8(RSP, XMM0 + 3, -12));
  tester.emit(IGen::store32_xmm32_gpr64_plus_s8(RSP, XMM0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F30F115C24F4F3440F116C24F4");
}

TEST(EmitterXMM, StackStoreFull32) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::store128_gpr64_xmm128_s32(RSP, XMM0 + 3, -1234));
  tester.emit(IGen::store128_gpr64_xmm128_s32(RSP, XMM0 + 13, -1234));
  EXPECT_EQ(tester.dump_to_hex_string(true), "660F7F9C242EFBFFFF66440F7FAC242EFBFFFF");
}

TEST(EmitterXMM, StackStoreFull8) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::store128_gpr64_xmm128_s8(RSP, XMM0 + 3, -12));
  tester.emit(IGen::store128_gpr64_xmm128_s8(RSP, XMM0 + 13, -12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "660F7F5C24F466440F7F6C24F4");
}

TEST(EmitterXMM, SqrtS) {
  CodeTester tester;
  tester.init_code_buffer(1024);
  tester.emit(IGen::sqrts_xmm(XMM0 + 1, XMM0 + 2));
  tester.emit(IGen::sqrts_xmm(XMM0 + 11, XMM0 + 2));
  tester.emit(IGen::sqrts_xmm(XMM0 + 1, XMM0 + 12));
  tester.emit(IGen::sqrts_xmm(XMM0 + 11, XMM0 + 12));
  EXPECT_EQ(tester.dump_to_hex_string(true), "F30F51CAF3440F51DAF3410F51CCF3450F51DC");
}
