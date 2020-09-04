#include "third-party/fmt/core.h"
#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"

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
          if (result != expected) {
            fmt::print("fail {} x {}: {}\n", v1, v2, tester.dump_to_hex_string());
          }
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