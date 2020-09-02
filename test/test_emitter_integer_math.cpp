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