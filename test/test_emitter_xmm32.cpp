#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "third-party/fmt/core.h"

using namespace emitter;

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
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float), 0, 0), 3.45f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float), 0, 0), 1.23f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float), 0, 0), 5.67f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float), 0, 0), 0);

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
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float) + 3, 0, 0), 3.45f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float) + 3, 0, 0), 1.23f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float) + 3, 0, 0), 5.67f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float) + 3, 0, 0), 0);

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
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 3 * sizeof(float) - offset, 0, 0), 3.45f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 2 * sizeof(float) - offset, 0, 0), 1.23f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 4 * sizeof(float) - offset, 0, 0), 5.67f);
        EXPECT_EQ(tester.execute_ret<float>((u64)memory, 5 * sizeof(float) - offset, 0, 0), 0);
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
        EXPECT_EQ(memory[2], 1.23f);
        EXPECT_EQ(memory[3], 1.234f);
        EXPECT_EQ(memory[4], 5.67f);

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
        EXPECT_EQ(memory[2], 1.23f);
        EXPECT_EQ(memory[3], 1.234f);
        EXPECT_EQ(memory[4], 5.67f);

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
        EXPECT_EQ(memory[2], 1.23f);
        EXPECT_EQ(memory[3], 1.234f);
        EXPECT_EQ(memory[4], 5.67f);

        iter++;
      }
    }
  }
}
