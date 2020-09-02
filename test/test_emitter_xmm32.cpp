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
    EXPECT_EQ(result, 1.2345f);
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
    EXPECT_EQ(-44.567f, tester.read<float>(loc_of_float));
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
          EXPECT_EQ(result, expected);
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
          EXPECT_EQ(result, expected);
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
          EXPECT_EQ(result, expected);
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
          EXPECT_EQ(result, expected);
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