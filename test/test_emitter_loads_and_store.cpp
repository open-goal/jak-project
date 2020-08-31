/*!
 * @file test_emitter_loads_and_stores.cpp
 * Tests for the emitter which are fast (checking 100's of functions)
 */

#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "third-party/fmt/core.h"
//
using namespace emitter;

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

        if (memory[3] != 7) {
          fmt::print("test {}, {}, {}\n", tester.reg_name(i), tester.reg_name(j),
                     tester.reg_name(k));
          printf("%s\n", tester.dump_to_hex_string().c_str());
        }
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

        if (memory[3] != 7) {
          fmt::print("test {}, {}, {}\n", tester.reg_name(i), tester.reg_name(j),
                     tester.reg_name(k));
          printf("%s\n", tester.dump_to_hex_string().c_str());
        }
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

        if (memory[3] != 7) {
          fmt::print("test {}, {}, {}\n", tester.reg_name(i), tester.reg_name(j),
                     tester.reg_name(k));
          printf("%s\n", tester.dump_to_hex_string().c_str());
        }
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