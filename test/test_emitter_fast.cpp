/*!
 * @file test_emitter_slow.cpp
 * Tests for the emitter which are fast (checking 100's of functions)
 */

#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "third-party/fmt/core.h"
//
using namespace emitter;

TEST(Emitter, load_constant_64_and_move_gpr_gpr_64) {
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

TEST(Emitter, load_constant_32_unsigned) {
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

TEST(Emitter, load_constant_32_signed) {
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

TEST(Emitter, load8s_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load8s_gpr64_gpr64_gpr64_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load8s_gpr64_gpr64_gpr64_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load8u_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load8u_gpr64_gpr64_gpr64_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load8u_gpr64_gpr64_gpr64_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16s_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16s_gpr64_gpr64_plus_gpr64_plus_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16s_gpr64_gpr64_plus_gpr64_plus_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16u_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16u_gpr64_gpr64_plus_gpr64_plus_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load16u_gpr64_gpr64_plus_gpr64_plus_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32s_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32s_gpr64_gpr64_plus_gpr64_plus_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32s_gpr64_gpr64_plus_gpr64_plus_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32u_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32u_gpr64_gpr64_plus_gpr64_plus_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load32u_gpr64_gpr64_plus_gpr64_plus_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load64_gpr64_goal_ptr_gpr64) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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


TEST(Emitter, load64_gpr64_gpr64_plus_gpr64_plus_s8) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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

TEST(Emitter, load64_gpr64_gpr64_plus_gpr64_plus_s32) {
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
        if(k != i && k != j) {
          tester.emit(IGen::mov_gpr64_u64(k, (iter&1)?0:UINT64_MAX));
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