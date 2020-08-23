/*!
 * @file test_CodeTester.cpp
 * Tests for the CodeTester, a tool for testing the emitter by emitting code and running it
 * from within the test application.
 */

#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"

using namespace goal;

TEST(CodeTester, prologue) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit_push_all_gprs();
  // check we generate the right code for pushing all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "50 51 52 53 54 55 56 57 41 50 41 51 41 52 41 53 41 54 41 55 41 56 41 57");
}

TEST(CodeTester, epilogue) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit_pop_all_gprs();
  // check we generate the right code for popping all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "41 5f 41 5e 41 5d 41 5c 41 5b 41 5a 41 59 41 58 5f 5e 5d 5c 5b 5a 59 58");
}

TEST(CodeTester, execute_return) {
  CodeTester tester;
  tester.init_code_buffer(256);
  // test creating a function which simply returns
  tester.emit_return();
  // and execute it!
  tester.execute();
}

TEST(CodeTester, execute_push_pop_gprs) {
  CodeTester tester;
  tester.init_code_buffer(256);
  // test we can push/pop gprs without crashing.
  tester.emit_push_all_gprs();
  tester.emit_pop_all_gprs();
  tester.emit_return();
  tester.execute();
}

TEST(CodeTester, load_constant_64_and_move_gpr_gpr_64) {
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

TEST(CodeTester, load_constant_32_unsigned) {
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
      tester.emit(IGen::mov_gpr64_u32(r1, constant));
      tester.emit(IGen::mov_gpr64_gpr64(RAX, r1));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute(), constant);
    }
  }
}

TEST(CodeTester, load_constant_32_signed) {
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

TEST(CodeTester, xmm_move) {
  std::vector<u32> u32_constants = {0, INT32_MAX, UINT32_MAX, 17};

  // test moving between xmms (32-bit) and gprs.
  CodeTester tester;
  tester.init_code_buffer(256);

  for(auto constant : u32_constants) {
    for(int r1 = 0; r1 < 16; r1++) {
      if(r1 == RSP) {
        continue;
      }
      for(int r2 = 0; r2 < 16; r2++) {
        if(r2 == RSP) {
          continue;
        }
        for(int r3 = 0; r3 < 16; r3++) {
          for(int r4 = 0; r4 < 16; r4++) {
            tester.clear();
            tester.emit_push_all_gprs(true);
            // move constant to gpr
            tester.emit(IGen::mov_gpr64_u32(r1, constant));
            // move gpr to xmm
            tester.emit(IGen::movd_xmm32_gpr32(get_nth_xmm(r3), r1));
            // move xmm to xmm
            tester.emit(IGen::mov_xmm32_xmm32(get_nth_xmm(r4), get_nth_xmm(r3)));
            // move xmm to gpr
            tester.emit(IGen::movd_gpr32_xmm32(r2, get_nth_xmm(r4)));
            // return!
            tester.emit(IGen::mov_gpr64_gpr64(RAX, r2));
            tester.emit_return();
          }
        }
      }
    }
  }
}