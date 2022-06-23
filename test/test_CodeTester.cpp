/*!
 * @file test_CodeTester.cpp
 * Tests for the CodeTester, a tool for testing the emitter by emitting code and running it
 * from within the test application.
 *
 * These tests should just make sure the basic functionality of CodeTester works, and that it
 * can generate prologues/epilogues, and execute them without crashing.
 */

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"

using namespace emitter;

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

TEST(CodeTester, xmm_store_128) {
  CodeTester tester;
  tester.init_code_buffer(256);
  //  movdqa [rbx], xmm3
  //  movdqa [r14], xmm3
  //  movdqa [rbx], xmm14
  //  movdqa [r14], xmm13
  tester.emit(IGen::store128_gpr64_xmm128(RBX, XMM3));
  tester.emit(IGen::store128_gpr64_xmm128(R14, XMM3));
  tester.emit(IGen::store128_gpr64_xmm128(RBX, XMM14));
  tester.emit(IGen::store128_gpr64_xmm128(R14, XMM13));
  EXPECT_EQ(tester.dump_to_hex_string(),
            "66 0f 7f 1b 66 41 0f 7f 1e 66 44 0f 7f 33 66 45 0f 7f 2e");

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(RSP, XMM1));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 7f 0c 24");  // requires SIB byte.

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(R12, XMM13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 7f 2c 24");  // requires SIB byte and REX byte

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(RBP, XMM1));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 7f 4d 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(RBP, XMM11));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 0f 7f 5d 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(R13, XMM2));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 41 0f 7f 55 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_xmm128(R13, XMM12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 7f 65 00");

  //  tester.emit(IGen::store128_gpr64_xmm128(RBX, XMM3));
  //  tester.emit(IGen::store128_gpr64_xmm128(R14, XMM3));
  //  tester.emit(IGen::store128_gpr64_xmm128(RBX, XMM14));
  //  tester.emit(IGen::store128_gpr64_xmm128(R14, XMM13));
  //  EXPECT_EQ(tester.dump_to_hex_string(),
  //            "f3 0f 7f 1b f3 41 0f 7f 1e f3 44 0f 7f 33 f3 45 0f 7f 2e");
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(RSP, XMM1));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 7f 0c 24");  // requires SIB byte.
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(R12, XMM13));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 45 0f 7f 2c 24");  // requires SIB byte and REX
  //  byte
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(RBP, XMM1));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 7f 4d 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(RBP, XMM11));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 44 0f 7f 5d 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(R13, XMM2));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 41 0f 7f 55 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::store128_gpr64_xmm128(R13, XMM12));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 45 0f 7f 65 00");
}

TEST(CodeTester, sub_gpr64_imm8) {
  CodeTester tester;
  tester.init_code_buffer(256);
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::sub_gpr64_imm8s(i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4883E8FF4883E9FF4883EAFF4883EBFF4883ECFF4883EDFF4883EEFF4883EFFF4983E8FF4983E9FF4983EA"
            "FF4983EBFF4983ECFF4983EDFF4983EEFF4983EFFF");
}

TEST(CodeTester, add_gpr64_imm8) {
  CodeTester tester;
  tester.init_code_buffer(256);
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::add_gpr64_imm8s(i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4883C0FF4883C1FF4883C2FF4883C3FF4883C4FF4883C5FF4883C6FF4883C7FF4983C0FF4983C1FF4983C2"
            "FF4983C3FF4983C4FF4983C5FF4983C6FF4983C7FF");
}

TEST(CodeTester, xmm_load_128) {
  CodeTester tester;
  tester.init_code_buffer(256);

  //  tester.emit(IGen::load128_xmm128_gpr64(XMM3, RBX));
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM3, R14));
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM14, RBX));
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM13, R14));
  //  EXPECT_EQ(tester.dump_to_hex_string(),
  //            "f3 0f 6f 1b f3 41 0f 6f 1e f3 44 0f 6f 33 f3 45 0f 6f 2e");
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM1, RSP));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 6f 0c 24");  // requires SIB byte.
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM13, R12));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 45 0f 6f 2c 24");  // requires SIB byte and REX
  //  byte
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM1, RBP));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 0f 6f 4d 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM11, RBP));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 44 0f 6f 5d 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM2, R13));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 41 0f 6f 55 00");
  //
  //  tester.clear();
  //  tester.emit(IGen::load128_xmm128_gpr64(XMM12, R13));
  //  EXPECT_EQ(tester.dump_to_hex_string(), "f3 45 0f 6f 65 00");
  tester.emit(IGen::load128_xmm128_gpr64(XMM3, RBX));
  tester.emit(IGen::load128_xmm128_gpr64(XMM3, R14));
  tester.emit(IGen::load128_xmm128_gpr64(XMM14, RBX));
  tester.emit(IGen::load128_xmm128_gpr64(XMM13, R14));
  EXPECT_EQ(tester.dump_to_hex_string(),
            "66 0f 6f 1b 66 41 0f 6f 1e 66 44 0f 6f 33 66 45 0f 6f 2e");

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM1, RSP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 6f 0c 24");  // requires SIB byte.

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM13, R12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 6f 2c 24");  // requires SIB byte and REX byte

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM1, RBP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 6f 4d 00");

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM11, RBP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 0f 6f 5d 00");

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM2, R13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 41 0f 6f 55 00");

  tester.clear();
  tester.emit(IGen::load128_xmm128_gpr64(XMM12, R13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 6f 65 00");
}

TEST(CodeTester, push_pop_xmms) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit_push_all_xmms();
  tester.emit_pop_all_xmms();
  tester.emit_return();
  tester.execute();
}

TEST(CodeTester, push_pop_all_the_things) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit_push_all_xmms();
  tester.emit_push_all_gprs();

  // ...
  tester.emit_pop_all_gprs();
  tester.emit_pop_all_xmms();
  tester.emit_return();
  tester.execute();
}
