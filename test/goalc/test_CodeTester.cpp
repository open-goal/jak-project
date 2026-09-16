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
#include "goalc/emitter/InstructionSet.h"
#include "goalc/emitter/Register.h"
#include "gtest/gtest.h"

using namespace emitter;

TEST(CodeTester, prologue_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit_push_all_gprs();
  // check we generate the right code for pushing all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "50 51 52 53 54 55 56 57 41 50 41 51 41 52 41 53 41 54 41 55 41 56 41 57");
}

TEST(CodeTester, prologue_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  // tester.emit(IGen::push_gpr64(tester.generator(), ARM64_REG::X0));
  // EXPECT_EQ(tester.dump_to_hex_string(), "e0 8f 1f f8");
  tester.emit_push_all_gprs();
  // check we generate the right code for pushing all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "e0 0f 1f f8 e1 0f 1f f8 e2 0f 1f f8 e3 0f 1f f8 e4 0f 1f f8 e5 0f 1f f8 e6 0f 1f f8 "
            "e7 0f 1f f8 e8 0f 1f f8 e9 0f 1f f8 ea 0f 1f f8 eb 0f 1f f8 ec 0f 1f f8 ed 0f 1f f8 "
            "ee 0f 1f f8 ef 0f 1f f8 f0 0f 1f f8 f1 0f 1f f8 f2 0f 1f f8 f3 0f 1f f8 f4 0f 1f f8 "
            "f5 0f 1f f8 f6 0f 1f f8 f7 0f 1f f8 f8 0f 1f f8 f9 0f 1f f8 fa 0f 1f f8 fb 0f 1f f8 "
            "fc 0f 1f f8 fd 0f 1f f8 fe 0f 1f f8");
}

TEST(CodeTester, epilogue_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  tester.emit_pop_all_gprs();
  // check we generate the right code for popping all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "41 5f 41 5e 41 5d 41 5c 41 5b 41 5a 41 59 41 58 5f 5e 5d 5c 5b 5a 59 58");
}

TEST(CodeTester, epilogue_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  tester.emit_pop_all_gprs();
  // check we generate the right code for popping all gpr's
  EXPECT_EQ(tester.dump_to_hex_string(),
            "fe 07 41 f8 fd 07 41 f8 fc 07 41 f8 fb 07 41 f8 fa 07 41 f8 f9 07 41 f8 f8 07 41 f8 "
            "f7 07 41 f8 f6 07 41 f8 f5 07 41 f8 f4 07 41 f8 f3 07 41 f8 f2 07 41 f8 f1 07 41 f8 "
            "f0 07 41 f8 ef 07 41 f8 ee 07 41 f8 ed 07 41 f8 ec 07 41 f8 eb 07 41 f8 ea 07 41 f8 "
            "e9 07 41 f8 e8 07 41 f8 e7 07 41 f8 e6 07 41 f8 e5 07 41 f8 e4 07 41 f8 e3 07 41 f8 "
            "e2 07 41 f8 e1 07 41 f8 e0 07 41 f8");
}

TEST(CodeTester, sub_gpr64_imm8_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::sub_gpr64_imm8s(tester.generator(), i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4883E8FF4883E9FF4883EAFF4883EBFF4883ECFF4883EDFF4883EEFF4883EFFF4983E8FF4983E9FF4983EA"
            "FF4983EBFF4983ECFF4983EDFF4983EEFF4983EFFF");
}

TEST(CodeTester, sub_gpr64_imm8_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  for (int i = 0; i < 31; i++) {
    tester.emit(IGen::sub_gpr64_imm8s(tester.generator(), i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "0004009121040091420400916304009184040091A5040091C6040091E704009108050091290500914A0500"
            "916B0500918C050091AD050091CE050091EF0500911006009131060091520600917306009194060091B506"
            "0091D6060091F706009118070091390700915A0700917B0700919C070091BD070091DE070091");
}

TEST(CodeTester, add_gpr64_imm8_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  for (int i = 0; i < 16; i++) {
    tester.emit(IGen::add_gpr64_imm8s(tester.generator(), i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "4883C0FF4883C1FF4883C2FF4883C3FF4883C4FF4883C5FF4883C6FF4883C7FF4983C0FF4983C1FF4983C2"
            "FF4983C3FF4983C4FF4983C5FF4983C6FF4983C7FF");
}

TEST(CodeTester, add_gpr64_imm8_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  for (int i = 0; i < 31; i++) {
    tester.emit(IGen::add_gpr64_imm8s(tester.generator(), i, -1));
  }
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "000400D1210400D1420400D1630400D1840400D1A50400D1C60400D1E70400D1080500D1290500D14A0500"
            "D16B0500D18C0500D1AD0500D1CE0500D1EF0500D1100600D1310600D1520600D1730600D1940600D1B506"
            "00D1D60600D1F70600D1180700D1390700D15A0700D17B0700D19C0700D1BD0700D1DE0700D1");
}

TEST(CodeTester, simd_store_128_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  //  movdqa [rbx], xmm3
  //  movdqa [r14], xmm3
  //  movdqa [rbx], xmm14
  //  movdqa [r14], xmm13
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), RBX, XMM3));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), R14, XMM3));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), RBX, XMM14));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), R14, XMM13));
  EXPECT_EQ(tester.dump_to_hex_string(),
            "66 0f 7f 1b 66 41 0f 7f 1e 66 44 0f 7f 33 66 45 0f 7f 2e");

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), RSP, XMM1));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 7f 0c 24");  // requires SIB byte.

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), R12, XMM13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 7f 2c 24");  // requires SIB byte and REX byte

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), RBP, XMM1));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 7f 4d 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), RBP, XMM11));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 0f 7f 5d 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), R13, XMM2));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 41 0f 7f 55 00");

  tester.clear();
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), R13, XMM12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 7f 65 00");
}

TEST(CodeTester, simd_store_128_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);

  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), X2, V3));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), X14, V3));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), X2, V14));
  tester.emit(IGen::store128_gpr64_simd128(tester.generator(), X14, V13));
  EXPECT_EQ(tester.dump_to_hex_string(), "43 00 80 3d c3 01 80 3d 4e 00 80 3d cd 01 80 3d");
}

TEST(CodeTester, xmm_load_128_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);

  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM3, RBX));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM3, R14));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM14, RBX));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM13, R14));
  EXPECT_EQ(tester.dump_to_hex_string(),
            "66 0f 6f 1b 66 41 0f 6f 1e 66 44 0f 6f 33 66 45 0f 6f 2e");

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM1, RSP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 6f 0c 24");  // requires SIB byte.

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM13, R12));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 6f 2c 24");  // requires SIB byte and REX byte

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM1, RBP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 0f 6f 4d 00");

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM11, RBP));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 44 0f 6f 5d 00");

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM2, R13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 41 0f 6f 55 00");

  tester.clear();
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), XMM12, R13));
  EXPECT_EQ(tester.dump_to_hex_string(), "66 45 0f 6f 65 00");
}

TEST(CodeTester, xmm_load_128_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);

  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), V3, X1));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), V3, X14));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), V14, X1));
  tester.emit(IGen::load128_simd128_gpr64(tester.generator(), V13, X14));
  EXPECT_EQ(tester.dump_to_hex_string(), "23 00 c0 3d c3 01 c0 3d 2e 00 c0 3d cd 01 c0 3d");
}

void execute_tester(CodeTester& tester) {
  if (tester.generator().instr_set() == InstructionSet::ARM64) {
#ifdef __aarch64__
    tester.execute();
#endif
  } else if (tester.generator().instr_set() == InstructionSet::X86) {
#ifndef __aarch64__
    tester.execute();
#endif
  }
}

// These tests actually execute the code, you cannot execute arm64 code on x86 and vise versa
// so these tests have to be conditional based on the platform unfortunately.
TEST(CodeTester, execute_push_pop_simd_x86) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit_push_all_simd();
  tester.emit_pop_all_simd();
  tester.emit_return();
  EXPECT_EQ(
      tester.dump_to_hex_string(true),
      "4883EC084883EC10660F7F04244883EC10660F7F0C244883EC10660F7F14244883EC10660F7F1C244883EC10660F"
      "7F24244883EC10660F7F2C244883EC10660F7F34244883EC10660F7F3C244883EC1066440F7F04244883EC106644"
      "0F7F0C244883EC1066440F7F14244883EC1066440F7F1C244883EC1066440F7F24244883EC1066440F7F2C244883"
      "EC1066440F7F34244883EC1066440F7F3C2466440F6F3C244883C41066440F6F34244883C41066440F6F2C244883"
      "C41066440F6F24244883C41066440F6F1C244883C41066440F6F14244883C41066440F6F0C244883C41066440F6F"
      "04244883C410660F6F3C244883C410660F6F34244883C410660F6F2C244883C410660F6F24244883C410660F6F1C"
      "244883C410660F6F14244883C410660F6F0C244883C410660F6F04244883C4104883C408C3");
  execute_tester(tester);
}

TEST(CodeTester, execute_push_pop_simd_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  tester.emit_push_all_simd();
  tester.emit_pop_all_simd();
  tester.emit_return();
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "FF4300D1E003803DFF4300D1E103803DFF4300D1E203803DFF4300D1E303803DFF4300D1E403803DFF4300"
            "D1E503803DFF43"
            "00D1E603803DFF4300D1E703803DFF4300D1E803803DFF4300D1E903803DFF4300D1EA03803DFF4300D1EB"
            "03803DFF4300D1"
            "EC03803DFF4300D1ED03803DFF4300D1EE03803DFF4300D1EF03803DFF4300D1F003803DFF4300D1F10380"
            "3DFF4300D1F203"
            "803DFF4300D1F303803DFF4300D1F403803DFF4300D1F503803DFF4300D1F603803DFF4300D1F703803DFF"
            "4300D1F803803D"
            "FF4300D1F903803DFF4300D1FA03803DFF4300D1FB03803DFF4300D1FC03803DFF4300D1FD03803DFF4300"
            "D1FE03803DFF43"
            "00D1FF03803DFF03C03DFF430091FE03C03DFF430091FD03C03DFF430091FC03C03DFF430091FB03C03DFF"
            "430091FA03C03D"
            "FF430091F903C03DFF430091F803C03DFF430091F703C03DFF430091F603C03DFF430091F503C03DFF4300"
            "91F403C03DFF43"
            "0091F303C03DFF430091F203C03DFF430091F103C03DFF430091F003C03DFF430091EF03C03DFF430091EE"
            "03C03DFF430091"
            "ED03C03DFF430091EC03C03DFF430091EB03C03DFF430091EA03C03DFF430091E903C03DFF430091E803C0"
            "3DFF430091E703"
            "C03DFF430091E603C03DFF430091E503C03DFF430091E403C03DFF430091E303C03DFF430091E203C03DFF"
            "430091E103C03D"
            "FF430091E003C03DFF430091C0035FD6");
  execute_tester(tester);
}

TEST(CodeTester, execute_push_pop_all_the_things_x86) {
  CodeTester tester;
  tester.init_code_buffer(512);
  tester.emit_push_all_simd();
  tester.emit_push_all_gprs();

  // ...
  tester.emit_pop_all_gprs();
  tester.emit_pop_all_simd();
  tester.emit_return();
  EXPECT_EQ(
      tester.dump_to_hex_string(true),
      "4883EC084883EC10660F7F04244883EC10660F7F0C244883EC10660F7F14244883EC10660F7F1C244883EC10660F"
      "7F24244883EC10660F7F2C244883EC10660F7F34244883EC10660F7F3C244883EC1066440F7F04244883EC106644"
      "0F7F0C244883EC1066440F7F14244883EC1066440F7F1C244883EC1066440F7F24244883EC1066440F7F2C244883"
      "EC1066440F7F34244883EC1066440F7F3C24505152535455565741504151415241534154415541564157415F415E"
      "415D415C415B415A415941585F5E5D5C5B5A595866440F6F3C244883C41066440F6F34244883C41066440F6F2C24"
      "4883C41066440F6F24244883C41066440F6F1C244883C41066440F6F14244883C41066440F6F0C244883C4106644"
      "0F6F04244883C410660F6F3C244883C410660F6F34244883C410660F6F2C244883C410660F6F24244883C410660F"
      "6F1C244883C410660F6F14244883C410660F6F0C244883C410660F6F04244883C4104883C408C3");
  execute_tester(tester);
}

TEST(CodeTester, execute_push_pop_all_the_things_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  tester.emit_push_all_simd();
  tester.emit_push_all_gprs();

  // ...
  tester.emit_pop_all_gprs();
  tester.emit_pop_all_simd();
  tester.emit_return();
  EXPECT_EQ(tester.dump_to_hex_string(true),
            "FF4300D1E003803DFF4300D1E103803DFF4300D1E203803DFF4300D1E303803DFF4300D1E403803DFF4300"
            "D1E503803DFF43"
            "00D1E603803DFF4300D1E703803DFF4300D1E803803DFF4300D1E903803DFF4300D1EA03803DFF4300D1EB"
            "03803DFF4300D1"
            "EC03803DFF4300D1ED03803DFF4300D1EE03803DFF4300D1EF03803DFF4300D1F003803DFF4300D1F10380"
            "3DFF4300D1F203"
            "803DFF4300D1F303803DFF4300D1F403803DFF4300D1F503803DFF4300D1F603803DFF4300D1F703803DFF"
            "4300D1F803803D"
            "FF4300D1F903803DFF4300D1FA03803DFF4300D1FB03803DFF4300D1FC03803DFF4300D1FD03803DFF4300"
            "D1FE03803DFF43"
            "00D1FF03803DE00F1FF8E10F1FF8E20F1FF8E30F1FF8E40F1FF8E50F1FF8E60F1FF8E70F1FF8E80F1FF8E9"
            "0F1FF8EA0F1FF8"
            "EB0F1FF8EC0F1FF8ED0F1FF8EE0F1FF8EF0F1FF8F00F1FF8F10F1FF8F20F1FF8F30F1FF8F40F1FF8F50F1F"
            "F8F60F1FF8F70F"
            "1FF8F80F1FF8F90F1FF8FA0F1FF8FB0F1FF8FC0F1FF8FD0F1FF8FE0F1FF8FE0741F8FD0741F8FC0741F8FB"
            "0741F8FA0741F8"
            "F90741F8F80741F8F70741F8F60741F8F50741F8F40741F8F30741F8F20741F8F10741F8F00741F8EF0741"
            "F8EE0741F8ED07"
            "41F8EC0741F8EB0741F8EA0741F8E90741F8E80741F8E70741F8E60741F8E50741F8E40741F8E30741F8E2"
            "0741F8E10741F8"
            "E00741F8FF03C03DFF430091FE03C03DFF430091FD03C03DFF430091FC03C03DFF430091FB03C03DFF4300"
            "91FA03C03DFF43"
            "0091F903C03DFF430091F803C03DFF430091F703C03DFF430091F603C03DFF430091F503C03DFF430091F4"
            "03C03DFF430091"
            "F303C03DFF430091F203C03DFF430091F103C03DFF430091F003C03DFF430091EF03C03DFF430091EE03C0"
            "3DFF430091ED03"
            "C03DFF430091EC03C03DFF430091EB03C03DFF430091EA03C03DFF430091E903C03DFF430091E803C03DFF"
            "430091E703C03D"
            "FF430091E603C03DFF430091E503C03DFF430091E403C03DFF430091E303C03DFF430091E203C03DFF4300"
            "91E103C03DFF43"
            "0091E003C03DFF430091C0035FD6");
  execute_tester(tester);
}

TEST(CodeTester, execute_return_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  // test creating a function which simply returns
  tester.emit_return();
  EXPECT_EQ(tester.dump_to_hex_string(), "c3");
  // and execute it!
  execute_tester(tester);
}

TEST(CodeTester, execute_return_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  // test creating a function which simply returns
  tester.emit(IGen::add_gpr64_imm8s(tester.generator(), ARM64_REG::X0, 1));
  tester.emit(IGen::ret(tester.generator()));
  EXPECT_EQ(tester.dump_to_hex_string(), "00 04 00 91 c0 03 5f d6");
  // and execute it!
  execute_tester(tester);
}

TEST(CodeTester, execute_push_pop_gprs_x86) {
  CodeTester tester;
  tester.init_code_buffer(256);
  // test we can push/pop gprs without crashing.
  tester.emit_push_all_gprs();
  tester.emit_pop_all_gprs();
  tester.emit_return();
  EXPECT_EQ(tester.dump_to_hex_string(),
            "50 51 52 53 54 55 56 57 41 50 41 51 41 52 41 53 41 54 41 55 41 56 41 57 41 5f 41 5e "
            "41 5d 41 5c 41 5b 41 5a 41 59 41 58 5f 5e 5d 5c 5b 5a 59 58 c3");
  execute_tester(tester);
}

TEST(CodeTester, execute_push_pop_gprs_arm64) {
  CodeTester tester(emitter::InstructionSet::ARM64);
  tester.init_code_buffer(256);
  // test we can push/pop gprs without crashing.
  tester.emit_push_all_gprs();
  tester.emit_pop_all_gprs();
  tester.emit_return();
  EXPECT_EQ(tester.dump_to_hex_string(),
            "e0 0f 1f f8 e1 0f 1f f8 e2 0f 1f f8 e3 0f 1f f8 e4 0f 1f f8 e5 0f 1f f8 e6 0f 1f f8 "
            "e7 0f 1f f8 e8 0f 1f f8 e9 0f 1f f8 ea 0f 1f f8 eb 0f 1f f8 ec 0f 1f f8 ed 0f 1f f8 "
            "ee 0f 1f f8 ef 0f 1f f8 f0 0f 1f f8 f1 0f 1f f8 f2 0f 1f f8 f3 0f 1f f8 f4 0f 1f f8 "
            "f5 0f 1f f8 f6 0f 1f f8 f7 0f 1f f8 f8 0f 1f f8 f9 0f 1f f8 fa 0f 1f f8 fb 0f 1f f8 "
            "fc 0f 1f f8 fd 0f 1f f8 fe 0f 1f f8 fe 07 41 f8 fd 07 41 f8 fc 07 41 f8 fb 07 41 f8 "
            "fa 07 41 f8 f9 07 41 f8 f8 07 41 f8 f7 07 41 f8 f6 07 41 f8 f5 07 41 f8 f4 07 41 f8 "
            "f3 07 41 f8 f2 07 41 f8 f1 07 41 f8 f0 07 41 f8 ef 07 41 f8 ee 07 41 f8 ed 07 41 f8 "
            "ec 07 41 f8 eb 07 41 f8 ea 07 41 f8 e9 07 41 f8 e8 07 41 f8 e7 07 41 f8 e6 07 41 f8 "
            "e5 07 41 f8 e4 07 41 f8 e3 07 41 f8 e2 07 41 f8 e1 07 41 f8 e0 07 41 f8 c0 03 5f d6");
  execute_tester(tester);
}
