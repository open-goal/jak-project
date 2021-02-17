#include "gtest/gtest.h"
#include "decompiler/Disasm/InstructionDecode.h"
#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

using namespace decompiler;

TEST(InstructionDecode, VDIV) {
  init_opcode_info();
  LinkedObjectFile file;
  u32 vdiv_test = 0b010010'1'10'01'10100'01010'01110111100;
  //                 ......   z  y  20    10
  LinkedWord vdiv_word(vdiv_test);
  auto instr = decode_instruction(vdiv_word, file, 0, 0);
  EXPECT_EQ(instr.to_string({}), "vdiv Q, vf10.y, vf20.z");
}

TEST(InstructionDecode, VRSQRT) {
  init_opcode_info();
  LinkedObjectFile file;
  u32 vdiv_test = 0b010010'1'10'01'10100'01010'01110111110;
  //                 ......   z  y  20    10
  LinkedWord vdiv_word(vdiv_test);
  auto instr = decode_instruction(vdiv_word, file, 0, 0);
  EXPECT_EQ(instr.to_string({}), "vrsqrt Q, vf10.y, vf20.z");
}

TEST(InstructionDecode, VSQRT) {
  init_opcode_info();
  LinkedObjectFile file;
  u32 vdiv_test = 0b010010'1'10'00'10100'00000'01110111101;
  //                 ......   z  X  20    X
  LinkedWord vdiv_word(vdiv_test);
  auto instr = decode_instruction(vdiv_word, file, 0, 0);
  EXPECT_EQ(instr.to_string({}), "vsqrt Q, vf20.z");
}