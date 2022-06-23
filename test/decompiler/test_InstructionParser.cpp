#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/Disasm/InstructionParser.h"
#include "gtest/gtest.h"

using namespace decompiler;

TEST(DecompilerInstructionParser, SimpleTest) {
  InstructionParser parser;
  std::vector<std::string> ops = {"daddu a0, a1, a2", "addu r0, t7, s6",  "daddiu r0, at, #t",
                                  "addiu t2, t3, 12", "slti v1, a3, -23", "sltiu s3, s4, 3",
                                  "sb v1, 12(a1)",    "sh s7, sym(s6)",   "sd s2, -12(s2)",
                                  "lw s3, 12(s7)",    "lwu t2, sym(s7)",  "add.s f0, f1, f2",
                                  "beq r0, r0, L312"};

  std::vector<DecompilerLabel> labels;
  labels.push_back(DecompilerLabel{"L311", 1, 2});
  labels.push_back(DecompilerLabel{"L312", 1, 2});
  labels.push_back(DecompilerLabel{"L313", 1, 2});
  for (auto& op : ops) {
    auto instr = parser.parse_single_instruction(op, labels);
    EXPECT_EQ(op, instr.to_string(labels));
  }
}

TEST(DecompilerInstructionParser, ProgramNoLabels) {
  InstructionParser parser;
  std::string program = "  daddu a0, a1, a2\n  sh s7, sym(s6)\n  sb v1, 12(a1)\n";
  auto result = parser.parse_program(program);
  EXPECT_EQ(result.print(), program);
}

TEST(DecompilerInstructionParser, ProgramLabels) {
  InstructionParser parser;
  std::string program =
      "L100:\n"
      "  daddu v0, v1, v0\n"
      "  beq at, r0, L102\n"
      "L102:\n"
      "  jr ra\n";
  auto result = parser.parse_program(program);
  EXPECT_EQ(result.print(), program);
}

TEST(DecompilerInstructionParser, VuMask) {
  InstructionParser parser;
  std::string program =
      "  vmove.xy vf1, vf2\n"
      "  vsub.yw vf1, vf2, vf25\n";
  auto result = parser.parse_program(program);
  EXPECT_EQ(result.print(), program);
}

TEST(DecompilerInstructionParser, VuMoves) {
  InstructionParser parser;
  std::string program =
      "  qmtc2.i vf2, a2\n"
      "  qmfc2.i v0, vf1\n";
  auto result = parser.parse_program(program);
  EXPECT_EQ(result.print(), program);
}

TEST(DecompilerInstructionParser, VuBroadcast) {
  InstructionParser parser;
  std::string program = "  vmuly.xyw vf1, vf1, vf2\n";
  auto result = parser.parse_program(program);
  EXPECT_EQ(result.print(), program);
}