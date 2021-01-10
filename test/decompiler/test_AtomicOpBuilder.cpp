#include "gtest/gtest.h"
#include "decompiler/IR2/AtomicOp.h"
#include "decompiler/IR2/AtomicOpBuilder.h"
#include "decompiler/Disasm/InstructionParser.h"
#include "third-party/fmt/core.h"
#include "third-party/fmt/format.h"
#include <regex>

using namespace decompiler;

std::regex labelRegex("^L\\d+:\\s*$");

// Auto indents / adds new-lines to a list of assembly lines
std::string assembly_from_list(std::vector<std::string> assemblyLines) {
  std::string str = "";
  for (std::string line : assemblyLines) {
    if (std::regex_match(line, labelRegex)) {
      str += fmt::format("{}\n", line);
    } else {
      str += fmt::format("  {}\n", line);
    }
  }
  return str;
}

void test_case(std::string assembly_lines,
               std::vector<std::string> output_lines,
               std::vector<std::vector<std::string>> write_regs,
               std::vector<std::vector<std::string>> read_regs,
               std::vector<std::vector<std::string>> clobbered_regs) {
  InstructionParser parser;
  // convert to Instructions:
  // some MIPS instructions. Can be a sequence of instructions, possibly with labels.
  ParsedProgram prg = parser.parse_program(assembly_lines);

  // this verifies we can convert from a string to an instruction, and back to a string again.
  // the instruction printer will add two leading spaces and a newline.
  EXPECT_EQ(prg.print(), assembly_lines);

  // next, set up a test environment for the conversion. The FunctionAtomicOps will hold
  // the result of the conversion
  FunctionAtomicOps container;

  // treat the entire program as a single basic block, and convert!
  convert_block_to_atomic_ops(0, prg.instructions.begin(), prg.instructions.end(), prg.labels,
                              &container);

  // count operations
  EXPECT_EQ(container.ops.size(), output_lines.size());

  // for now, we create an empty environment. The environment will be used in the future to
  // rename register to variables, but for now, we just leave it empty and the printing will
  // use register names
  Env env;

  // check the we get the right result:
  for (size_t i = 0; i < container.ops.size(); i++) {
    const auto& op = container.ops.at(i);
    EXPECT_EQ(op->to_string(prg.labels, &env), output_lines.at(i));

    // check that the registers read/written are identified for the operation

    // check write registers
    EXPECT_EQ(op->write_regs().size(), write_regs.at(i).size());
    for (size_t j = 0; j < op->write_regs().size(); j++) {
      const std::string expected_reg = op->write_regs().at(j).to_string();
      // the ordering of the registers doesn't matter. It could happen to be in the same order
      // as the opcode here, but it may not always be the case.
      bool found = false;
      for (const std::string reg : write_regs.at(i)) {
        // TODO - is there a potential bug here in the event that either list has duplicate
        // registers?
        if (reg == expected_reg) {
          found = true;
          break;
        }
      }
      EXPECT_TRUE(found) << fmt::format("Unable to find expected WRITE register - {}",
                                        expected_reg);
    }

    // check read registers
    EXPECT_EQ(op->read_regs().size(), read_regs.at(i).size());
    for (size_t j = 0; j < op->read_regs().size(); j++) {
      const std::string expected_reg = op->read_regs().at(j).to_string();
      // the ordering of the registers doesn't matter. It could happen to be in the same order
      // as the opcode here, but it may not always be the case.
      bool found = false;
      for (const std::string reg : read_regs.at(i)) {
        // TODO - is there a potential bug here in the event that either list has duplicate
        // registers?
        if (reg == expected_reg) {
          found = true;
          break;
        }
      }
      EXPECT_TRUE(found) << fmt::format("Unable to find expected READ register - {}", expected_reg);
    }

    // check clobbered registers
    EXPECT_EQ(op->clobber_regs().size(), clobbered_regs.at(i).size());
    for (size_t j = 0; j < op->clobber_regs().size(); j++) {
      const std::string expected_reg = op->clobber_regs().at(j).to_string();
      // the ordering of the registers doesn't matter. It could happen to be in the same order
      // as the opcode here, but it may not always be the case.
      bool found = false;
      for (const std::string reg : clobbered_regs.at(i)) {
        // TODO - is there a potential bug here in the event that either list has duplicate
        // registers?
        if (reg == expected_reg) {
          found = true;
          break;
        }
      }
      EXPECT_TRUE(found) << fmt::format("Unable to find expected CLOBBERED register - {}",
                                        expected_reg);
    }
  }
}

TEST(DecompilerAtomicOpBuilder, Example) {
  test_case(assembly_from_list({"and v0, v1, a3", "and a1, a2, a2"}),
            {"(set! v0 (logand v1 a3))", "(set! a1 (logand a2 a2))"}, {{"v0"}, {"a1"}},
            {{"v1", "a3"}, {"a2", "a2"}}, {{}, {}});
}

TEST(DecompilerAtomicOpBuilder, ABS_S) {
  test_case(assembly_from_list({"abs.s f1, f2"}), {"(set! f1 (abs.s f2))"}, {{"f1"}}, {{"f2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, ADDIU) {
  test_case(assembly_from_list({"addiu a1, r0, 12"}), {"(set! a1 12)"}, {{"a1"}}, {{}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, ADD_S) {
  test_case(assembly_from_list({"add.s f1, f2, f3"}), {"(set! f1 (+.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, AND) {
  test_case(assembly_from_list({"and a1, a2, a3"}), {"(set! a1 (logand a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, ANDI) {
  test_case(assembly_from_list({"andi a1, a2, 1234"}), {"(set! a1 (logand a2 1234))"}, {{"a1"}},
            {{"a2"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, CVT_S_W) {
  test_case(assembly_from_list({"cvt.s.w f1, f2"}), {"(set! f1 (i2f f2))"}, {{"f1"}}, {{"f2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, CVT_W_S) {
  test_case(assembly_from_list({"cvt.w.s f1, f2"}), {"(set! f1 (f2i f2))"}, {{"f1"}}, {{"f2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DADDIU) {
  test_case(assembly_from_list({"daddiu a1, s7, test"}), {"(set! a1 'test)"}, {{"a1"}}, {{}}, {{}});
  test_case(assembly_from_list({"daddiu a1, s7, -10"}), {"(set! a1 '())"}, {{"a1"}}, {{}}, {{}});
  test_case(assembly_from_list({"daddiu a1, s7, -32768"}), {"(set! a1 __START-OF-TABLE__)"},
            {{"a1"}}, {{}}, {{}});
  test_case(assembly_from_list({"daddiu a1, s7, 8"}), {"(set! a1 '#t)"}, {{"a1"}}, {{}}, {{}});
  test_case(assembly_from_list({"L123:", "daddiu a1, fp, L123"}), {"(set! a1 L123)"}, {{"a1"}},
            {{}}, {{}});
  test_case(assembly_from_list({"daddiu a1, a2, 1234"}), {"(set! a1 (+ a2 1234))"}, {{"a1"}},
            {{"a2"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DADDU) {
  test_case(assembly_from_list({"daddu a1, a2, a3"}), {"(set! a1 (+ a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DIV_S) {
  test_case(assembly_from_list({"div.s f1, f2, f3"}), {"(set! f1 (/.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DSLL) {
  test_case(assembly_from_list({"dsll a2, a3, 3"}), {"(set! a2 (shl a3 3))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSLL32) {
  test_case(assembly_from_list({"dsll32 a2, a3, 3"}), {"(set! a2 (shl a3 35))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSLLV) {
  test_case(assembly_from_list({"dsllv a1, a2, a3"}), {"(set! a1 (shl a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRA) {
  test_case(assembly_from_list({"dsra a2, a3, 3"}), {"(set! a2 (sra a3 3))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRA32) {
  test_case(assembly_from_list({"dsra32 a2, a3, 3"}), {"(set! a2 (sra a3 35))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRAV) {
  test_case(assembly_from_list({"dsrav a1, a2, a3"}), {"(set! a1 (sra a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRL) {
  test_case(assembly_from_list({"dsrl a2, a3, 3"}), {"(set! a2 (srl a3 3))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRL32) {
  test_case(assembly_from_list({"dsrl32 a2, a3, 3"}), {"(set! a2 (srl a3 35))"}, {{"a2"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, DSRLV) {
  test_case(assembly_from_list({"dsrlv a1, a2, a3"}), {"(set! a1 (srl a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, DSUBU) {
  test_case(assembly_from_list({"dsubu a1, a2, a3"}), {"(set! a1 (- a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
  test_case(assembly_from_list({"dsubu a1, r0, a3"}), {"(set! a1 (- a3))"}, {{"a1"}}, {{"a3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, LB) {
  test_case(assembly_from_list({"L123:", "lb a3, L123(fp)"}), {"(set! a3 (l.b L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lb a2, 0(a3)"}), {"(set! a2 (l.b a3))"}, {{"a2"}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"lb a2, 12(a3)"}), {"(set! a2 (l.b (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LBU) {
  test_case(assembly_from_list({"L123:", "lbu a3, L123(fp)"}), {"(set! a3 (l.bu L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lbu a2, 0(a3)"}), {"(set! a2 (l.bu a3))"}, {{"a2"}}, {{"a3"}},
            {{}});
  test_case(assembly_from_list({"lbu a2, 12(a3)"}), {"(set! a2 (l.bu (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LD) {
  test_case(assembly_from_list({"L123:", "ld a3, L123(fp)"}), {"(set! a3 (l.d L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"ld a2, 0(a3)"}), {"(set! a2 (l.d a3))"}, {{"a2"}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"ld a2, 12(a3)"}), {"(set! a2 (l.d (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LH) {
  test_case(assembly_from_list({"L123:", "lh a3, L123(fp)"}), {"(set! a3 (l.h L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lh a2, 0(a3)"}), {"(set! a2 (l.h a3))"}, {{"a2"}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"lh a2, 12(a3)"}), {"(set! a2 (l.h (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LHU) {
  test_case(assembly_from_list({"L123:", "lhu a3, L123(fp)"}), {"(set! a3 (l.hu L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lhu a2, 0(a3)"}), {"(set! a2 (l.hu a3))"}, {{"a2"}}, {{"a3"}},
            {{}});
  test_case(assembly_from_list({"lhu a2, 12(a3)"}), {"(set! a2 (l.hu (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LUI) {
  test_case(assembly_from_list({"lui a3, 2"}), {"(set! a3 131072)"}, {{"a3"}}, {{}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LW) {
  test_case(assembly_from_list({"lw r0, 2(r0)"}), {"(break!)"}, {{}}, {{}}, {{}});
  test_case(assembly_from_list({"lw a2, test(s7)"}), {"(set! a2 test)"}, {{"a2"}}, {{}}, {{}});
  test_case(assembly_from_list({"L123:", "lw a3, L123(fp)"}), {"(set! a3 (l.w L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lw a2, 0(a3)"}), {"(set! a2 (l.w a3))"}, {{"a2"}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"lw a2, 12(a3)"}), {"(set! a2 (l.w (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LWC1) {
  test_case(assembly_from_list({"L123:", "lwc1 f3, L123(fp)"}), {"(set! f3 (l.f L123))"}, {{"f3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lwc1 f2, 0(a3)"}), {"(set! f2 (l.f a3))"}, {{"f2"}}, {{"a3"}},
            {{}});
  test_case(assembly_from_list({"lwc1 f2, 12(a3)"}), {"(set! f2 (l.f (+ a3 12)))"}, {{"f2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, LWU) {
  test_case(assembly_from_list({"L123:", "lwu a3, L123(fp)"}), {"(set! a3 (l.wu L123))"}, {{"a3"}},
            {{}}, {{}});
  test_case(assembly_from_list({"lwu a2, 0(a3)"}), {"(set! a2 (l.wu a3))"}, {{"a2"}}, {{"a3"}},
            {{}});
  test_case(assembly_from_list({"lwu a2, 12(a3)"}), {"(set! a2 (l.wu (+ a3 12)))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MAX_S) {
  test_case(assembly_from_list({"max.s f1, f2, f3"}), {"(set! f1 (max.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MFC1) {
  test_case(assembly_from_list({"mfc1 a1, f3"}), {"(set! a1 (fpr->gpr f3))"}, {{"a1"}}, {{"f3"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, MIN_S) {
  test_case(assembly_from_list({"min.s f1, f2, f3"}), {"(set! f1 (min.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MOVN) {
  test_case(assembly_from_list({"movn a1, s7, a2"}), {"(cmove-#f-nonzero a1 a2)"}, {{"a1"}},
            {{"a2"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MOVZ) {
  test_case(assembly_from_list({"movz a1, s7, a2"}), {"(cmove-#f-zero a1 a2)"}, {{"a1"}}, {{"a2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, MOV_S) {
  test_case(assembly_from_list({"mov.s f1, f2"}), {"(set! f1 f2)"}, {{"f1"}}, {{"f2"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MTC1) {
  test_case(assembly_from_list({"mtc1 f3, a1"}), {"(set! f3 (gpr->fpr a1))"}, {{"f3"}}, {{"a1"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, MULT3) {
  test_case(assembly_from_list({"mult3 a1, a2, a3"}), {"(set! a1 (*.si a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MULTU3) {
  test_case(assembly_from_list({"multu3 a1, a2, a3"}), {"(set! a1 (*.ui a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, MUL_S) {
  test_case(assembly_from_list({"mul.s f1, f2, f3"}), {"(set! f1 (*.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, NEG_S) {
  test_case(assembly_from_list({"neg.s f1, f2"}), {"(set! f1 (neg.s f2))"}, {{"f1"}}, {{"f2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, NOR) {
  test_case(assembly_from_list({"nor a1, a2, r0"}), {"(set! a1 (lognot a2))"}, {{"a1"}}, {{"a2"}},
            {{}});
  test_case(assembly_from_list({"nor a1, a2, a3"}), {"(set! a1 (lognor a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, OR) {
  test_case(assembly_from_list({"or a1, a2, a3"}), {"(set! a1 (logior a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
  test_case(assembly_from_list({"or a2, r0, r0"}), {"(set! a2 0)"}, {{"a2"}}, {{}}, {{}});
  test_case(assembly_from_list({"or a1, s7, r0"}), {"(set! a1 '#f)"}, {{"a1"}}, {{}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, ORI) {
  test_case(assembly_from_list({"ori a2, r0, 1234"}), {"(set! a2 1234)"}, {{"a2"}}, {{}}, {{}});
  test_case(assembly_from_list({"ori a2, r0, -1234"}), {"(set! a2 -1234)"}, {{"a2"}}, {{}}, {{}});
  test_case(assembly_from_list({"ori a2, a3, -1234"}), {"(set! a2 (logior a3 -1234))"}, {{"a2"}},
            {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SB) {
  test_case(assembly_from_list({"sb a1, 2(a3)"}), {"(s.b! (+ a3 2) a1)"}, {{}}, {{"a1", "a3"}},
            {{}});
  test_case(assembly_from_list({"sb a1, 0(a3)"}), {"(s.b! a3 a1)"}, {{}}, {{"a1", "a3"}}, {{}});
  test_case(assembly_from_list({"sb s7, 2(a3)"}), {"(s.b! (+ a3 2) '#f)"}, {{}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"sb s7, 0(a3)"}), {"(s.b! a3 '#f)"}, {{}}, {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SD) {
  test_case(assembly_from_list({"sd a1, 2(a3)"}), {"(s.d! (+ a3 2) a1)"}, {{}}, {{"a1", "a3"}},
            {{}});
  test_case(assembly_from_list({"sd a1, 0(a3)"}), {"(s.d! a3 a1)"}, {{}}, {{"a1", "a3"}}, {{}});
  test_case(assembly_from_list({"sd s7, 2(a3)"}), {"(s.d! (+ a3 2) '#f)"}, {{}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"sd s7, 0(a3)"}), {"(s.d! a3 '#f)"}, {{}}, {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SH) {
  test_case(assembly_from_list({"sh a1, 2(a3)"}), {"(s.h! (+ a3 2) a1)"}, {{}}, {{"a1", "a3"}},
            {{}});
  test_case(assembly_from_list({"sh a1, 0(a3)"}), {"(s.h! a3 a1)"}, {{}}, {{"a1", "a3"}}, {{}});
  test_case(assembly_from_list({"sh s7, 2(a3)"}), {"(s.h! (+ a3 2) '#f)"}, {{}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"sh s7, 0(a3)"}), {"(s.h! a3 '#f)"}, {{}}, {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SLL) {
  test_case(assembly_from_list({"sll r0, r0, 0"}), {"(nop!)"}, {{}}, {{}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SQRT_S) {
  test_case(assembly_from_list({"sqrt.s f1, f2"}), {"(set! f1 (sqrt.s f2))"}, {{"f1"}}, {{"f2"}},
            {{}});
}

TEST(DecompilerAtomicOpBuilder, SUB_S) {
  test_case(assembly_from_list({"sub.s f1, f2, f3"}), {"(set! f1 (-.s f2 f3))"}, {{"f1"}},
            {{"f2", "f3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SW) {
  test_case(assembly_from_list({"sw a1, test(s7)"}), {"(s.w! test a1)"}, {{}}, {{"a1"}}, {{}});
  test_case(assembly_from_list({"sw s7, test(s7)"}), {"(s.w! test '#f)"}, {{}}, {{}}, {{}});
  test_case(assembly_from_list({"sw a1, 2(a3)"}), {"(s.w! (+ a3 2) a1)"}, {{}}, {{"a1", "a3"}},
            {{}});
  test_case(assembly_from_list({"sw a1, 0(a3)"}), {"(s.w! a3 a1)"}, {{}}, {{"a1", "a3"}}, {{}});
  test_case(assembly_from_list({"sw s7, 2(a3)"}), {"(s.w! (+ a3 2) '#f)"}, {{}}, {{"a3"}}, {{}});
  test_case(assembly_from_list({"sw s7, 0(a3)"}), {"(s.w! a3 '#f)"}, {{}}, {{"a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, SWC1) {
  test_case(assembly_from_list({"swc1 f2, 2(a3)"}), {"(s.f! (+ a3 2) f2)"}, {{}}, {{"f2", "a3"}},
            {{}});
  test_case(assembly_from_list({"swc1 f2, 0(a3)"}), {"(s.f! a3 f2)"}, {{}}, {{"f2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, XOR) {
  test_case(assembly_from_list({"xor a1, a2, a3"}), {"(set! a1 (logxor a2 a3))"}, {{"a1"}},
            {{"a2", "a3"}}, {{}});
}

TEST(DecompilerAtomicOpBuilder, XORI) {
  test_case(assembly_from_list({"xori a1, a2, 1234"}), {"(set! a1 (logxor a2 1234))"}, {{"a1"}},
            {{"a2"}}, {{}});
}
