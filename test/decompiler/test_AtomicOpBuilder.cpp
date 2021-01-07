#include "gtest/gtest.h"
#include "decompiler/IR2/AtomicOp.h"
#include "decompiler/IR2/AtomicOpBuilder.h"
#include "decompiler/Disasm/InstructionParser.h"

using namespace decompiler;
TEST(DecompilerAtomicOpBuilder, Example) {
  InstructionParser parser;

  // some MIPS instructions. Can be a sequence of instructions, possibly with labels.
  std::string input_program =
      "and v0, v1, a3\n"
      "and a1, a2, a2";

  // convert to Instructions:
  ParsedProgram prg = parser.parse_program(input_program);

  // this verifies we can convert from a string to an instruction, and back to a string again.
  // the instruction printer will add two leading spaces and a newline.
  EXPECT_EQ(prg.print(), "  and v0, v1, a3\n  and a1, a2, a2\n");

  // next, set up a test environment for the conversion. The FunctionAtomicOps will hold
  // the result of the conversion
  FunctionAtomicOps container;

  // treat the entire program as a single basic block, and convert!
  convert_block_to_atomic_ops(0, prg.instructions.begin(), prg.instructions.end(), prg.labels,
                              &container);

  // we should get back a single and operation:
  EXPECT_EQ(2, container.ops.size());

  // for now, we create an empty environment. The environment will be used in the future to
  // rename register to variables, but for now, we just leave it empty and the printing will
  // use register names
  Env env;

  // check the we get the right result:
  EXPECT_EQ(container.ops.at(0)->to_string(prg.labels, &env), "(set! v0 (logand v1 a3))");
  EXPECT_EQ(container.ops.at(1)->to_string(prg.labels, &env), "(set! a1 (logand a2 a2))");

  // check that the registers read/written are identified for the first op (and v0, v1, a3)
  auto& first_op = container.ops.at(0);

  // two registers read (v1 and a3)
  EXPECT_EQ(first_op->read_regs().size(), 2);
  // one register written (v0)
  EXPECT_EQ(first_op->write_regs().size(), 1);
  // no clobber registers (register which ends up with a garbage value in it)
  EXPECT_EQ(first_op->clobber_regs().size(), 0);

  // the ordering of the two read registers doesn't matter. It happens to be in the same order
  // as the opcode here, but it may not always be the case.
  EXPECT_EQ(first_op->read_regs().at(0).to_string(), "v1");
  EXPECT_EQ(first_op->read_regs().at(1).to_string(), "a3");
  EXPECT_EQ(first_op->write_regs().at(0).to_string(), "v0");
}