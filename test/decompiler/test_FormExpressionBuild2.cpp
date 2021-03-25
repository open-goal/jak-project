#include "gtest/gtest.h"
#include "FormRegressionTest.h"

using namespace decompiler;

TEST_F(FormRegressionTest, MatrixPMult) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -112\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 80(sp)\n"
      "    sq gp, 96(sp)\n"

      "    or gp, a0, r0\n"
      "    daddiu s5, sp, 16\n"
      "    sq r0, 0(s5)\n"
      "    sq r0, 16(s5)\n"
      "    sq r0, 32(s5)\n"
      "    sq r0, 48(s5)\n"
      "    lw t9, matrix*!(s7)\n"
      "    or a0, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lq v1, 0(s5)\n"
      "    sq v1, 0(gp)\n"
      "    lq v1, 16(s5)\n"
      "    sq v1, 16(gp)\n"
      "    lq v1, 32(s5)\n"
      "    sq v1, 32(gp)\n"
      "    lq v1, 48(s5)\n"
      "    sq v1, 48(gp)\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function matrix matrix matrix)";
  std::string expected =
      "(let ((v0-0 arg0))\n"
      "  (let* ((v1-1 (sar (+ arg2 15) 4))\n"
      "         (a0-1 (&+ arg0 (shl v1-1 4)))\n"
      "         (a1-1 (&+ arg1 (shl v1-1 4)))\n"
      "         )\n"
      "   (while (nonzero? v1-1)\n"
      "    (+! v1-1 -1)\n"
      "    (&+! a0-1 -16)\n"
      "    (&+! a1-1 -16)\n"
      "    (set!\n"
      "     (-> (the-as (pointer uint128) a0-1))\n"
      "     (-> (the-as (pointer uint128) a1-1))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}