#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

TEST_F(FormRegressionTestJak1, ExprTruncate) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mtc1 f0, a0\n"
      "    cvt.w.s f0, f0\n"
      "    cvt.s.w f0, f0\n"
      "    mfc1 v0, f0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function float float)";
  std::string expected = "(the float (the int arg0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprIntegralP) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mtc1 f0, a0\n"
      "    cvt.w.s f0, f0\n"
      "    cvt.s.w f0, f0\n"
      "    mfc1 v1, f0\n"
      "    mtc1 f0, v1\n"
      "    mtc1 f1, a0\n"
      "    c.eq.s f0, f1\n"
      "    bc1t L31\n"
      "    daddiu v0, s7, 8\n"

      "    or v0, s7, r0\n"
      "L31:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function float float)";
  std::string expected = "(the-as float (= (the float (the int arg0)) arg0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprFractionalPart) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mtc1 f0, a0\n"
      "    mtc1 f1, a0\n"
      "    cvt.w.s f1, f1\n"
      "    cvt.s.w f1, f1\n"
      "    mfc1 v1, f1\n"
      "    mtc1 f1, v1\n"
      "    sub.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function float float)";
  std::string expected = "(- arg0 (the float (the int arg0)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprSeek) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L24:\n"
      "    mtc1 f0, a0\n"
      "    mtc1 f3, a1\n"
      "    mtc1 f1, a2\n"
      "    sub.s f2, f3, f0\n"
      "    abs.s f4, f2\n"
      "    c.lt.s f1, f4\n"
      "    bc1t L25\n"
      "    sll r0, r0, 0\n"

      "    mfc1 v0, f3\n"
      "    beq r0, r0, L27\n"
      "    sll r0, r0, 0\n"

      "L25:\n"
      "    mtc1 f3, r0\n"
      "    c.lt.s f2, f3\n"
      "    bc1t L26\n"
      "    sll r0, r0, 0\n"

      "    add.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    beq r0, r0, L27\n"
      "    sll r0, r0, 0\n"

      "L26:\n"
      "    sub.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"

      "L27:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function float float float float)";
  std::string expected =
      "(let\n"
      "  ((f2-0 (- arg1 arg0)))\n"
      "  (cond\n"
      "   ((>= arg2 (fabs f2-0)) arg1)\n"
      "   ((>= f2-0 0.0) (+ arg0 arg2))\n"
      "   (else (- arg0 arg2))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected);
}
