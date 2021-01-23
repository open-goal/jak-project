#include "gtest/gtest.h"
#include "FormRegressionTest.h"

using namespace decompiler;

TEST_F(FormRegressionTest, ExprIdentity) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object)";
  std::string expected = "a0-0";
  test_with_expr(func, type, expected);
}

// TEST_F(FormRegressionTest, ExprFloatingPoint) {
//  std::string func =
//      "    sll r0, r0, 0\n"
//      "L345:\n"
//      "    daddiu sp, sp, -16\n"
//      "    sd fp, 8(sp)\n"
//      "    or fp, t9, r0\n"
//      "    lwc1 f0, L345(fp)\n"
//      "    mtc1 f1, a0\n"
//      "    div.s f0, f0, f1\n"
//      "    mfc1 v0, f0\n"
//      "    ld fp, 8(sp)\n"
//      "    jr ra\n"
//      "    daddiu sp, sp, 16";
//  std::string type = "(function float float)";
//  std::string expected =
//      "(begin\n"
//      "  (set! f0-0 (l.f L345))\n"
//      "  (set! f1-0 (gpr->fpr a0-0))\n"
//      "  (set! f0-1 (/.s f0-0 f1-0))\n"
//      "  (set! v0-0 (fpr->gpr f0-1))\n"
//      "  )";
//  test_with_expr(func, type, expected);
//}