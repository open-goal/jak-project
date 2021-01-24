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

TEST_F(FormRegressionTest, ExprFloatingPoint) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L345:\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    lwc1 f0, L345(fp)\n"
      "    mtc1 f1, a0\n"
      "    div.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function float float)";
  std::string expected = "(/ (l.f L345) a0-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionSigned) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(+ a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionUnSigned) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(+ a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionMixed1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(+ a0-0 (the-as int a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionMixed2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int uint)";
  std::string expected = "(+ a0-0 (the-as uint a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionSignedWrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int uint)";
  std::string expected = "(the-as uint (+ a0-0 a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionUnSignedWrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint int)";
  std::string expected = "(the-as int (+ a0-0 a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionMixed1WrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint uint)";
  std::string expected = "(the-as uint (+ a0-0 (the-as int a1-0)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAdditionMixed2WrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(the-as int (+ a0-0 (the-as uint a1-0)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprSubtraction) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    dsubu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(- a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMultiplication) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(* a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMultiplicationWrong1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(* a0-0 (the-as int a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMultiplicationWrong2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(* (the-as int a0-0) a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMultiplicationWrong3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(the-as uint (* (the-as int a0-0) (the-as int a1-0)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprDivision1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(/ a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprDivision2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(/ (the-as int a0-0) a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprDivision3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(/ a0-0 (the-as int a1-0))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprDivision4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(the-as uint (/ (the-as int a0-0) (the-as int a1-0)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAsh) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L305:\n"
      "    or v1, a0, r0\n"
      "    bgezl a1, L306\n"
      "    dsllv v0, v1, a1\n"

      "    dsubu a0, r0, a1\n"
      "    dsrav v0, v1, a0\n"
      "L306:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(ash a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMod) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mfhi v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(mod a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprAbs) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L301:\n"
      "    or v0, a0, r0\n"
      "    bltzl v0, L302\n"
      "    dsubu v0, r0, v0\n"

      "L302:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int)";
  std::string expected = "(abs a0-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMin) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movz v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(min a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMax) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movn v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(max a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLogior) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logior a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLogxor) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    xor v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logxor a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLognor) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    nor v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(lognor a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLogand) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    and v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logand a0-0 a1-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLognot) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    nor v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int)";
  std::string expected = "(lognot a0-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprFalse) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function symbol)";
  std::string expected = "'#f";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprTrue) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu v0, s7, #t\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function symbol)";
  std::string expected = "'#t";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprPrintBfloat) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L343:\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L343\n"
      "    lwc1 f0, 0(gp)\n"
      "    mfc1 a2, f0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0 \n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function bfloat bfloat)";

  std::string expected = "(begin (set! gp-0 a0-0) (format '#t L343 (-> gp-0 data)) gp-0)";
  test_with_expr(func, type, expected, false, "", {{"L343", "~f"}});
}

TEST_F(FormRegressionTest, ExprSizeOfType) {
  std::string func =
      "L346:\n"  // fake label.
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    ld v1, L346(fp)\n"
      "    lhu a0, 14(a0)\n"
      "    dsll a0, a0, 2\n"
      "    daddiu a0, a0, 43\n"
      "    and v0, v1, a0 \n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function type uint)";

  std::string expected = "(logand (l.d L346) (+ (sll (-> a0-1 allocated-length) 2) 43))";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTest, ExprBasicTypeP) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L285:\n"
      "    lwu v1, -4(a0)\n"
      "    lw a0, object(s7)\n"

      "L286:\n"
      "    bne v1, a1, L287\n"
      "    or a2, s7, r0\n"

      "    daddiu v1, s7, #t\n"
      "    or v0, v1, r0\n"
      "    beq r0, r0, L288\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"
      "L287:\n"
      "    lwu v1, 4(v1)\n"
      "    bne v1, a0, L286\n"
      "    sll r0, r0, 0\n"
      "    or v0, s7, r0\n"
      "L288:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function basic type symbol)";
  std::string expected =
      "(begin\n"
      "  (set! v1-0 (-> a0-0 type))\n"
      "  (set! a0-1 object)\n"
      "  (until\n"
      "   (begin (set! v1-0 (-> v1-0 parent)) (= v1-0 a0-1))\n"  // likely using set! as value. we
                                                                 // don't plan on supporting this.
      "   (if\n"
      "    (= v1-0 a1-0)\n"
      "    (return ((begin (set! v1-1 '#t) (set! v0-0 v1-1))) ((set! v1-0 0)))\n"
      "    )\n"
      "   )\n"
      "  '#f\n"
      "  )";
  test_with_expr(func, type, expected);
}