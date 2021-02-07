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

  std::string expected = "(logand (l.d L346) (+ (shl (-> a0-0 allocated-length) 2) 43))";
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
      "    (return '#t)\n"
      "    )\n"
      "   )\n"
      "  '#f\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, FinalBasicTypeP) {
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
      "(defun test-function ((a0-0 basic) (a1-0 type))\n"
      "  (local-vars\n"
      "   (v1-0 type)\n"
      "   (a0-1 type)\n"
      "   (a2-0 symbol)\n"
      "   )\n"
      "  (begin\n"
      "   (set! v1-0 (-> a0-0 type))\n"
      "   (set! a0-1 object)\n"
      "   (until\n"
      "    (begin (set! v1-0 (-> v1-0 parent)) (= v1-0 a0-1))\n"
      "    (if (= v1-0 a1-0) (return (quote #t)))\n"
      "    )\n"
      "   (quote #f)\n"
      "   )\n"
      "  )";
  test_final_function(func, type, expected);
}

TEST_F(FormRegressionTest, ExprTypeTypep) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L280:\n"
      "    lw v1, object(s7)\n"

      "L281:\n"
      "    bne a0, a1, L282\n"
      "    or a2, s7, r0\n"

      "    daddiu v1, s7, #t\n"
      "    or v0, v1, r0\n"
      "    beq r0, r0, L284\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"

      "L282:\n"
      "    lwu a0, 4(a0)\n"
      "    dsubu a2, a0, v1\n"
      "    daddiu a3, s7, 8\n"
      "    movn a3, s7, a2\n"
      "    bnel s7, a3, L283\n"
      "    or a2, a3, r0\n"

      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a0\n"

      "L283:\n"
      "    beq s7, a2, L281\n"
      "    sll r0, r0, 0\n"

      "    or v0, s7, r0\n"

      "L284:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function type type symbol)";

  std::string expected =
      "(begin\n"
      "  (set! v1-0 object)\n"
      "  (until\n"
      "   (begin\n"
      "    (set! a0-0 (-> a0-0 parent))\n"
      "    (or (= a0-0 v1-0) (zero? a0-0))\n"
      "    )\n"
      "   (if (= a0-0 a1-0) (return '#t))\n"
      "   )\n"
      "  '#f\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTest, ExprFindParentMethod) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L275:\n"
      "    dsll v1, a1, 2\n"
      "    daddu v1, v1, a0\n"
      "    lwu v1, 16(v1)\n"

      "L276:\n"
      "    lw a2, object(s7)\n"
      "    bne a0, a2, L277\n"
      "    or a2, s7, r0\n"

      "    lw v1, nothing(s7)\n"
      "    or v0, v1, r0\n"
      "    beq r0, r0, L279\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"

      "L277:\n"
      "    lwu a0, 4(a0)\n"
      "    dsll a2, a1, 2\n"
      "    daddu a2, a2, a0\n"

      "    lwu v0, 16(a2)\n"
      "    bne v0, r0, L278\n"
      "    or a2, s7, r0\n"

      "    lw v1, nothing(s7)\n"
      "    or v0, v1, r0\n"
      "    beq r0, r0, L279\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"

      "L278:\n"
      "    beq v0, v1, L276\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"

      "L279:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function type int function)";

  std::string expected =
      "(begin\n"
      "  (set! v1-2 (-> a0-0 method-table a1-0))\n"
      "  (until\n"
      "   (!= v0-0 v1-2)\n"
      "   (if (= a0-0 object) (return nothing))\n"
      "   (set! a0-0 (-> a0-0 parent))\n"
      "   (set! v0-0 (-> a0-0 method-table a1-0))\n"
      "   (if (zero? v0-0) (return nothing))\n"
      "   )\n"
      "  (set! v1-5 '#f)\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTest, ExprRef) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L272:\n"
      "    addiu v1, r0, 0\n"
      "    beq r0, r0, L274\n"
      "    sll r0, r0, 0\n"

      "L273:\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n"
      "    lw a0, 2(a0)\n"
      "    daddiu v1, v1, 1\n"

      "L274:\n"
      "    slt a2, v1, a1\n"
      "    bne a2, r0, L273\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    lw v0, -2(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object int object)";

  std::string expected =
      "(begin\n"
      "  (set! v1-0 0)\n"
      "  (while\n"
      "   (< v1-0 a1-0)\n"
      "   (nop!)\n"
      "   (nop!)\n"
      "   (set! a0-0 (cdr a0-0))\n"
      "   (set! v1-0 (+ v1-0 1))\n"
      "   )\n"
      "  (set! v1-1 '#f)\n"
      "  (set! v1-2 '#f)\n"
      "  (car a0-0)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprPairMethod4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L266:\n"
      "    daddiu v1, s7, -10\n"
      "    bne a0, v1, L267\n"
      "    sll r0, r0, 0\n"

      "    addiu v0, r0, 0\n"
      "    beq r0, r0, L271\n"
      "    sll r0, r0, 0\n"

      "L267:\n"
      "    lw v1, 2(a0)\n"
      "    addiu v0, r0, 1\n"
      "    beq r0, r0, L269\n"
      "    sll r0, r0, 0\n"

      "L268:\n"
      "    daddiu v0, v0, 1\n"
      "    lw v1, 2(v1)\n"

      "L269:\n"
      "    daddiu a0, s7, -10\n"
      "    dsubu a0, v1, a0\n"
      "    daddiu a1, s7, 8\n"
      "    movz a1, s7, a0\n"
      "    beql s7, a1, L270\n"
      "    or a0, a1, r0\n"

      "    dsll32 a0, v1, 30\n"
      "    slt a1, a0, r0\n"
      "    daddiu a0, s7, 8\n"
      "    movz a0, s7, a1\n"

      "L270:\n"
      "    bne s7, a0, L268\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"

      "L271:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n";
  std::string type = "(function pair int)";

  std::string expected =
      "(begin\n"
      "  (cond\n"
      "   ((= a0-0 '()) (set! v0-0 0))\n"
      "   (else\n"
      "    (set! v1-1 (cdr a0-0))\n"
      "    (set! v0-0 1)\n"
      "    (while\n"
      "      (and (!= v1-1 '()) "
      "                   (< (shl (the-as int v1-1) 62) 0)\n"
      "      )\n"
      "     (set! v0-0 (+ v0-0 1))\n"
      "     (set! v1-1 (cdr v1-1))\n"
      "     )\n"
      "    (set! v1-2 '#f)\n"
      "    )\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprPairMethod5) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lw v1, pair(s7)\n"
      "    lhu v0, 8(v1)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function pair uint)";

  std::string expected = "(-> pair size)";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTest, ExprLast) {
  std::string func =
      "    sll r0, r0, 0\n"

      "    or v0, a0, r0\n"
      "    beq r0, r0, L264\n"
      "    sll r0, r0, 0\n"

      "L263:\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n"
      "    lw v0, 2(v0)\n"

      "L264:\n"
      "    daddiu v1, s7, -10\n"
      "    lw a0, 2(v0)\n"
      "    bne a0, v1, L263\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object)";

  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (while (!= (cdr v0-0) '())"
      "     (nop!)\n"
      "     (nop!)\n"
      "     (set! v0-0 (cdr v0-0)))\n"
      "  (set! v1-1 '#f)\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprMember) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L257:\n"
      "    or v1, a1, r0\n"
      "    beq r0, r0, L259\n"
      "    sll r0, r0, 0\n"

      "L258:\n"
      "    lw v1, 2(v1)\n"

      "L259:\n"
      "    daddiu a1, s7, -10\n"
      "    dsubu a1, v1, a1\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a1\n"
      "    bnel s7, a2, L260\n"
      "    or a1, a2, r0\n"

      "    lw a1, -2(v1)\n"
      "    dsubu a2, a1, a0\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a2\n"

      "L260:\n"
      "    beq s7, a1, L258\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    daddiu a0, s7, -10\n"
      "    beq v1, a0, L261\n"
      "    or v0, s7, r0\n"

      "    or v0, v1, r0\n"

      "L261:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object object)";

  std::string expected =
      "(begin\n"
      "  (set! v1-0 a1-0)\n"
      "  (while\n"
      "   (not (or (= v1-0 '()) (= (car v1-0) a0-0)))\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (set! a0-1 '#f)\n"
      "  (if (!= v1-0 '()) v1-0)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprNmember) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or s5, a0, r0\n"
      "    or gp, a1, r0\n"
      "    beq r0, r0, L254\n"
      "    sll r0, r0, 0\n"

      "L253:\n"
      "    lw gp, 2(gp)\n"

      "L254:\n"
      "    daddiu v1, s7, -10\n"
      "    dsubu v1, gp, v1\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L255\n"
      "    or v1, a0, r0\n"

      "    lw t9, name=(s7)\n"
      "    lw a0, -2(gp)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"

      "L255:\n"
      "    beq s7, v1, L253\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    daddiu v1, s7, -10\n"
      "    beq gp, v1, L256\n"
      "    or v0, s7, r0\n"

      "    or v0, gp, r0\n"

      "L256:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function basic object object)";

  std::string expected =
      "(begin\n"
      "  (set! s5-0 a0-0)\n"
      "  (set! gp-0 a1-0)\n"
      "  (while\n"
      "   (not (or (= gp-0 '()) (name= (the-as basic (car gp-0)) s5-0)))\n"
      "   (set! gp-0 (cdr gp-0))\n"
      "   )\n"
      "  (set! v1-2 '#f)\n"
      "  (if (!= gp-0 '()) gp-0)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprAssoc) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v1, a1, r0\n"
      "    beq r0, r0, L249\n"
      "    sll r0, r0, 0\n"
      "L248:\n"
      "    lw v1, 2(v1)\n"

      "L249:\n"
      "    daddiu a1, s7, -10\n"
      "    dsubu a1, v1, a1\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a1\n"
      "    bnel s7, a2, L250\n"
      "    or a1, a2, r0\n"

      "    lw a1, -2(v1)\n"
      "    lw a1, -2(a1)\n"
      "    dsubu a2, a1, a0\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a2\n"

      "L250:\n"
      "    beq s7, a1, L248\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    daddiu a0, s7, -10\n"
      "    beq v1, a0, L251\n"
      "    or v0, s7, r0\n"

      "    lw v0, -2(v1)\n"

      "L251:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object object)";

  std::string expected =
      "(begin\n"
      "  (set! v1-0 a1-0)\n"
      "  (while\n"
      "   (not (or (= v1-0 '()) (= (car (car v1-0)) a0-0)))\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (set! a0-1 '#f)\n"
      "  (if (!= v1-0 '()) (car v1-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprAssoce) {
  std::string func =
      "    sll r0, r0, 0\n"

      "    or v1, a1, r0\n"
      "    beq r0, r0, L244\n"
      "    sll r0, r0, 0\n"

      "L243:\n"
      "    lw v1, 2(v1)\n"

      "L244:\n"
      "    daddiu a1, s7, -10\n"
      "    dsubu a1, v1, a1\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a1\n"
      "    bnel s7, a2, L245\n"
      "    or a1, a2, r0\n"

      "    lw a1, -2(v1)\n"
      "    lw a1, -2(a1)\n"
      "    dsubu a1, a1, a0\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a1\n"
      "    bnel s7, a2, L245\n"
      "    or a1, a2, r0\n"

      "    lw a1, -2(v1)\n"
      "    lw a1, -2(a1)\n"
      "    daddiu a2, s7, else\n"
      "    dsubu a2, a1, a2\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a2\n"

      "L245:\n"
      "    beq s7, a1, L243\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    daddiu a0, s7, -10\n"
      "    beq v1, a0, L246\n"
      "    or v0, s7, r0\n"

      "    lw v0, -2(v1)\n"

      "L246:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object object)";

  std::string expected =
      "(begin\n"
      "  (set! v1-0 a1-0)\n"
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (= v1-0 '())\n"
      "     (= (car (car v1-0)) a0-0)\n"
      "     (= (car (car v1-0)) 'else)\n"
      "     )\n"
      "    )\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (set! a0-1 '#f)\n"
      "  (if (!= v1-0 '()) (car v1-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprNassoc) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or s5, a0, r0\n"
      "    or gp, a1, r0\n"
      "    beq r0, r0, L238\n"
      "    sll r0, r0, 0\n"

      "L237:\n"
      "    lw gp, 2(gp)\n"

      "L238:\n"
      "    daddiu v1, s7, -10\n"
      "    dsubu v1, gp, v1\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L240\n"
      "    or v1, a0, r0\n"

      "    lw v1, -2(gp)\n"
      "    lw a1, -2(v1)\n"
      "    dsll32 v1, a1, 30\n"
      "    slt v1, v1, r0\n"
      "    beq v1, r0, L239\n"
      "    sll r0, r0, 0\n"

      "    lw t9, nmember(s7)\n"
      "    or a0, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L240\n"
      "    sll r0, r0, 0\n"

      "L239:\n"
      "    lw t9, name=(s7)\n"
      "    or a0, a1, r0\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"

      "L240:\n"
      "    beq s7, v1, L237\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    daddiu v1, s7, -10\n"
      "    beq gp, v1, L241\n"
      "    or v0, s7, r0\n"

      "    lw v0, -2(gp)\n"

      "L241:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function object object object)";

  std::string expected =
      "(begin\n"
      "  (set! s5-0 a0-0)\n"
      "  (set! gp-0 a1-0)\n"
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (= gp-0 (quote ()))\n"
      "     (begin\n"
      "      (set! a1-1 (car (car gp-0)))\n"
      "      (if "
      "       (pair? a1-1)\n"
      "       (nmember (the-as basic s5-0) a1-1)\n"
      "       (name= (the-as basic a1-1) (the-as basic s5-0))"
      "      )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (set! gp-0 (cdr gp-0))\n"
      "   )\n"
      "  (set! v1-3 (quote #f))\n"
      "  (if (!= gp-0 (quote ())) (car gp-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprNassoce) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L230:\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or s5, a0, r0\n"
      "    or gp, a1, r0\n"
      "    beq r0, r0, L232\n"
      "    sll r0, r0, 0\n"

      "L231:\n"
      "    lw gp, 2(gp)\n"

      "L232:\n"
      "    daddiu v1, s7, -10\n"
      "    dsubu v1, gp, v1\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L234\n"
      "    or v1, a0, r0\n"

      "    lw v1, -2(gp)\n"
      "    lw s4, -2(v1)\n"
      "    dsll32 v1, s4, 30\n"
      "    slt v1, v1, r0\n"
      "    beq v1, r0, L233\n"
      "    sll r0, r0, 0\n"

      "    lw t9, nmember(s7)\n"
      "    or a0, s5, r0\n"
      "    or a1, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L234\n"
      "    sll r0, r0, 0\n"

      "L233:\n"
      "    lw t9, name=(s7)\n"
      "    or a0, s4, r0\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    bnel s7, v0, L234\n"
      "    or v1, v0, r0\n"

      "    daddiu v1, s7, else\n"
      "    dsubu a0, s4, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a0\n"

      "L234:\n"
      "    beq s7, v1, L231\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    daddiu v1, s7, -10\n"
      "    beq gp, v1, L235\n"
      "    or v0, s7, r0\n"

      "    lw v0, -2(gp)\n"

      "L235:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function object object object)";

  // will need fixing if we clean up the set! if thing
  std::string expected =
      "(begin\n"
      "  (set! s5-0 a0-0)\n"
      "  (set! gp-0 a1-0)\n"
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (= gp-0 (quote ()))\n"
      "     (begin\n"
      "      (set! s4-0 (car (car gp-0)))\n"
      "      (if\n"
      "       (pair? s4-0)\n"
      "       (nmember (the-as basic s5-0) s4-0)\n"
      "       (or\n"
      "        (name= (the-as basic s4-0) (the-as basic s5-0))\n"
      "        (= s4-0 (quote else))\n"
      "        )\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (set! gp-0 (cdr gp-0))\n"
      "   )\n"
      "  (set! v1-4 (quote #f))\n"
      "  (if (!= gp-0 (quote ())) (car gp-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprAppend) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L224:\n"
      "    daddiu v1, s7, -10\n"
      "    bne a0, v1, L225\n"
      "    sll r0, r0, 0\n"

      "    or v0, a1, r0\n"
      "    beq r0, r0, L229\n"
      "    sll r0, r0, 0\n"

      "L225:\n"
      "    or v1, a0, r0\n"
      "    beq r0, r0, L227\n"
      "    sll r0, r0, 0\n"

      "L226:\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n"
      "    lw v1, 2(v1)\n"

      "L227:\n"
      "    daddiu a2, s7, -10\n"
      "    lw a3, 2(v1)\n"
      "    bne a3, a2, L226\n"
      "    sll r0, r0, 0\n"

      "    or a2, s7, r0\n"
      "    daddiu a2, s7, -10\n"
      "    beq v1, a2, L228\n"
      "    or a2, s7, r0\n"

      "    sw a1, 2(v1)\n"
      "    or v1, a1, r0\n"

      "L228:\n"
      "    or v0, a0, r0\n"

      "L229:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object object)";

  std::string expected =
      "(cond\n"
      "  ((= a0-0 '()) a1-0)\n"
      "  (else\n"
      "   (set! v1-1 a0-0)\n"
      "   (while (!= (cdr v1-1) '()) (nop!) (nop!) (set! v1-1 (cdr v1-1)))\n"
      "   (set! a2-1 '#f)\n"
      "   (when (!= v1-1 '()) (set! (cdr v1-1) a1-0) (set! v1-2 a1-0))\n"
      "   a0-0\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprDelete) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L217:\n"
      "    lw v1, -2(a1)\n"
      "    bne a0, v1, L218\n"
      "    sll r0, r0, 0\n"

      "    lw v0, 2(a1)\n"
      "    beq r0, r0, L223\n"
      "    sll r0, r0, 0\n"

      "L218:\n"
      "    or v1, a1, r0\n"
      "    lw a2, 2(a1)\n"
      "    beq r0, r0, L220\n"
      "    sll r0, r0, 0\n"

      "L219:\n"
      "    or v1, a2, r0\n"
      "    lw a2, 2(a2)\n"

      "L220:\n"
      "    daddiu a3, s7, -10\n"
      "    dsubu a3, a2, a3\n"
      "    daddiu t0, s7, 8\n"
      "    movn t0, s7, a3\n"
      "    bnel s7, t0, L221\n"
      "    or a3, t0, r0\n"

      "    lw a3, -2(a2)\n"
      "    dsubu t0, a3, a0\n"
      "    daddiu a3, s7, 8\n"
      "    movn a3, s7, t0\n"

      "L221:\n"
      "    beq s7, a3, L219\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    daddiu a0, s7, -10\n"
      "    beq a2, a0, L222\n"
      "    or a0, s7, r0\n"

      "    lw a0, 2(a2)\n"
      "    sw a0, 2(v1)\n"

      "L222:\n"
      "    or v0, a1, r0\n"

      "L223:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object pair)";

  std::string expected =
      "(the-as\n"
      "  pair\n"
      "  (cond\n"
      "   ((= a0-0 (car a1-0)) (cdr a1-0))\n"
      "   (else\n"
      "    (set! v1-1 a1-0)\n"
      "    (set! a2-0 (cdr a1-0))\n"
      "    (while\n"
      "     (not (or (= a2-0 (quote ())) (= (car a2-0) a0-0)))\n"
      "     (set! v1-1 a2-0)\n"
      "     (set! a2-0 (cdr a2-0))\n"
      "     )\n"
      "    (set! a0-1 (quote #f))\n"
      "    (if (!= a2-0 (quote ())) (set! (cdr v1-1) (cdr a2-0)))\n"
      "    a1-0\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprDeleteCar) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L210:\n"
      "    lw v1, -2(a1)\n"
      "    lw v1, -2(v1)\n"
      "    bne a0, v1, L211\n"
      "    sll r0, r0, 0\n"

      "    lw v0, 2(a1)\n"
      "    beq r0, r0, L216\n"
      "    sll r0, r0, 0\n"

      "L211:\n"
      "    or v1, a1, r0\n"
      "    lw a2, 2(a1)\n"
      "    beq r0, r0, L213\n"
      "    sll r0, r0, 0\n"

      "L212:\n"
      "    or v1, a2, r0\n"
      "    lw a2, 2(a2)\n"

      "L213:\n"
      "    daddiu a3, s7, -10\n"
      "    dsubu a3, a2, a3\n"
      "    daddiu t0, s7, 8\n"
      "    movn t0, s7, a3\n"
      "    bnel s7, t0, L214\n"
      "    or a3, t0, r0\n"

      "    lw a3, -2(a2)\n"
      "    lw a3, -2(a3)\n"
      "    dsubu t0, a3, a0\n"
      "    daddiu a3, s7, 8\n"
      "    movn a3, s7, t0\n"

      "L214:\n"
      "    beq s7, a3, L212\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    daddiu a0, s7, -10\n"
      "    beq a2, a0, L215\n"
      "    or a0, s7, r0\n"

      "    lw a0, 2(a2)\n"
      "    sw a0, 2(v1)\n"

      "L215:\n"
      "    or v0, a1, r0\n"

      "L216:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object pair)";

  std::string expected =
      "(the-as\n"
      "  pair\n"
      "  (cond\n"
      "   ((= a0-0 (car (car a1-0))) (cdr a1-0))\n"
      "   (else\n"
      "    (set! v1-2 a1-0)\n"
      "    (set! a2-0 (cdr a1-0))\n"
      "    (while\n"
      "     (not (or (= a2-0 (quote ())) (= (car (car a2-0)) a0-0)))\n"
      "     (set! v1-2 a2-0)\n"
      "     (set! a2-0 (cdr a2-0))\n"
      "     )\n"
      "    (set! a0-1 (quote #f))\n"
      "    (if (!= a2-0 (quote ())) (set! (cdr v1-2) (cdr a2-0)))\n"
      "    a1-0\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprInsertCons) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, delete-car!(s7)\n"
      "    lw a0, -2(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or a3, v0, r0\n"
      "    lw v1, pair(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    daddiu a0, s7, global\n"
      "    lw a1, pair(s7)\n"
      "    or a2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    ld ra, 0(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function object object pair)";

  // NOTE - this appears to _not_ be a nested call.
  std::string expected =
      "(begin\n"
      "  (set! gp-0 a0-0)\n"
      "  (set! a3-0 (delete-car! (car gp-0) a1-0))\n"
      "  (new 'global pair gp-0 a3-0)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprSort) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -112\n"
      "    sd ra, 0(sp)\n"
      "    sq s1, 16(sp)\n"
      "    sq s2, 32(sp)\n"
      "    sq s3, 48(sp)\n"
      "    sq s4, 64(sp)\n"
      "    sq s5, 80(sp)\n"
      "    sq gp, 96(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    addiu s4, r0, -1\n"
      "    beq r0, r0, L208\n"
      "    sll r0, r0, 0\n"

      "L201:\n"
      "    addiu s4, r0, 0\n"
      "    or s3, gp, r0\n"
      "    beq r0, r0, L206\n"
      "    sll r0, r0, 0\n"

      "L202:\n"
      "    lw s2, -2(s3)\n"
      "    lw v1, 2(s3)\n"
      "    lw s1, -2(v1)\n"
      "    or t9, s5, r0\n"
      "    or a0, s2, r0\n"
      "    or a1, s1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beql s7, v1, L203\n"
      "    daddiu a0, s7, 8\n"

      "    slt a1, r0, v1\n"
      "    daddiu a0, s7, 8\n"
      "    movz a0, s7, a1\n"

      "L203:\n"
      "    beql s7, a0, L204\n"
      "    or v1, a0, r0\n"

      "    daddiu a0, s7, #t\n"
      "    dsubu a0, v1, a0\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L204:\n"
      "    beq s7, v1, L205\n"
      "    or v1, s7, r0\n"

      "    daddiu s4, s4, 1\n"
      "    sw s1, -2(s3)\n"
      "    lw v1, 2(s3)\n"
      "    sw s2, -2(v1)\n"
      "    or v1, s2, r0\n"

      "L205:\n"
      "    lw s3, 2(s3)\n"

      "L206:\n"
      "    lw v1, 2(s3)\n"
      "    daddiu a0, s7, -10\n"
      "    dsubu v1, v1, a0\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L207\n"
      "    or v1, a0, r0\n"

      "    lw v1, 2(s3)\n"
      "    dsll32 v1, v1, 30\n"
      "    slt a0, v1, r0\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a0\n"

      "L207:\n"
      "    beq s7, v1, L202\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"

      "L208:\n"
      "    bne s4, r0, L201\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    lq s4, 64(sp)\n"
      "    lq s3, 48(sp)\n"
      "    lq s2, 32(sp)\n"
      "    lq s1, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function object (function object object object) object)";

  // TODO - this should probably be tested.
  std::string expected =
      "(begin\n"
      "  (set! gp-0 a0-0)\n"
      "  (set! s5-0 a1-0)\n"
      "  (set! s4-0 -1)\n"
      "  (while\n"
      "   (nonzero? s4-0)\n"
      "   (set! s4-0 0)\n"
      "   (set! s3-0 gp-0)\n"
      "   (while\n"
      "    (not\n"
      "     (or (= (cdr s3-0) (quote ())) (>= (shl (the-as int (cdr s3-0)) 62) 0))\n"
      "     )\n"
      "    (set! s2-0 (car s3-0))\n"
      "    (set! s1-0 (car (cdr s3-0)))\n"
      "    (set! v1-1 (s5-0 s2-0 s1-0))\n"
      "    (when\n"
      "     (and (or (not v1-1) (> (the-as int v1-1) 0)) (!= v1-1 (quote #t)))\n"
      "     (set! s4-0 (+ s4-0 1))\n"
      "     (set! (car s3-0) s1-0)\n"
      "     (set! (car (cdr s3-0)) s2-0)\n"
      "     (set! v1-5 s2-0)\n"
      "     )\n"
      "    (set! s3-0 (cdr s3-0))\n"
      "    )\n"
      "   (set! v1-10 (quote #f))\n"
      "   (set! v1-11 (quote #f))\n"
      "   )\n"
      "  (set! v1-12 (quote #f))\n"
      "  gp-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTest, ExprInlineArrayMethod0) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a2, r0\n"
      "    lw v1, object(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    or v1, a1, r0\n"
      "    lhu a2, 8(a1)\n"
      "    lhu a1, 12(a1)\n"
      "    multu3 a1, gp, a1\n"
      "    daddu a2, a2, a1\n"
      "    or a1, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq v0, r0, L199\n"
      "    or v1, s7, r0\n"

      "    sw gp, 0(v0)\n"
      "    sw gp, 4(v0)\n"

      "L199:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function symbol type int inline-array-class)";

  std::string expected =
      "(begin\n"
      "  (set! gp-0 a2-0)\n"
      "  (set!\n"
      "   v0-0\n"
      "   (object-new\n"
      "    a0-0\n"
      "    a1-0\n"
      "    (+ (-> a1-0 size) (* (the-as uint gp-0) (-> a1-0 heap-base)))\n"
      "    )\n"
      "   )\n"
      "  (when\n"
      "   (nonzero? v0-0)\n"
      "   (set! (-> v0-0 length) gp-0)\n"
      "   (set! (-> v0-0 allocated-length) gp-0)\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTest, ExprInlineArrayMethod4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lw v0, 0(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function inline-array-class int)";

  std::string expected = "(-> a0-0 length)";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTest, ExprInlineArrayMethod5) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lwu v1, -4(a0)\n"
      "    lhu v1, 8(v1)\n"
      "    lw a1, 4(a0)\n"
      "    lwu a0, -4(a0)\n"
      "    lhu a0, 12(a0)\n"
      "    mult3 a0, a1, a0\n"
      "    daddu v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function inline-array-class int)";

  std::string expected =
      "(the-as int\n"
      "  (+ (-> a0-0 type size)\n"
      "     (the-as uint\n"
      "       (* (-> a0-0 allocated-length)"
      "          (the-as int (-> a0-0 type heap-base)))\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTest, ExprArrayMethod0) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -112\n"
      "    sd ra, 0(sp)\n"
      "    sq s1, 16(sp)\n"
      "    sq s2, 32(sp)\n"
      "    sq s3, 48(sp)\n"
      "    sq s4, 64(sp)\n"
      "    sq s5, 80(sp)\n"
      "    sq gp, 96(sp)\n"

      "    or gp, a2, r0\n"
      "    or s5, a3, r0\n"
      "    lw v1, object(s7)\n"
      "    lwu s4, 16(v1)\n"
      "    or s3, a0, r0\n"
      "    or s2, a1, r0\n"
      "    lhu s1, 8(a1)\n"
      "    lw t9, type-type?(s7)\n"
      "    or a0, gp, r0\n"
      "    lw a1, number(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq s7, v0, L194\n"
      "    sll r0, r0, 0\n"

      "    lhu v1, 8(gp)\n"
      "    beq r0, r0, L195\n"
      "    sll r0, r0, 0\n"

      "L194:\n"
      "    addiu v1, r0, 4\n"

      "L195:\n"
      "    mult3 v1, s5, v1\n"
      "    daddu a2, s1, v1\n"
      "    or t9, s4, r0\n"
      "    or a0, s3, r0\n"
      "    or a1, s2, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sw s5, 4(v0)\n"
      "    sw s5, 0(v0)\n"
      "    sw gp, 8(v0)\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    lq s4, 64(sp)\n"
      "    lq s3, 48(sp)\n"
      "    lq s2, 32(sp)\n"
      "    lq s1, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function symbol type type int array)";

  std::string expected =
      "(begin\n"
      "  (set! gp-0 a2-0)\n"
      "  (set! s5-0 a3-0)\n"
      "  (set!\n"
      "   v0-1\n"
      "   (object-new\n"
      "    a0-0\n"
      "    a1-0\n"
      "    (+\n"
      "     (-> a1-0 size)\n"
      "     (the-as uint (* s5-0 (if (type-type? gp-0 number) (-> gp-0 size) 4)))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (set! (-> v0-1 allocated-length) s5-0)\n"
      "  (set! (-> v0-1 length) s5-0)\n"
      "  (set! (-> v0-1 content-type) gp-0)\n"
      "  v0-1\n"
      "  )";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTest, ExprArrayMethod4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L90:\n"
      "    lw v0, 0(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function array int)";

  std::string expected = "(-> a0-0 length)";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTest, ExprArrayMethod5) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L87:\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"
      "    or s4, a0, r0\n"
      "    lw v1, array(s7)\n"
      "    lhu gp, 8(v1)\n"
      "    lw s5, 4(s4)\n"
      "    lw t9, type-type?(s7)\n"
      "    lwu a0, 8(s4)\n"
      "    lw a1, number(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    beq s7, v0, L88\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    lwu v1, 8(s4)\n"
      "    lhu v1, 8(v1)\n"
      "    beq r0, r0, L89\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L88:\n"
      "    addiu v1, r0, 4\n"
      "L89:\n"
      "    mult3 v1, s5, v1\n"
      "    daddu v0, gp, v1\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function array int)";

  std::string expected =
      "(begin\n"
      "  (set! s4-0 a0-0)\n"
      "  (the-as\n"
      "   int\n"
      "   (+\n"
      "    (-> array size)\n"
      "    (the-as\n"
      "     uint\n"
      "     (*\n"
      "      (-> s4-0 allocated-length)\n"
      "      (if\n"
      "       (type-type? (-> s4-0 content-type) number)\n"
      "       (-> s4-0 content-type size)\n"
      "       4\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTest, ExprMemCopy) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L84:\n"
      "    or v0, a0, r0\n"
      "    addiu v1, r0, 0\n"
      "    beq r0, r0, L86\n"
      "    sll r0, r0, 0\n"

      "L85:\n"
      "    lbu a3, 0(a1)\n"
      "    sb a3, 0(a0)\n"
      "    daddiu a0, a0, 1\n"
      "    daddiu a1, a1, 1\n"
      "    daddiu v1, v1, 1\n"
      "L86:\n"
      "    slt a3, v1, a2\n"
      "    bne a3, r0, L85\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function pointer pointer int pointer)";

  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (set! v1-0 0)\n"
      "  (while\n"
      "   (< v1-0 a2-0)\n"
      "   (set! (-> (the-as (pointer int8) a0-0)) (-> (the-as (pointer uint8) a1-0)))\n"
      "   (set! a0-0 (+ a0-0 (the-as uint 1)))\n"
      "   (set! a1-0 (+ a1-0 (the-as uint 1)))\n"
      "   (set! v1-0 (+ v1-0 1))\n"
      "   )\n"
      "  (set! v1-1 (quote #f))\n"
      "  (set! v1-2 (quote #f))\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMemSet32) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L75:\n"
      "    or v0, a0, r0\n"
      "    addiu v1, r0, 0\n"
      "    beq r0, r0, L77\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L76:\n"
      "    sw a2, 0(a0)\n"
      "    daddiu a0, a0, 4\n"
      "    sll r0, r0, 0\n"
      "    daddiu v1, v1, 1\n"
      "L77:\n"
      "    slt a3, v1, a1\n"
      "    bne a3, r0, L76\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n";
  std::string type = "(function pointer int int pointer)";

  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (set! v1-0 0)\n"
      "  (while\n"
      "   (< v1-0 a1-0)\n"
      "   (set! (-> (the-as (pointer int32) a0-0)) a2-0)\n"
      "   (set! a0-0 (+ a0-0 (the-as uint 4)))\n"
      "   (nop!)\n"
      "   (set! v1-0 (+ v1-0 1))\n"
      "   )\n"
      "  (set! v1-1 (quote #f))\n"
      "  (set! v1-2 (quote #f))\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprMemOr) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L72:\n"
      "    or v0, a0, r0\n"
      "    addiu v1, r0, 0\n"
      "    beq r0, r0, L74\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L73:\n"
      "    lbu a3, 0(a0)\n"
      "    lbu t0, 0(a1)\n"
      "    or a3, a3, t0\n"
      "    sb a3, 0(a0)\n"
      "    daddiu a0, a0, 1\n"
      "    daddiu a1, a1, 1\n"
      "    daddiu v1, v1, 1\n"
      "L74:\n"
      "    slt a3, v1, a2\n"
      "    bne a3, r0, L73\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function pointer pointer int pointer)";

  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (set! v1-0 0)\n"
      "  (while\n"
      "   (< v1-0 a2-0)\n"
      "   (set!\n"
      "    (-> (the-as (pointer int8) a0-0))\n"
      "    (logior\n"
      "     (-> (the-as (pointer uint8) a0-0))\n"
      "     (-> (the-as (pointer uint8) a1-0))\n"
      "     )\n"
      "    )\n"
      "   (set! a0-0 (+ a0-0 (the-as uint 1)))\n"
      "   (set! a1-0 (+ a1-0 (the-as uint 1)))\n"
      "   (set! v1-0 (+ v1-0 1))\n"
      "   )\n"
      "  (set! v1-1 (quote #f))\n"
      "  (set! v1-2 (quote #f))\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprFact) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L65:\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sq gp, 16(sp)\n"
      "    or gp, a0, r0\n"
      "    addiu v1, r0, 1\n"
      "    bne gp, v1, L66\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    addiu v0, r0, 1\n"
      "    beq r0, r0, L67\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L66:\n"
      "    lw t9, fact(s7)\n"
      "    daddiu a0, gp, -1\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    mult3 v0, gp, v0\n"
      "L67:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function int int)";

  std::string expected = "(begin (set! gp-0 a0-0) (if (= gp-0 1) 1 (* gp-0 (fact (+ gp-0 -1)))))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprPrint) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L63:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    dsll32 v1, a0, 29\n"
      "    beql v1, r0, L64\n"
      "    lw v1, binteger(s7)\n"
      "\n"
      "    bgtzl v1, L64\n"
      "    lw v1, pair(s7)\n"
      "\n"
      "    lwu v1, -4(a0)\n"
      "L64:\n"
      "    lwu t9, 24(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function object object)";

  std::string expected = "((method-of-type (rtype-of a0-0) print) a0-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprPrintl) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L61:\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"
      "    or gp, a0, r0\n"
      "    or a0, gp, r0\n"
      "    dsll32 v1, a0, 29\n"
      "    beql v1, r0, L62\n"
      "    lw v1, binteger(s7)\n"
      "\n"
      "    bgtzl v1, L62\n"
      "    lw v1, pair(s7)\n"
      "\n"
      "    lwu v1, -4(a0)\n"
      "L62:\n"
      "    lwu t9, 24(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L324\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function object object)";

  // todo - I think this is a sign that we're unscrambling method calls in the wrong order.
  // but I want to wait for a less confusing example before making a change.
  std::string expected =
      "(begin\n"
      "  (set! gp-0 a0-0)\n"
      "  (set! a0-1 gp-0)\n"
      "  (set! v1-2 ((method-of-type (rtype-of a0-1) print) a0-1))\n"
      "  (format (quote #t) L324)\n"
      "  gp-0\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L324", "~%"}});
}

TEST_F(FormRegressionTest, ExprInspect) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L59:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    dsll32 v1, a0, 29\n"
      "    beql v1, r0, L60\n"
      "    lw v1, binteger(s7)\n"
      "\n"
      "    bgtzl v1, L60\n"
      "    lw v1, pair(s7)\n"
      "\n"
      "    lwu v1, -4(a0)\n"
      "L60:\n"
      "    lwu t9, 28(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function object object)";

  std::string expected = "((method-of-type (rtype-of a0-0) inspect) a0-0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprPrintTreeBitmask) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L54:\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"
      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    addiu s4, r0, 0\n"
      "    beq r0, r0, L58\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L55:\n"
      "    andi v1, gp, 1\n"
      "    bne v1, r0, L56\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L323\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    beq r0, r0, L57\n"
      "    sll r0, r0, 0\n"
      "\n"
      "L56:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L322\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "L57:\n"
      "    dsrl gp, gp, 1\n"
      "    daddiu s4, s4, 1\n"
      "L58:\n"
      "    slt v1, s4, s5\n"
      "    bne v1, r0, L55\n"
      "    sll r0, r0, 0\n"
      "\n"
      "    or v1, s7, r0\n"
      "    or v0, s7, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function int int symbol)";

  std::string expected =
      "(begin\n"
      "  (set! gp-0 a0-0)\n"
      "  (set! s5-0 a1-0)\n"
      "  (set! s4-0 0)\n"
      "  (while\n"
      "   (< s4-0 s5-0)\n"
      "   (if\n"
      "    (zero? (logand gp-0 1))\n"
      "    (format (quote #t) L323)\n"
      "    (format (quote #t) L322)\n"
      "    )\n"
      "   (set! gp-0 (shr (the-as uint gp-0) 1))\n"
      "   (set! s4-0 (+ s4-0 1))\n"
      "   )\n"
      "  (set! v1-3 (quote #f))\n"
      "  (quote #f)\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L323", "    "}, {"L322", "|   "}});
}

TEST_F(FormRegressionTest, ExprPrintName) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L136:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"

      "    bne a0, a1, L137\n"
      "    or v1, s7, r0\n"

      "    daddiu v0, s7, #t\n"
      "    beq r0, r0, L143\n"
      "    sll r0, r0, 0\n"

      "L137:\n"
      "    lwu v1, -4(a0)\n"
      "    lw a2, string(s7)\n"
      "    dsubu v1, v1, a2\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, v1\n"
      "    beql s7, a2, L138\n"
      "    or v1, a2, r0\n"

      "    lwu v1, -4(a1)\n"
      "    lw a2, string(s7)\n"
      "    dsubu a2, v1, a2\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a2\n"

      "L138:\n"
      "    beq s7, v1, L139\n"
      "    or v1, s7, r0\n"

      "    lw t9, string=(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq r0, r0, L143\n"
      "    sll r0, r0, 0\n"

      "L139:\n"
      "    lwu v1, -4(a0)\n"
      "    lw a2, string(s7)\n"
      "    dsubu v1, v1, a2\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, v1\n"
      "    beql s7, a2, L140\n"
      "    or v1, a2, r0\n"

      "    lwu v1, -4(a1)\n"
      "    lw a2, symbol(s7)\n"
      "    dsubu a2, v1, a2\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a2\n"

      "L140:\n"
      "    beq s7, v1, L141\n"
      "    or v1, s7, r0\n"

      "    lw t9, string=(s7)\n"
      "    ori v1, r0, 65336\n"
      "    daddu v1, v1, a1\n"
      "    lwu a1, 0(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    beq r0, r0, L143\n"
      "    sll r0, r0, 0\n"

      "L141:\n"
      "    lwu v1, -4(a1)\n"
      "    lw a2, string(s7)\n"
      "    dsubu v1, v1, a2\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, v1\n"
      "    beql s7, a2, L142\n"
      "    or v1, a2, r0\n"

      "    lwu v1, -4(a0)\n"
      "    lw a2, symbol(s7)\n"
      "    dsubu a2, v1, a2\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a2\n"

      "L142:\n"
      "    beq s7, v1, L143\n"
      "    or v0, s7, r0\n"

      "    lw t9, string=(s7)\n"
      "    or v1, a1, r0\n"
      "    ori a1, r0, 65336\n"
      "    daddu a0, a1, a0\n"
      "    lwu a1, 0(a0)\n"
      "    or a0, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "L143:\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function basic basic symbol)";

  std::string expected =
      "(cond\n"
      "  ((= a0-0 a1-0) (quote #t))\n"
      "  ((and (= (-> a0-0 type) string) (= (-> a1-0 type) string))\n"
      "   (string= (the-as string a0-0) (the-as string a1-0))\n"
      "   )\n"
      "  ((and (= (-> a0-0 type) string) (= (-> a1-0 type) symbol))\n"
      "   (string= (the-as string a0-0) (-> (+ 65336 (the-as int a1-0)) 0))\n"
      "   )\n"
      "  ((and (= (-> a1-0 type) string) (= (-> a0-0 type) symbol))\n"
      "   (string= (the-as string a1-0) (-> (+ 65336 (the-as int a0-0)) 0))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 parse_hint_json("[\t\t[24, [\"a1\", \"symbol\"]],\n"
                                 "\t\t[39, [\"a0\", \"symbol\"]]]"));
}