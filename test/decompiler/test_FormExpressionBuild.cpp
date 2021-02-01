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
      "    (return '#t (set! v1-0 0))\n"
      "    )\n"
      "   )\n"
      "  '#f\n"
      "  )";
  test_with_expr(func, type, expected);
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
      "   (if (= a0-0 a1-0) (return '#t (set! v1-0 0)))\n"
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
      "  (set! v1-2 (-> a0-0 methods a1-0))\n"
      "  (until\n"
      "   (!= v0-0 v1-2)\n"
      "   (if (= a0-0 object) (return nothing (set! v1-2 0)))\n"
      "   (set! a0-0 (-> a0-0 parent))\n"
      "   (set! v0-0 (-> a0-0 methods a1-0))\n"
      "   (if (zero? v0-0) (return nothing (set! v1-2 0)))\n"
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
      "   (<.si v1-0 a1-0)\n"
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
      "                   (<0.si (sll (the-as uint v1-1) 62))\n"
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
      "   (not (or (= gp-0 '()) (name= (car gp-0) s5-0)))\n"
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
      "      (if (pair? a1-1) (nmember s5-0 a1-1) (name= a1-1 s5-0))\n"
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
      "       (nmember s5-0 s4-0)\n"
      "       (or (name= s4-0 s5-0) (= s4-0 (quote else)))\n"
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
      "     (or (= (cdr s3-0) (quote ())) (>=0.si (sll (the-as uint (cdr s3-0)) 62)))\n"
      "     )\n"
      "    (set! s2-0 (car s3-0))\n"
      "    (set! s1-0 (car (cdr s3-0)))\n"
      "    (set! v1-1 (s5-0 s2-0 s1-0))\n"
      "    (when\n"
      "     (and (or (not v1-1) (>0.si v1-1)) (!= v1-2 (quote #t)))\n"
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

TEST_F(FormRegressionTest, ExprArrayMethod3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L130:\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L342\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, type-type?(s7)\n"
      "    lwu a0, 8(gp)\n"
      "    lw a1, integer(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq s7, v0, L182\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 8(gp)\n"
      "    lwu v1, 0(v1)\n"
      "    daddiu a0, s7, int32\n"
      "    bne v1, a0, L135\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L134\n"
      "    sll r0, r0, 0\n"

      "L131:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L132\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L133\n"
      "    sll r0, r0, 0\n"

      "L132:\n"
      "    daddiu a1, fp, L340\n"
      "L133:\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L134:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L131\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"
      "L135:\n"
      "    daddiu a0, s7, uint32\n"
      "    bne v1, a0, L140\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L139\n"
      "    sll r0, r0, 0\n"

      "L136:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L137\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L138\n"
      "    sll r0, r0, 0\n"

      "L137:\n"
      "    daddiu a1, fp, L340\n"
      "L138:\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lwu a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L139:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L136\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"

      "L140:\n"
      "    daddiu a0, s7, int64\n"
      "    bne v1, a0, L145\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L144\n"
      "    sll r0, r0, 0\n"

      "L141:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L142\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L143\n"
      "    sll r0, r0, 0\n"

      "L142:\n"
      "    daddiu a1, fp, L340\n"
      "L143:\n"
      "    dsll v1, s5, 3\n"
      "    daddu v1, v1, gp\n"
      "    ld a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    daddiu s5, s5, 1\n"
      "L144:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L141\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"
      "L145:\n"
      "    daddiu a0, s7, uint64\n"
      "    bne v1, a0, L150\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L149\n"
      "    sll r0, r0, 0\n"

      "L146:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L147\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L339\n"
      "    beq r0, r0, L148\n"
      "    sll r0, r0, 0\n"

      "L147:\n"
      "    daddiu a1, fp, L338\n"
      "L148:\n"
      "    dsll v1, s5, 3\n"
      "    daddu v1, v1, gp\n"
      "    ld a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L149:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L146\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"

      "L150:\n"
      "    daddiu a0, s7, int8\n"
      "    bne v1, a0, L155\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L154\n"
      "    sll r0, r0, 0\n"

      "L151:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L152\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L153\n"
      "    sll r0, r0, 0\n"

      "L152:\n"
      "    daddiu a1, fp, L340\n"
      "L153:\n"
      "    daddu v1, s5, gp\n"
      "    lb a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L154:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L151\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"

      "L155:\n"
      "    daddiu a0, s7, uint8\n"
      "    bne v1, a0, L160\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L159\n"
      "    sll r0, r0, 0\n"

      "L156:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L157\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L158\n"
      "    sll r0, r0, 0\n"

      "L157:\n"
      "    daddiu a1, fp, L340\n"
      "L158:\n"
      "    daddu v1, s5, gp\n"
      "    lbu a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L159:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L156\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"
      "L160:\n"
      "    daddiu a0, s7, int16\n"
      "    bne v1, a0, L165\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L164\n"
      "    sll r0, r0, 0\n"

      "L161:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L162\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L163\n"
      "    sll r0, r0, 0\n"

      "L162:\n"
      "    daddiu a1, fp, L340\n"

      "L163:\n"
      "    dsll v1, s5, 1\n"
      "    daddu v1, v1, gp\n"
      "    lh a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    daddiu s5, s5, 1\n"

      "L164:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L161\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"

      "L165:\n"
      "    daddiu a0, s7, uint16\n"
      "    bne v1, a0, L170\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L169\n"
      "    sll r0, r0, 0\n"

      "L166:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L167\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L168\n"
      "    sll r0, r0, 0\n"

      "L167:\n"
      "    daddiu a1, fp, L340\n"
      "L168:\n"
      "    dsll v1, s5, 1\n"
      "    daddu v1, v1, gp\n"
      "    lhu a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddiu s5, s5, 1\n"
      "L169:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L166\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"
      "L170:\n"
      "    daddiu a0, s7, uint128\n"
      "    dsubu a0, v1, a0\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a0\n"
      "    bnel s7, a1, L171\n"
      "    or v1, a1, r0\n"

      "    daddiu a0, s7, int128\n"
      "    dsubu a0, v1, a0\n"
      "    daddiu v1, s7, 8\n"
      "    movn v1, s7, a0\n"

      "L171:\n"
      "    beq s7, v1, L176\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L175\n"
      "    sll r0, r0, 0\n"

      "L172:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L173\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L339\n"
      "    beq r0, r0, L174\n"
      "    sll r0, r0, 0\n"

      "L173:\n"
      "    daddiu a1, fp, L338\n"

      "L174:\n"
      "    dsll v1, s5, 4\n"
      "    daddu v1, v1, gp\n"
      "    lq a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    daddiu s5, s5, 1\n"
      "L175:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L172\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L181\n"
      "    sll r0, r0, 0\n"

      "L176:\n"
      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L180\n"
      "    sll r0, r0, 0\n"

      "L177:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    bne s5, r0, L178\n"
      "    sll r0, r0, 0\n"

      "    daddiu a1, fp, L341\n"
      "    beq r0, r0, L179\n"
      "    sll r0, r0, 0\n"

      "L178:\n"
      "    daddiu a1, fp, L340\n"
      "L179:\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    daddiu s5, s5, 1\n"

      "L180:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L177\n"
      "    sll r0, r0, 0\n"
      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "L181:\n"
      "    beq r0, r0, L192\n"
      "    sll r0, r0, 0\n"
      "L182:\n"
      "    lw v1, float(s7)\n"
      "    lwu a0, 8(gp)\n"
      "    bne a0, v1, L187\n"
      "    sll r0, r0, 0\n"

      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L186\n"
      "    sll r0, r0, 0\n"
      "L183:\n"
      "    bne s5, r0, L184\n"
      "    sll r0, r0, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L343\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L185\n"
      "    sll r0, r0, 0\n"
      "L184:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L337\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "L185:\n"
      "    daddiu s5, s5, 1\n"
      "L186:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L183\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    beq r0, r0, L192\n"
      "    sll r0, r0, 0\n"
      "L187:\n"
      "    addiu s5, r0, 0\n"
      "    beq r0, r0, L191\n"
      "    sll r0, r0, 0\n"
      "L188:\n"
      "    bne s5, r0, L189\n"
      "    sll r0, r0, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L336\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L190\n"
      "    sll r0, r0, 0\n"
      "L189:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L335\n"
      "    dsll v1, s5, 2\n"
      "    daddu v1, v1, gp\n"
      "    lw a2, 12(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    or v1, v0, r0\n"
      "L190:\n"
      "    daddiu s5, s5, 1\n"
      "L191:\n"
      "    lw v1, 0(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L188\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "L192:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L334\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function array array)";

  std::string expected =
      "(begin\n"
      "  (set! gp-0 a0-0)\n"
      "  (format (quote #t) L342)\n"
      "  (cond\n"
      "   ((type-type? (-> gp-0 content-type) integer)\n"
      "    (set! v1-1 (-> gp-0 content-type symbol))\n"
      "    (set!\n"
      "     v1-39\n"
      "     (cond\n"
      "      ((= v1-1 (quote int32))\n"
      "       (set! s5-0 0)\n"
      "       (while\n"
      "        (<.si s5-0 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-0) L341 L340) (-> gp-0 s5-0))\n"
      "        (set! s5-0 (+ s5-0 1))\n"
      "        )\n"
      "       (set! v1-5 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote uint32))\n"
      "       (set! s5-1 0)\n"
      "       (while\n"
      "        (<.si s5-1 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-1) L341 L340) (-> gp-0 s5-1))\n"
      "        (set! s5-1 (+ s5-1 1))\n"
      "        )\n"
      "       (set! v1-10 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote int64))\n"
      "       (set! s5-2 0)\n"
      "       (while\n"
      "        (<.si s5-2 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-2) L341 L340) (-> gp-0 s5-2))\n"
      "        (set! s5-2 (+ s5-2 1))\n"
      "        )\n"
      "       (set! v1-15 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote uint64))\n"
      "       (set! s5-3 0)\n"
      "       (while\n"
      "        (<.si s5-3 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-3) L339 L338) (-> gp-0 s5-3))\n"
      "        (set! s5-3 (+ s5-3 1))\n"
      "        )\n"
      "       (set! v1-20 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote int8))\n"
      "       (set! s5-4 0)\n"
      "       (while\n"
      "        (<.si s5-4 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-4) L341 L340) (-> gp-0 s5-4))\n"
      "        (set! s5-4 (+ s5-4 1))\n"
      "        )\n"
      "       (set! v1-24 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote uint8))\n"
      "       (set! s5-5 0)\n"
      "       (while\n"
      "        (<.si s5-5 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-5) L341 L340) (-> gp-0 s5-5))\n"
      "        (set! s5-5 (+ s5-5 1))\n"
      "        )\n"
      "       (set! v1-28 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote int16))\n"
      "       (set! s5-6 0)\n"
      "       (while\n"
      "        (<.si s5-6 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-6) L341 L340) (-> gp-0 s5-6))\n"
      "        (set! s5-6 (+ s5-6 1))\n"
      "        )\n"
      "       (set! v1-33 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      ((= v1-1 (quote uint16))\n"
      "       (set! s5-7 0)\n"
      "       (while\n"
      "        (<.si s5-7 (-> gp-0 length))\n"
      "        (format (quote #t) (if (zero? s5-7) L341 L340) (-> gp-0 s5-7))\n"
      "        (set! s5-7 (+ s5-7 1))\n"
      "        )\n"
      "       (set! v1-38 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      (else\n"
      "       (set! v1-40 (or (= v1-1 (quote uint128)) (= v1-40 (quote int128))))\n"
      "       (cond\n"
      "        (v1-40\n"
      "         (set! s5-8 0)\n"
      "         (while\n"
      "          (<.si s5-8 (-> gp-0 length))\n"
      "          (set! t9-10 format)\n"
      "          (set! a0-21 (quote #t))\n"
      "          (set! a1-11 (if (zero? s5-8) L339 L338))\n"
      "          (set! v1-42 (+ (sll (the-as uint s5-8) 4) (the-as int gp-0)))\n"
      "          (.lq a2-8 12 v1-42)\n"
      "          (t9-10 a0-21 a1-11 a2-8)\n"
      "          (set! s5-8 (+ s5-8 1))\n"
      "          )\n"
      "         (set! v1-44 (quote #f))\n"
      "         (quote #f)\n"
      "         )\n"
      "        (else\n"
      "         (set! s5-9 0)\n"
      "         (while\n"
      "          (<.si s5-9 (-> gp-0 length))\n"
      "          (format (quote #t) (if (zero? s5-9) L341 L340) (-> gp-0 s5-9))\n"
      "          (set! s5-9 (+ s5-9 1))\n"
      "          )\n"
      "         (set! v1-49 (quote #f))\n"
      "         (quote #f)\n"
      "         )\n"
      "        )\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (else\n"
      "    (set!\n"
      "     v1-69\n"
      "     (cond\n"
      "      ((= (-> gp-0 content-type) float)\n"
      "       (set! s5-10 0)\n"
      "       (while\n"
      "        (<.si s5-10 (-> gp-0 length))\n"
      "        (set!\n"
      "         v1-57\n"
      "         (if\n"
      "          (zero? s5-10)\n"
      "          (format (quote #t) L343 (-> gp-0 s5-10))\n"
      "          (format (quote #t) L337 (-> gp-0 s5-10))\n"
      "          )\n"
      "         )\n"
      "        (set! s5-10 (+ s5-10 1))\n"
      "        )\n"
      "       (set! v1-59 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      (else\n"
      "       (set! s5-11 0)\n"
      "       (while\n"
      "        (<.si s5-11 (-> gp-0 length))\n"
      "        (set!\n"
      "         v1-66\n"
      "         (if\n"
      "          (zero? s5-11)\n"
      "          (format (quote #t) L336 (-> gp-0 s5-11))\n"
      "          (format (quote #t) L335 (-> gp-0 s5-11))\n"
      "          )\n"
      "         )\n"
      "        (set! s5-11 (+ s5-11 1))\n"
      "        )\n"
      "       (set! v1-68 (quote #f))\n"
      "       (quote #f)\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (format (quote #t) L334)\n"
      "  gp-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "array",
                 {{"L343", "~f"},
                  {"L342", "#("},
                  {"L341", "~D"},
                  {"L340", " ~D"},
                  {"L339", "#x~X"},
                  {"L338", " #x~X"},
                  {"L337", " ~f"},
                  {"L336", "~A"},
                  {"L335", " ~A"},
                  {"L334", ")"}},
                 parse_hint_json("["
                                 "\t\t[23, [\"gp\", \"(array int32)\"]],\n"
                                 "\t\t[43, [\"gp\", \"(array uint32)\"]],\n"
                                 "\t\t[63, [\"gp\", \"(array int64)\"]],\n"
                                 "\t\t[83, [\"gp\", \"(array uint64)\"]],\n"
                                 "\t\t[102, [\"gp\", \"(array int8)\"]],\n"
                                 "\t\t[121, [\"gp\", \"(array uint8)\"]],\n"
                                 "\t\t[141, [\"gp\", \"(array int16)\"]],\n"
                                 "\t\t[161, [\"gp\", \"(array uint16)\"]],\n"
                                 "\t\t[185, [\"gp\", \"(array uint128)\"]],\n"
                                 "\t\t[203, [\"gp\", \"(array int32)\"]],\n"
                                 "\t\t[222, [\"gp\", \"(array float)\"]],\n"
                                 "\t\t[231, [\"gp\", \"(array float)\"]],\n"
                                 "\t\t[248, [\"gp\", \"(array basic)\"]],\n"
                                 "\t\t[257, [\"gp\", \"(array basic)\"]]]"));
}