#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

TEST_F(FormRegressionTestJak1, ExprIdentity) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object)";
  std::string expected = "arg0";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprFloatingPoint) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L345:\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, a0\n"
      "    div.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function float float)";
  std::string expected = "(/ 0.0 arg0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionSigned) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(+ arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionUnSigned) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(+ arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionMixed1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(+ arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionMixed2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int uint)";
  std::string expected = "(+ arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionSignedWrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int uint)";
  std::string expected = "(the-as uint (+ arg0 arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionUnSignedWrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint int)";
  std::string expected = "(the-as int (+ arg0 arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionMixed1WrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint uint)";
  std::string expected = "(the-as uint (+ arg0 arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAdditionMixed2WrongReturn) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(the-as int (+ arg0 arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprSubtraction) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    dsubu v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(- arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMultiplication) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(* arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMultiplicationWrong1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(* arg0 (the-as int arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMultiplicationWrong2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(* (the-as int arg0) arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMultiplicationWrong3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(the-as uint (* (the-as int arg0) (the-as int arg1)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprDivision1) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(/ arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprDivision2) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint int int)";
  std::string expected = "(/ (the-as int arg0) arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprDivision3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int uint int)";
  std::string expected = "(/ arg0 (the-as int arg1))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprDivision4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function uint uint uint)";
  std::string expected = "(the-as uint (/ (the-as int arg0) (the-as int arg1)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAsh) {
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
  std::string expected = "(ash arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMod) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    div a0, a1\n"
      "    mfhi v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(mod arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprAbs) {
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
  std::string expected = "(abs arg0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMin) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movz v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(min arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMax) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movn v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(max arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLogior) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logior arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLogxor) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    xor v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logxor arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLognor) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    nor v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(lognor arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLogand) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    and v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(logand arg0 arg1)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLognot) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    nor v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int)";
  std::string expected = "(lognot arg0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprFalse) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function symbol)";
  std::string expected = "#f";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprTrue) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu v0, s7, #t\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function symbol)";
  std::string expected = "#t";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprPrintBfloat) {
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

  std::string expected = "(begin (format #t \"~f\" (-> arg0 data)) arg0)";
  test_with_expr(func, type, expected, false, "", {{"L343", "~f"}});
}

TEST_F(FormRegressionTestJak1, ExprSizeOfType) {
  std::string func =
      "L346:\n"  // fake label.
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      //"    ld v1, L346(fp)\n"
      "    addiu v1, r0, 3\n"
      "    lhu a0, 14(a0)\n"
      "    dsll a0, a0, 2\n"
      "    daddiu a0, a0, 43\n"
      "    and v0, v1, a0 \n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function type uint)";

  std::string expected = "(logand 3 (+ (* (-> arg0 allocated-length) 4) 43))";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprBasicTypeP) {
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
      "  (let ((v1-0 (-> arg0 type))\n"
      "        (a0-1 object)\n"
      "        )\n"
      "   (until (= v1-0 a0-1)\n"
      "    (if (= v1-0 arg1)\n"
      "     (return #t)\n"
      "     )\n"
      "    (set! v1-0 (-> v1-0 parent))\n"
      "    )\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, FinalBasicTypeP) {
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
      "(defun test-function ((arg0 basic) (arg1 type))\n"
      "  (let ((v1-0 (-> arg0 type))\n"
      "        (a0-1 object)\n"
      "        )\n"
      "   (until (= v1-0 a0-1)\n"
      "    (if (= v1-0 arg1)\n"
      "     (return #t)\n"
      "     )\n"
      "    (set! v1-0 (-> v1-0 parent))\n"
      "    )\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_final_function(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprTypeTypep) {
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
      "  (let ((v1-0 object))\n"
      "   (until (or (= arg0 v1-0) (zero? arg0))\n"
      "    (if (= arg0 arg1)\n"
      "     (return #t)\n"
      "     )\n"
      "    (set! arg0 (-> arg0 parent))\n"
      "    )\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprFindParentMethod) {
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
      "  (let\n"
      "   ((v1-2 (-> arg0 method-table arg1)))\n"
      "   (until\n"
      "    (!= v0-0 v1-2)\n"
      "    (if (= arg0 object) (return nothing))\n"
      "    (set! arg0 (-> arg0 parent))\n"
      "    (set! v0-0 (-> arg0 method-table arg1))\n"
      "    (if (zero? v0-0) (return nothing))\n"
      "    )\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprRef) {
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
      "(begin (dotimes (v1-0 arg1) (nop!) (nop!) (set! arg0 (cdr arg0))) (car arg0))";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprPairMethod4) {
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

  // multiple return paths merged variable issue.
  std::string expected =
      "(begin\n"
      "  (cond\n"
      "   ((null? arg0) (set! v0-0 0))\n"
      "   (else\n"
      "    (let\n"
      "     ((v1-1 (cdr arg0)))\n"
      "     (set! v0-0 1)\n"
      "     (while\n"
      "      (and (not (null? v1-1)) (pair? v1-1))\n"
      "      (+! v0-0 1)\n"
      "      (set! v1-1 (cdr v1-1))\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprPairMethod5) {
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

TEST_F(FormRegressionTestJak1, ExprLast) {
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
      "(let\n"
      "  ((v0-0 arg0))\n"
      "  (while (not (null? (cdr v0-0))) (nop!) (nop!) (set! v0-0 (cdr v0-0)))\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprMember) {
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
      "(let\n"
      "  ((v1-0 arg1))\n"
      "  (while\n"
      "   (not (or (null? v1-0) (= (car v1-0) arg0)))\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (if (not (null? v1-0)) v1-0)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprNmember) {
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
      "  (while\n"
      "   (not (or (null? arg1) (name= (the-as basic (car arg1)) arg0)))\n"
      "   (set! arg1 (cdr arg1))\n"
      "   )\n"
      "  (if (not (null? arg1)) arg1)\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprAssoc) {
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
      "(let\n"
      "  ((v1-0 arg1))\n"
      "  (while\n"
      "   (not (or (null? v1-0) (= (car (car v1-0)) arg0)))\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (if (not (null? v1-0)) (car v1-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprAssoce) {
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
      "(let\n"
      "  ((v1-0 arg1))\n"
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (null? v1-0)\n"
      "     (= (car (car v1-0)) arg0)\n"
      "     (= (car (car v1-0)) (quote else))\n"
      "     )\n"
      "    )\n"
      "   (set! v1-0 (cdr v1-0))\n"
      "   )\n"
      "  (if (not (null? v1-0)) (car v1-0))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprNassoc) {
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
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (null? arg1)\n"
      "     (let\n"
      "      ((a1-1 (car (car arg1))))\n"
      "      (if\n"
      "       (pair? a1-1)\n"
      "       (nmember (the-as basic arg0) a1-1)\n"
      "       (name= (the-as basic a1-1) (the-as basic arg0))\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (set! arg1 (cdr arg1))\n"
      "   )\n"
      "  (if (not (null? arg1)) (car arg1))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprNassoce) {
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
      "  (while\n"
      "   (not\n"
      "    (or\n"
      "     (null? arg1)\n"
      "     (let\n"
      "      ((s4-0 (car (car arg1))))\n"
      "      (if\n"
      "       (pair? s4-0)\n"
      "       (nmember (the-as basic arg0) s4-0)\n"
      "       (or\n"
      "        (name= (the-as basic s4-0) (the-as basic arg0))\n"
      "        (= s4-0 (quote else))\n"
      "        )\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (set! arg1 (cdr arg1))\n"
      "   )\n"
      "  (if (not (null? arg1)) (car arg1))\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprAppend) {
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
      "  ((null? arg0) arg1)\n"
      "  (else\n"
      "   (let\n"
      "    ((v1-1 arg0))\n"
      "    (while (not (null? (cdr v1-1))) (nop!) (nop!) (set! v1-1 (cdr v1-1)))\n"
      "    (if (not (null? v1-1)) (set! (cdr v1-1) arg1))\n"
      "    )\n"
      "   arg0\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprDelete) {
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
      "   ((= arg0 (car arg1)) (cdr arg1))\n"
      "   (else\n"
      "    (let\n"
      "     ((v1-1 arg1) (a2-0 (cdr arg1)))\n"
      "     (while\n"
      "      (not (or (null? a2-0) (= (car a2-0) arg0)))\n"
      "      (set! v1-1 a2-0)\n"
      "      (set! a2-0 (cdr a2-0))\n"
      "      )\n"
      "     (if (not (null? a2-0)) (set! (cdr v1-1) (cdr a2-0)))\n"
      "     )\n"
      "    arg1\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprDeleteCar) {
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
      "   ((= arg0 (car (car arg1))) (cdr arg1))\n"
      "   (else\n"
      "    (let\n"
      "     ((v1-2 arg1) (a2-0 (cdr arg1)))\n"
      "     (while\n"
      "      (not (or (null? a2-0) (= (car (car a2-0)) arg0)))\n"
      "      (set! v1-2 a2-0)\n"
      "      (set! a2-0 (cdr a2-0))\n"
      "      )\n"
      "     (if (not (null? a2-0)) (set! (cdr v1-2) (cdr a2-0)))\n"
      "     )\n"
      "    arg1\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprInsertCons) {
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
  std::string expected = "(let ((a3-0 (delete-car! (car arg0) arg1))) (cons arg0 a3-0))";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprSort) {
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
      "  (let\n"
      "   ((s4-0 -1))\n"
      "   (while\n"
      "    (nonzero? s4-0)\n"
      "    (set! s4-0 0)\n"
      "    (let\n"
      "     ((s3-0 arg0))\n"
      "     (while\n"
      "      (not\n"
      "       (or (null? (cdr s3-0)) (not (pair? (cdr s3-0))))\n"
      "       )\n"
      "      (let*\n"
      "       ((s2-0 (car s3-0)) (s1-0 (car (cdr s3-0))) (v1-1 (arg1 s2-0 s1-0)))\n"
      "       (when\n"
      "        (and (or (not v1-1) (> (the-as int v1-1) 0)) (!= v1-1 #t))\n"
      "        (+! s4-0 1)\n"
      "        (set! (car s3-0) s1-0)\n"
      "        (set! (car (cdr s3-0)) s2-0)\n"
      "        )\n"
      "       )\n"
      "      (set! s3-0 (cdr s3-0))\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, true, "");
}

TEST_F(FormRegressionTestJak1, ExprInlineArrayMethod0) {
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
      "(let\n"
      "  ((v0-0\n"
      "    (object-new\n"
      "     arg0\n"
      "     arg1\n"
      "     (the-as int (+ (-> arg1 size) (* (the-as uint arg2) (-> arg1 heap-base))))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (when\n"
      "   (nonzero? v0-0)\n"
      "   (set! (-> v0-0 length) arg2)\n"
      "   (set! (-> v0-0 allocated-length) arg2)\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTestJak1, ExprInlineArrayMethod4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lw v0, 0(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function inline-array-class int)";

  std::string expected = "(-> arg0 length)";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTestJak1, ExprInlineArrayMethod5) {
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
      "(the-as\n"
      "  int\n"
      "  (+\n"
      "   (-> arg0 type size)\n"
      "   (* (-> arg0 allocated-length) (the-as int (-> arg0 type heap-base)))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "inline-array-class");
}

TEST_F(FormRegressionTestJak1, ExprArrayMethod0) {
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
      "(let\n"
      "  ((v0-1\n"
      "    (object-new\n"
      "     arg0\n"
      "     arg1\n"
      "     (the-as int (+ (-> arg1 size) (* arg3 (if (type-type? arg2 number)\n"
      "                                            (the-as int (-> arg2 size))\n"
      "                                            4\n"
      "                                            )\n"
      "                                    )\n"
      "                  )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (set! (-> v0-1 allocated-length) arg3)\n"
      "  (set! (-> v0-1 length) arg3)\n"
      "  (set! (-> v0-1 content-type) arg2)\n"
      "  v0-1\n"
      "  )";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTestJak1, ExprArrayMethod4) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L90:\n"
      "    lw v0, 0(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function array int)";

  std::string expected = "(-> arg0 length)";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTestJak1, ExprArrayMethod5) {
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
      "(the-as\n"
      "  int\n"
      "  (+\n"
      "   (-> array size)\n"
      "   (* (-> arg0 allocated-length) (if (type-type? (-> arg0 content-type) number)\n"
      "                                  (the-as int (-> arg0 content-type size))\n"
      "                                  4\n"
      "                                  )\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, true, "array");
}

TEST_F(FormRegressionTestJak1, ExprMemCopy) {
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
      "(let\n"
      "  ((v0-0 arg0))\n"
      "  (dotimes\n"
      "   (v1-0 arg2)\n"
      "   (set! (-> (the-as (pointer uint8) arg0)) (-> (the-as (pointer uint8) arg1)))\n"
      "   (&+! arg0 1)\n"
      "   (&+! arg1 1)\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMemSet32) {
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
      "(let\n"
      "  ((v0-0 arg0))\n"
      "  (dotimes\n"
      "   (v1-0 arg1)\n"
      "   (set! (-> (the-as (pointer int32) arg0)) arg2)\n"
      "   (&+! arg0 4)\n"
      "   (nop!)\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMemOr) {
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
      "(let ((v0-0 arg0))\n"
      "  (dotimes (v1-0 arg2)\n"
      "   (logior!\n"
      "    (-> (the-as (pointer uint8) arg0))\n"
      "    (-> (the-as (pointer uint8) arg1))\n"
      "    )\n"
      "   (&+! arg0 1)\n"
      "   (&+! arg1 1)\n"
      "   )\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprFact) {
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

  std::string expected = "(if (= arg0 1) 1 (* arg0 (fact (+ arg0 -1))))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprPrint) {
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

  std::string expected = "((method-of-type (rtype-of arg0) print) arg0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprPrintl) {
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
      "    lwu v1, -4(a0)\n"  // use 1
      "L62:\n"
      "    lwu t9, 24(v1)\n"
      "    jalr ra, t9\n"  // use 2
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

  std::string expected =
      "(begin\n"
      "  (let ((a0-1 arg0)) ((method-of-type (rtype-of a0-1) print) a0-1))\n"
      "  (format #t \"~%\")\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L324", "~%"}});
}

TEST_F(FormRegressionTestJak1, ExprInspect) {
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

  std::string expected = "((method-of-type (rtype-of arg0) inspect) arg0)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprPrintTreeBitmask) {
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
      "  (dotimes\n"
      "   (s4-0 arg1)\n"
      "   (if (not (logtest? arg0 1)) (format #t \"    \") (format #t \"|   \"))\n"
      "   (set! arg0 (shr arg0 1))\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L323", "    "}, {"L322", "|   "}});
}

TEST_F(FormRegressionTestJak1, ExprPrintName) {
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
      "  ((= arg0 arg1)\n"
      "   #t\n"
      "   )\n"
      "  ((and (= (-> arg0 type) string) (= (-> arg1 type) string))\n"
      "   (string= (the-as string arg0) (the-as string arg1))\n"
      "   )\n"
      "  ((and (= (-> arg0 type) string) (= (-> arg1 type) symbol))\n"
      "   (string= (the-as string arg0) (symbol->string arg1))\n"
      "   )\n"
      "  ((and (= (-> arg1 type) string) (= (-> arg0 type) symbol))\n"
      "   (string= (the-as string arg1) (symbol->string arg0))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 "[\t\t[24, \"a1\", \"symbol\"],\n"
                 "\t\t[39, \"a0\", \"symbol\"]]");
}

TEST_F(FormRegressionTestJak1, ExprProfileBarMethod9) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lw v1, 0(a0)\n"
      "    daddiu v1, v1, -2\n"
      "    dsll v1, v1, 4\n"
      "    daddu v1, a0, v1\n"
      "    lwu v0, 16(v1)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function profile-bar int uint)";

  std::string expected = "(-> arg0 data (+ (-> arg0 profile-frame-count) -2) time-stamp)";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprStopwatchElapsedSeconds) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L20:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      "    lw t9, abs(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      //"    lwc1 f0, L20(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, v1\n"
      "    cvt.s.w f1, f1\n"
      "    mul.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function int float)";

  std::string expected = "(let ((v1-0 (abs arg0))) (* 0.0 (the float v1-0)))";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprCopyStringString) {
  std::string func =
      "  sll r0, r0, 0\n"
      "L161:\n"
      "    daddiu v1, a0, 4\n"
      "    daddiu a1, a1, 4\n"
      "    beq r0, r0, L163\n"
      "    sll r0, r0, 0\n"

      "L162:\n"
      "    lbu a2, 0(a1)\n"
      "    sb a2, 0(v1)\n"
      "    daddiu v1, v1, 1\n"
      "    daddiu a1, a1, 1\n"

      "L163:\n"
      "    lbu a2, 0(a1)\n"
      "    bne a2, r0, L162\n"
      "    sll r0, r0, 0\n"

      "    or a1, s7, r0\n"
      "    sb r0, 0(v1)\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function string string string)";
  // this is correct, but an example of where let's might look better if nested,
  // even if it means making the scope a little bit larger...
  std::string expected =
      "(begin\n"
      "  (let\n"
      "   ((v1-0 (-> arg0 data)))\n"
      "   (let\n"
      "    ((a1-1 (-> arg1 data)))\n"
      "    (while\n"
      "     (nonzero? (-> a1-1 0))\n"
      "     (set! (-> v1-0 0) (-> a1-1 0))\n"
      "     (set! v1-0 (&-> v1-0 1))\n"
      "     (set! a1-1 (&-> a1-1 1))\n"
      "     )\n"
      "    )\n"
      "   (set! (-> v1-0 0) (the-as uint 0))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, StringLt) {
  std::string func =
      "  sll r0, r0, 0\n"
      "L91:\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    or a0, gp, r0\n"
      "    lw v1, string(s7)\n"
      "    lwu t9, 32(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    or s4, v1, r0\n"
      "    or a0, s5, r0\n"
      "    lw v1, string(s7)\n"
      "    lwu t9, 32(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    slt a0, s4, v1\n"
      "    movz s4, v1, a0\n"
      "    addiu v1, r0, 0\n"
      "    beq r0, r0, L95\n"
      "    sll r0, r0, 0\n"

      "L92:\n"
      "    daddu a0, v1, gp\n"
      "    lbu a0, 4(a0)\n"
      "    daddu a1, v1, s5\n"
      "    lbu a1, 4(a1)\n"
      "    sltu a0, a0, a1\n"
      "    beq a0, r0, L93\n"
      "    or a0, s7, r0\n"

      "    daddiu v1, s7, #t\n"
      "    or v0, v1, r0\n"
      "    beq r0, r0, L96\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"
      "    beq r0, r0, L94\n"
      "    sll r0, r0, 0\n"

      "L93:\n"
      "    daddu a0, v1, s5\n"
      "    lbu a0, 4(a0)\n"
      "    daddu a1, v1, gp\n"
      "    lbu a1, 4(a1)\n"
      "    sltu a0, a0, a1\n"
      "    beq a0, r0, L94\n"
      "    or a0, s7, r0\n"

      "    or v0, s7, r0\n"
      "    beq r0, r0, L96\n"
      "    sll r0, r0, 0\n"

      "    or v1, r0, r0\n"

      "L94:\n"
      "    daddiu v1, v1, 1\n"

      "L95:\n"
      "    slt a0, v1, s4\n"
      "    bne a0, r0, L92\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    or v0, s7, r0\n"

      "L96:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function string string symbol)";
  std::string expected =
      "(begin\n"
      "  (let ((s4-1 (min (length arg0) (length arg1))))\n"
      "   (dotimes (v1-4 s4-1)\n"
      "    (cond\n"
      "     ((< (-> arg0 data v1-4) (-> arg1 data v1-4))\n"
      "      (return #t)\n"
      "      )\n"
      "     ((< (-> arg1 data v1-4) (-> arg0 data v1-4))\n"
      "      (return #f)\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected, false, "");
}

TEST_F(FormRegressionTestJak1, ExprAssert) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      "    bne s7, a0, L12\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu v1, fp, L17\n"
      "    or a2, a1, r0\n"
      "    or a1, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "L12:\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function symbol string int)";

  std::string expected = "(begin (if (not arg0) (format #t \"A ~A\" arg1)) 0)";
  test_with_expr(func, type, expected, false, "", {{"L17", "A ~A"}});
}

TEST_F(FormRegressionTestJak1, ExprTerminal2) {
  std::string func =
      "sll r0, r0, 0\n"
      "L29:\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      //"    lwc1 f0, L71(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, a0\n"
      "    mul.s f0, f0, f1\n"
      "    mtc1 f1, a1\n"
      "    sub.s f0, f0, f1\n"
      "    mtc1 f1, a2\n"
      "    div.s f0, f0, f1\n"
      "    sqrt.s f0, f0\n"
      "    mtc1 f1, a1\n"
      "    mtc1 f2, a2\n"
      "    mul.s f3, f0, f0\n"
      "    mul.s f2, f2, f3\n"
      "    add.s f1, f1, f2\n"
      "    sub.s f0, f0, f1\n"
      "    mfc1 v0, f0\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16\n";
  std::string type = "(function float float float float float)";

  std::string expected =
      "(let\n"
      "  ((f0-4 (sqrtf (/ (- (* 0.0 arg0) arg1) arg2))))\n"
      "  (- f0-4 (+ arg1 (* arg2 (* f0-4 f0-4))))\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L17", "A ~A"}});
}

TEST_F(FormRegressionTestJak1, MoveFalse) {
  std::string func =
      "sll r0, r0, 0\n"
      "L29:\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    daddiu v1, s7, 8\n"
      "    daddiu a0, a0, 12\n"
      "    andi a0, a0, 1\n"
      "    movz v1, s7, a0\n"
      "    or v0, v1, r0\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16\n";
  std::string type = "(function int symbol)";

  std::string expected = "(logtest? (+ arg0 12) 1)";
  test_with_expr(func, type, expected, false, "", {{"L17", "A ~A"}});
}

// Good for testing that in-place ops (+!) check the _variable_ is the same.
TEST_F(FormRegressionTestJak1, QMemCpy) {
  std::string func =
      "sll r0, r0, 0\n"
      "L78:\n"
      "    or v0, a0, r0\n"
      "    daddiu v1, a2, 15\n"
      "    dsra v1, v1, 4\n"
      "    dsll a2, v1, 4\n"
      "    daddu a0, a0, a2\n"
      "    dsll a2, v1, 4\n"
      "    daddu a1, a1, a2\n"
      "    beq r0, r0, L80\n"
      "    sll r0, r0, 0\n"

      "L79:\n"
      "    daddiu v1, v1, -1\n"
      "    daddiu a0, a0, -16\n"
      "    daddiu a1, a1, -16\n"
      "    lq a2, 0(a1)\n"
      "    sq a2, 0(a0)\n"

      "L80:\n"
      "    bne v1, r0, L79\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n";
  std::string type = "(function pointer pointer int pointer)";
  std::string expected =
      "(let ((v0-0 arg0))\n"
      "  (let* ((v1-1 (/ (+ arg2 15) 16))\n"
      "         (a0-1 (&+ arg0 (* v1-1 16)))\n"
      "         (a1-1 (&+ arg1 (* v1-1 16)))\n"
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

TEST_F(FormRegressionTestJak1, StripStripTrailingWhitespace) {
  std::string func =
      "sll r0, r0, 0\n"
      "L52:\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or gp, a0, r0\n"
      "    or a0, gp, r0\n"
      "    lw v1, string(s7)\n"
      "    lwu t9, 32(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq v1, r0, L56\n"
      "    or v1, s7, r0\n"

      "    daddiu s5, gp, 4\n"
      "    or a0, gp, r0\n"
      "    lw v1, string(s7)\n"
      "    lwu t9, 32(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    daddiu v1, v1, -1\n"
      "    daddu v1, s5, v1\n"
      "    beq r0, r0, L54\n"
      "    sll r0, r0, 0\n"
      "L53:\n"
      "    daddiu v1, v1, -1\n"
      "L54:\n"
      "    daddiu a0, gp, 4\n"
      "    slt a0, v1, a0\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a0\n"
      "    beql s7, a1, L55\n"
      "    or a0, a1, r0\n"

      "    lbu a0, 0(v1)\n"
      "    daddiu a0, a0, -32\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a0\n"
      "    bnel s7, a1, L55\n"
      "    or a0, a1, r0\n"

      "    lbu a0, 0(v1)\n"
      "    daddiu a0, a0, -9\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a0\n"
      "    bnel s7, a1, L55\n"
      "    or a0, a1, r0\n"

      "    lbu a0, 0(v1)\n"
      "    daddiu a0, a0, -13\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a0\n"
      "    bnel s7, a1, L55\n"
      "    or a0, a1, r0\n"

      "    lbu a0, 0(v1)\n"
      "    daddiu a1, a0, -10\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, a1\n"
      "L55:\n"
      "    bne s7, a0, L53\n"
      "    sll r0, r0, 0\n"

      "    or a0, s7, r0\n"
      "    sb r0, 1(v1)\n"
      "    or v1, r0, r0\n"
      "L56:\n"
      "    or v0, s7, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48\n";

  std::string type = "(function string symbol)";
  std::string expected =
      "(begin\n"
      "  (when (nonzero? (length arg0))\n"
      "   (let ((v1-6 (&+ (-> arg0 data) (+ (length arg0) -1))))\n"
      "    (while\n"
      "     (and\n"
      "      (>= (the-as int v1-6) (the-as int (-> arg0 data)))\n"
      "      (or\n"
      "       (= (-> v1-6 0) 32)\n"
      "       (= (-> v1-6 0) 9)\n"
      "       (= (-> v1-6 0) 13)\n"
      "       (= (-> v1-6 0) 10)\n"
      "       )\n"
      "      )\n"
      "     (set! v1-6 (&-> v1-6 -1))\n"
      "     )\n"
      "    (set! (-> v1-6 1) (the-as uint 0))\n"
      "    )\n"
      "   0\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected);
}

// Let bug (github #328)
TEST_F(FormRegressionTestJak1, TimeToGround) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      "    mtc1 f0, r0\n"
      "    addiu v0, r0, 0\n"
      "    beq r0, r0, L3\n"
      "    sll r0, r0, 0\n"

      "L2:\n"
      "    mtc1 f1, a0\n"
      //"    lwc1 f2, L7(fp)\n"
      "    mtc1 f2, r0\n"
      "    mtc1 f3, a1\n"
      "    mul.s f2, f2, f3\n"
      "    sub.s f1, f1, f2\n"
      "    mfc1 a0, f1\n"
      //"    lwc1 f1, L7(fp)\n"
      "    mtc1 f1, r0\n"
      "    mtc1 f2, a0\n"
      "    mul.s f1, f1, f2\n"
      "    add.s f0, f0, f1\n"
      "    daddiu v0, v0, 1\n"

      "L3:\n"
      "    mtc1 f1, a2\n"
      "    neg.s f1, f1\n"
      "    c.lt.s f1, f0\n"
      "    bc1t L2\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0 \n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function float float float float)";
  std::string expected =
      "(let ((f0-0 0.0)\n"
      "     (v0-0 0)\n"
      "     )\n"
      "  (while (< (- arg2) f0-0)\n"
      "   (set! arg0 (- arg0 (* 0.0 arg1)))\n"
      "   (+! f0-0 (* 0.0 arg0))\n"
      "   (+! v0-0 1)\n"
      "   )\n"
      "  (the-as float v0-0)\n"
      "  )";
  test_with_expr(func, type, expected);
}

// Infinite loop bug (github #196)
TEST_F(FormRegressionTestJak1, LoopingCode) {
  std::string func =
      "sll r0, r0, 0\n"
      "L1:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"

      "L2:\n"
      "    lwu s6, 44(s6)\n"
      "    mtlo1 s6\n"
      "    lwu s6, 12(s6)\n"
      "    jalr ra, s6\n"
      "    mflo1 s6\n"

      "    beq r0, r0, L2\n"
      "    sll r0, r0, 0\n"

      "    or v0, s7, r0\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function symbol)";
  std::string expected =
      "(begin\n"
      "  (loop\n"
      "   (suspend)\n"
      "   )\n"
      "  (the-as symbol #f)\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, AbsAsSideEffect) {
  std::string func =
      "sll r0, r0, 0\n"
      "    dsubu v1, a1, a0\n"
      "    or a3, v1, r0\n"
      "    bltzl a3, L14\n"

      "    dsubu a3, r0, a3\n"

      "L14:\n"
      "    slt a3, a2, a3\n"
      "    bne a3, r0, L15\n"
      "    sll r0, r0, 0\n"

      "    or v0, a1, r0\n"
      "    beq r0, r0, L17\n"
      "    sll r0, r0, 0\n"

      "L15:\n"
      "    slt v1, v1, r0\n"
      "    bne v1, r0, L16\n"
      "    sll r0, r0, 0\n"

      "    daddu v0, a0, a2\n"
      "    beq r0, r0, L17\n"
      "    sll r0, r0, 0\n"

      "L16:\n"
      "    dsubu v0, a0, a2\n"

      "L17:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int int)";
  std::string expected =
      "(let* ((v1-0 (- arg1 arg0))\n"
      "      (a3-0 (abs v1-0))\n"  // modified
      "      )\n"
      //"  (set! a3-0 (abs a3-0))\n"
      "  (cond\n"
      "   ((>= arg2 a3-0)\n"
      "    arg1\n"
      "    )\n"
      "   ((>= v1-0 0)\n"
      "    (+ arg0 arg2)\n"
      "    )\n"
      "   (else\n"
      "    (- arg0 arg2)\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected);
}

// for github https://github.com/water111/jak-project/issues/332
// method 11 bit-array
TEST_F(FormRegressionTestJak1, AshPropagation) {
  // (ash a2-0 a3-0)
  std::string func =
      "sll r0, r0, 0\n"
      "    dsra v1, a1, 3\n"
      "    daddu v1, v1, a0\n"
      "    lbu v1, 8(v1)\n"  // (-> arg0 bytes (sar arg1 3)) [LOAD]
      "    addiu a2, r0, 1\n"
      "    andi a3, a1, 7\n"
      "    bgezl a3, L17\n"  // use

      "    dsllv a2, a2, a3\n"  // use, def

      "    dsubu a3, r0, a3\n"  // use
      "    dsrav a2, a2, a3\n"  // (ash 1 (logand arg1 7)

      "L17:\n"
      "    or v1, v1, a2\n"   // (logior (-> arg0 bytes (sar arg1 3)) (ash 1 (logand arg1 7)))
      "    dsra a1, a1, 3\n"  // compute source.
      "    daddu a0, a1, a0\n"

      "    sb v1, 8(a0)\n"
      "    or v0, r0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function bit-array int int)";
  std::string expected =
      "(begin\n"
      "  (logior! (-> arg0 bytes (/ arg1 8)) (ash 1 (logand arg1 7)))\n"
      "  0\n"
      "  )";
  test_with_expr(func, type, expected);
}

// for github https://github.com/water111/jak-project/issues/332
// method 9 bit-array
// also checks output prop.
TEST_F(FormRegressionTestJak1, AshPropagation2) {
  // (ash a2-0 a3-0)
  std::string func =
      "sll r0, r0, 0\n"
      "L20:\n"
      "    dsra v1, a1, 3\n"
      "    daddu v1, v1, a0\n"
      "    lbu v1, 8(v1)\n"
      "    daddiu v0, s7, 8\n"
      "    addiu a0, r0, 1\n"
      "    andi a1, a1, 7\n"
      "    bgezl a1, L21\n"

      "    dsllv a0, a0, a1\n"

      "    dsubu a1, r0, a1\n"
      "    dsrav a0, a0, a1\n"

      "L21:\n"
      "    and v1, v1, a0\n"
      "    movz v0, s7, v1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function bit-array int symbol)";
  std::string expected =
      "(let ((v1-2 (-> arg0 bytes (/ arg1 8))))\n"
      "  (logtest? v1-2 (ash 1 (logand arg1 7)))\n"
      "  )";
  test_with_expr(func, type, expected);
}
