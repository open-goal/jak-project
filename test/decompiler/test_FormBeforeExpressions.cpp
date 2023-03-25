#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

TEST_F(FormRegressionTestJak1, StringTest) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  TestSettings settings;
  settings.strings = {{"L100", "testing-string"}, {"L101", "testing-string-2"}};
  auto test = make_function(func, TypeSpec("function", {TypeSpec("none")}), settings);

  EXPECT_EQ(test->file.get_goal_string_by_label(test->file.get_label_by_name("L100")),
            "testing-string");
  EXPECT_EQ(test->file.get_goal_string_by_label(test->file.get_label_by_name("L101")),
            "testing-string-2");
}

TEST_F(FormRegressionTestJak1, SimplestTest) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object)";
  std::string expected = "(begin (set! v0-0 a0-0) (ret-value v0-0))";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Op3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L308:\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(begin (set! v0-0 (*.si a0-0 a1-0)) (ret-value v0-0))";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Division) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L307:\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(begin (set! v0-0 (/.si a0-0 a1-0)) (ret-value v0-0))";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Ash) {
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
      "    daddu sp, sp, r0\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0";
  std::string type = "(function int int int)";
  std::string expected = "(begin (set! v1-0 a0-0) (set! v0-0 (ash.si v1-0 a1-0)) (ret-value v0-0))";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Abs) {
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
  std::string expected = "(begin (set! v0-0 a0-0) (set! v0-0 (abs v0-0)) (ret-value v0-0))";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Min) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movz v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (set! v1-0 a1-0)\n"
      "  (set! v0-1 (min.si v0-0 v1-0))\n"
      "  (ret-value v0-1)\n"
      "  )";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, Max) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L299:\n"
      "    or v0, a0, r0\n"
      "    or v1, a1, r0\n"
      "    slt a0, v0, v1\n"
      "    movn v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected =
      "(begin\n"
      "  (set! v0-0 a0-0)\n"
      "  (set! v1-0 a1-0)\n"
      "  (set! v0-1 (max.si v0-0 v1-0))\n"
      "  (ret-value v0-1)\n"
      "  )";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, FormatString) {
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

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function bfloat bfloat)";
  std::string expected =
      "(begin\n"
      "  (set! t9-0 format)\n"
      "  (set! a0-1 #t)\n"
      "  (set! a1-0 L343)\n"
      "  (set! f0-0 (-> a0-0 data))\n"
      "  (set! a2-0 (fpr->gpr f0-0))\n"
      "  (call! a0-1 a1-0 a2-0)\n"  // #t, "~f", the float
      "  (set! v0-1 a0-0)\n"
      "  (ret-value v0-1)\n"
      "  )";
  test_no_expr(func, type, expected, false, "", {{"L343", "~f"}});
}

TEST_F(FormRegressionTestJak1, WhileLoop) {
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
      "   (begin (set! v1-0 (-> v1-0 parent)) (= v1-0 a0-1))\n"
      "   (if\n"
      "    (= v1-0 a1-0)\n"
      "    (return (begin (set! v1-1 #t) (set! v0-0 v1-1)))\n"
      "    )\n"
      "   )\n"
      "  (set! v0-0 #f)\n"
      "  (ret-value v0-0)\n"
      "  )";
  test_no_expr(func, type, expected);
}

// Note - this test looks weird because or's aren't fully processed at this point.
TEST_F(FormRegressionTestJak1, Or) {
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
      "    (or\n"
      "     (begin\n"
      "      (set! a0-0 (-> a0-0 parent))\n"
      "      (set! a3-0 (= a0-0 v1-0))\n"
      "      a3-0\n"  // this sets a2-0, the unused result of the OR. it gets a separate
                      // variable because it's not used.
      "      )\n"
      "     (set! a2-1 (zero? a0-0))\n"  // so this should be a2-1.
      "     )\n"
      "    a2-1\n"
      "    )\n"
      "   (if\n"
      "    (= a0-0 a1-0)\n"
      "    (return (begin (set! v1-1 #t) (set! v0-0 v1-1)))\n"
      "    )\n"
      "   )\n"
      "  (set! v0-0 #f)\n"
      "  (ret-value v0-0)\n"
      "  )";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, DynamicMethodAccess) {
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
      "  (set! v1-0 (sll a1-0 2))\n"
      "  (set! v1-1 (+ v1-0 a0-0))\n"
      "  (set! v1-2 (dyn-method-access v1-1))\n"  // get the method of the given type.
      "  (until\n"
      "   (!= v0-0 v1-2)\n"  // actually goes after the body, so it's fine to refer to v1-2
      "   (if\n"
      "    (begin\n"
      "     (if\n"
      "      (begin (set! a2-0 object) (= a0-0 a2-0))\n"               // if we reached the top
      "      (return (begin (set! v1-3 nothing) (set! v0-0 v1-3)))\n"  // return
                                                                       // nothing.
      "      )\n"
      "     (set! a0-0 (-> a0-0 parent))\n"  // get next parent type
      "     (set! a2-2 (sll a1-0 2))\n"      // fancy access
      "     (set! a2-3 (+ a2-2 a0-0))\n"
      "     (set! v0-0 (dyn-method-access a2-3))\n"  // get method (in v0-1, the same var as loop
                                                     // condition)
      "     (zero? v0-0)\n"                          // is it defined?
      "     )\n"
      "    (return (begin (set! v1-4 nothing) (set! v0-0 v1-4)))\n"  // also
                                                                     // return
                                                                     // nothing.
      "    )\n"
      "   )\n"
      "  (ret-value v0-0)\n"
      "  )";
  test_no_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, SimpleLoopMergeCheck) {
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
  std::string type = "(function object int)";
  std::string expected =
      "(begin\n"
      "  (set! v1-0 0)\n"
      "  (while\n"
      "   (<.si v1-0 a1-0)\n"
      "   (nop!)\n"
      "   (nop!)\n"
      "   (set! a0-0 (cdr a0-0))\n"  // should have merged
      "   (set! v1-0 (+ v1-0 1))\n"  // also should have merged
      "   )\n"
      "  (set! v0-0 (car a0-0))\n"
      "  (ret-value v0-0)\n"
      "  )";
  test_no_expr(func, type, expected, true);
}

TEST_F(FormRegressionTestJak1, And) {
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
      "    daddu sp, sp, r0";
  std::string type = "(function pair int)";
  std::string expected =
      "(begin\n"
      "(cond\n"
      "  ((begin (set! v1-0 '()) (= a0-0 v1-0)) (set! v0-0 0))\n"  // should be a case, not a return
      "  (else\n"
      "   (set! v1-1 (cdr a0-0))\n"  // v1-1 iteration.
      "   (set! v0-0 1)\n"           // v0-1 count
      "   (while\n"
      "    (begin\n"
      "     (and\n"
      "      (begin (set! a0-1 '()) (set! a1-0 (!= v1-1 a0-1)) a1-0)\n"     // check v1-1
      "      (begin (set! a0-3 (sll v1-1 62)) (set! a0-2 (<0.si a0-3)))\n"  // check v1-1
      "      )\n"
      "     a0-2\n"  // this variable doesn't appear, but is set by the and.
      "     )\n"
      "    (set! v0-0 (+ v0-0 1))\n"  // merged (and the result)
      "    (set! v1-1 (cdr v1-1))\n"  // also merged.
      "    )\n"
      "   )\n"
      "  )"
      "(ret-value v0-0))\n";
  test_no_expr(func, type, expected, true);
}

TEST_F(FormRegressionTestJak1, FunctionCall) {
  // nmember
  std::string func =
      "    sll r0, r0, 0\n"

      "L252:\n"
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
      "(begin (if\n"  // this if needs regrouping.
      "  (begin\n"
      "   (while\n"
      "    (begin\n"
      "     (or\n"
      "      (begin (set! v1-0 '()) (set! a0-1 (= a1-0 v1-0)) a0-1)\n"  // got empty list.
      "      (begin\n"
      "       (set! t9-0 name=)\n"
      "       (set! a0-2 (car a1-0))\n"
      "       (set! a1-1 a0-0)\n"
      "       (set! v0-0 (call! a0-2 a1-1))\n"
      "       (set! v1-1 v0-0)\n"  // name match
      "       )\n"
      "      )\n"
      "     (not v1-1)\n"  // no name match AND no empty list.
      "     )\n"
      "    (set! a1-0 (cdr a1-0))\n"  // get next (merged)
      "    )\n"
      "   (set! v1-3 '())\n"  //
      "   (!= a1-0 v1-3)\n"   // IF CONDITION
      "   )\n"
      "  (set! v0-1 a1-0)\n"  // not empty, so return the result
      "  )"                   // the (set! v0 #f) from the if is added later.
      "  (ret-value v0-1))\n";
  test_no_expr(func, type, expected, true);
}

TEST_F(FormRegressionTestJak1, NestedAndOr) {
  std::string func =
      "    sll r0, r0, 0\n"

      "L200:\n"
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
  std::string expected =
      "(begin\n"
      "  (set! s4-0 -1)\n"  // s4-0 = flag
      "  (while\n"
      "   (nonzero? s4-0)\n"   // there is stuff to do...
      "   (set! s4-0 0)\n"     // flag = 0
      "   (set! s3-0 a0-0)\n"  // s3 = list-iter
      "   (while\n"
      "    (begin\n"
      "     (or\n"
      "      (begin\n"
      "       (set! v1-6 (cdr s3-0))\n"  // s3-0 = cdr
      "       (set! a0-4 '())\n"
      "       (set! a0-5 (= v1-6 a0-4))\n"
      "       a0-5\n"  // cdr = empty list (sets v1-7 secretly)
      "       )\n"
      "      (begin\n"
      "       (set! v1-8 (cdr s3-0))\n"
      "       (set! v1-9 (sll v1-8 62))\n"
      "       (set! v1-7 (>=0.si v1-9))\n"  // car is not a list.
      "       )\n"
      "      )\n"
      "     (not v1-7)\n"  // while we still have an iterable list...
      "     )\n"
      "    (when\n"
      "     (begin\n"
      "      (and\n"
      "       (begin\n"
      "        (or\n"
      "         (begin\n"
      "          (set! s2-0 (car s3-0))\n"  // s2 = car
      "          (set! v1-0 (cdr s3-0))\n"
      "          (set! s1-0 (car v1-0))\n"         // s1 = cadr
      "          (set! t9-0 a1-0)\n"               // func
      "          (set! a0-1 s2-0)\n"               // car
      "          (set! a1-1 s1-0)\n"               // cadr
      "          (set! v0-0 (call! a0-1 a1-1))\n"  // compare!
      "          (set! v1-1 v0-0)\n"
      "          (not v1-1)\n"  // result is false (secretly sets a0-2)
      "          )\n"
      "         (set! a0-2 (>0.si v1-1))\n"  // >0
      "         )\n"
      "        a0-2\n"  // false or >0
      "        )\n"
      "       (begin (set! a0-3 #t) (set! v1-2 (!= v1-1 a0-3)))\n"  // not #t
      "       )\n"
      "      v1-2\n"  // (and (or false >0) (not #t))
      "      )\n"
      "     (set! s4-0 (+ s4-0 1))\n"  // increment, merge
      "     (set! (car s3-0) s1-0)\n"  // set iter's car to cadr
      "     (set! v1-4 (cdr s3-0))\n"  // current cdr
      "     (set! (car v1-4) s2-0)\n"  // set cadr
      "     (set! v1-5 s2-0)\n"
      "     )\n"
      "    (set! s3-0 (cdr s3-0))\n"  // increment!
      "    )\n"
      "   )\n"
      "  (set! v0-1 a0-0)\n"
      "  (ret-value v0-1)\n"
      "  )";
  test_no_expr(func, type, expected, true);
}

TEST_F(FormRegressionTestJak1, NewMethod) {
  // inline-array-class new
  std::string func =
      "    sll r0, r0, 0\n"

      "L198:\n"
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
      "(begin (when\n"
      "  (begin\n"
      "   (set! v1-0 object)\n"
      "   (set! t9-0 (method-of-type v1-0 new))\n"  // object new
      "   (set! v1-1 a1-0)\n"                       // ?
      "   (set! a2-1 (-> a1-0 size))\n"             // math
      "   (set! a1-1 (-> a1-0 heap-base))\n"
      "   (set! a1-2 (*.ui a2-0 a1-1))\n"
      "   (set! a2-2 (+ a2-1 a1-2))\n"
      "   (set! a1-3 v1-1)\n"  // size!
      "   (set! v0-0 (call! a0-0 a1-3 a2-2))\n"
      "   (nonzero? v0-0)\n"  // only if we got memory...
      "   )\n"
      "  (set! (-> v0-0 length) a2-0)\n"  // store size
      "  (set! (-> v0-0 allocated-length) a2-0)\n"
      "  )"
      "  (ret-value v0-0))\n";
  test_no_expr(func, type, expected, false, "inline-array-class");
}

TEST_F(FormRegressionTestJak1, Recursive) {
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

      "    addiu v0, r0, 1\n"
      "    beq r0, r0, L67\n"
      "    sll r0, r0, 0\n"

      "L66:\n"
      "    lw t9, fact(s7)\n"
      "    daddiu a0, gp, -1\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    mult3 v0, gp, v0\n"
      "L67:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function int int)";
  std::string expected =
      "(begin (cond\n"
      "  ((begin (set! v1-0 1) (= a0-0 v1-0)) (set! v0-0 1))\n"  // base
      "  (else\n"
      "   (set! t9-0 fact)\n"  // recurse!
      "   (set! a0-1 (+ a0-0 -1))\n"
      "   (set! v0-1 (call! a0-1))\n"
      "   (set! v0-0 (*.si a0-0 v0-1))\n"  // not quite a tail call...
      "   )\n"
      "  )"
      "  (ret-value v0-0))\n";
  test_no_expr(func, type, expected, false);
}

TEST_F(FormRegressionTestJak1, TypeOf) {
  std::string func =
      "    sll r0, r0, 0\n"

      "L63:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"

      "    dsll32 v1, a0, 29\n"
      "    beql v1, r0, L64\n"
      "    lw v1, binteger(s7)\n"

      "    bgtzl v1, L64\n"
      "    lw v1, pair(s7)\n"

      "    lwu v1, -4(a0)\n"

      "L64:\n"
      "    lwu t9, 24(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function object object)";
  std::string expected =
      "(begin\n"
      "  (set! v1-1 (rtype-of a0-0))\n"
      "  (set! t9-0 (method-of-type v1-1 print))\n"  // print method.
      "  (set! v0-0 (call! a0-0))\n"
      "  (ret-value v0-0)\n"
      "  )";
  test_no_expr(func, type, expected, false);
}
