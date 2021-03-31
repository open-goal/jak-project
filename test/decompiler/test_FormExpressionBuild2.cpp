#include "gtest/gtest.h"
#include "FormRegressionTest.h"

using namespace decompiler;

// tests stack variables
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
  std::string type = "(function matrix matrix matrix matrix)";
  std::string expected =
      "(begin\n"
      "  (let ((s5-0 (new (quote stack) (quote matrix))))\n"
      "   (set! (-> s5-0 vector 0 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 1 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 2 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 3 quad) (the-as uint128 0))\n"
      "   (matrix*! s5-0 arg1 arg2)\n"
      "   (set! (-> arg0 vector 0 quad) (-> s5-0 vector 0 quad))\n"
      "   (set! (-> arg0 vector 1 quad) (-> s5-0 vector 1 quad))\n"
      "   (set! (-> arg0 vector 2 quad) (-> s5-0 vector 2 quad))\n"
      "   (set! (-> arg0 vector 3 quad) (-> s5-0 vector 3 quad))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_vars(func, type, expected,
                       "[\n"
                       "        [16, \"matrix\"]\n"
                       "    ]");
}

// TODO- this should also work without the cast, but be uglier.
TEST_F(FormRegressionTest, VectorXQuaternionWithCast) {
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
      "    lw t9, quaternion->matrix(s7)\n"
      "    or a0, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    daddu v1, r0, s5\n"
      "    lq v1, 0(v1)\n"

      "    sq v1, 0(gp)\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function quaternion quaternion quaternion)";
  std::string expected =
      "(begin\n"
      "  (let ((s5-0 (new (quote stack) (quote matrix))))\n"
      "   (set! (-> s5-0 vector 0 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 1 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 2 quad) (the-as uint128 0))\n"
      "   (set! (-> s5-0 vector 3 quad) (the-as uint128 0))\n"
      "   (quaternion->matrix s5-0 arg1)\n"
      "   (set! (-> arg0 vec quad) (-> (the-as (pointer uint128) (-> s5-0 data)) 0))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_vars(func, type, expected,
                       "[\n"
                       "        [16, \"matrix\"]\n"
                       "    ]",
                       "[[10, \"v1\", \"(pointer uint128)\"]]");
}

TEST_F(FormRegressionTest, EliminateFloatDeadSet) {
  std::string func =
      "sll r0, r0, 0\n"
      "L32:\n"
      "    daddiu sp, sp, -16\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      "    lwu v1, 4(a0)\n"
      "    mtc1 f0, v1\n"
      "    cvt.s.w f1, f0\n"
      //"    lwc1 f0, L83(fp)\n"
      "    mtc1 f0, r0\n"
      "    lw a1, *display*(s7)\n"
      "    ld a1, 780(a1)\n"
      "    divu a1, v1\n"
      "    mfhi v1\n"
      "    mtc1 f2, v1\n"
      "    cvt.s.w f2, f2\n"
      "    lwc1 f3, 0(a0)\n"
      "    add.s f2, f2, f3\n"
      "    div.s f3, f2, f1\n"
      "    cvt.w.s f3, f3\n"
      "    cvt.s.w f3, f3\n"
      "    mul.s f3, f3, f1\n"
      "    sub.s f2, f2, f3\n"
      "    div.s f1, f2, f1\n"
      "    mul.s f0, f0, f1\n"
      //"    lwc1 f1, L84(fp)\n"
      "    mtc1 f1, r0\n"
      //"    lwc1 f2, L83(fp)\n"
      "    mtc1 f2, r0\n"
      "    lwc1 f3, 12(a0)\n"
      "    mul.s f2, f2, f3\n"
      "    sub.s f1, f1, f2\n"
      //"    lwc1 f2, L84(fp)\n"
      "    mtc1 f2, r0\n"
      //"    lwc1 f3, L83(fp)\n"
      "    mtc1 f3, r0\n"
      "    lwc1 f4, 8(a0)\n"
      "    mul.s f3, f3, f4\n"
      "    sub.s f2, f2, f3\n"
      //"    lwc1 f3, L84(fp)\n"
      "    mtc1 f3, r0\n"
      "    add.s f3, f3, f1\n"
      "    c.lt.s f0, f3\n"
      "    bc1t L33\n"
      "    sll r0, r0, 0\n"

      "    mtc1 f0, r0\n"
      "    mfc1 v1, f0\n"
      "    beq r0, r0, L36\n"
      "    sll r0, r0, 0\n"

      "L33:\n"
      //"    lwc1 f3, L84(fp)\n"e
      "    mtc1 f3, r0\n"
      "    c.lt.s f3, f0\n"
      "    bc1f L34\n"
      "    sll r0, r0, 0\n"

      //"    lwc1 f2, L84(fp)\n"
      "    mtc1 f2, r0\n"
      //"    lwc1 f3, L82(fp)\n"
      "    mtc1 f3, r0\n"
      "    add.s f0, f3, f0\n"
      "    div.s f0, f0, f1\n"
      "    sub.s f0, f2, f0\n"
      "    mfc1 v1, f0\n"
      "    beq r0, r0, L36\n"
      "    sll r0, r0, 0\n"

      "L34:\n"
      "    c.lt.s f0, f2\n"
      "    bc1t L35\n"
      "    sll r0, r0, 0\n"

      //"    lwc1 f0, L84(fp)\n"
      "    mtc1 f0, r0\n"
      "    mfc1 v1, f0\n"
      "    beq r0, r0, L36\n"
      "    sll r0, r0, 0\n"

      "L35:\n"
      "    div.s f0, f0, f2\n"
      "    mfc1 v1, f0\n"

      "L36:\n"
      "    mfc1 v0, f0\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function sync-info-paused float)";
  std::string expected =
      "(let* ((v1-0 (-> arg0 period))\n"
      "      (f1-0 (the float v1-0))\n"
      "      (f0-1 0.0)\n"
      "      (f2-2\n"
      "       (+\n"
      "        (the float (mod (-> *display* base-frame-counter) v1-0))\n"
      "        (-> arg0 offset)\n"
      "        )\n"
      "       )\n"
      "      (f0-2\n"
      "       (* f0-1 (/ (- f2-2 (* (the float (the int (/ f2-2 f1-0))) f1-0)) f1-0))\n"
      "       )\n"
      "      (f1-3 (- 0.0 (* 0.0 (-> arg0 pause-after-in))))\n"
      "      (f2-7 (- 0.0 (* 0.0 (-> arg0 pause-after-out))))\n"
      "      )\n"
      "  (cond\n"
      "   ((>= f0-2 (+ 0.0 f1-3))\n"
      "    0.0\n"
      "    )\n"
      "   ((< 0.0 f0-2)\n"
      "    (- 0.0 (/ (+ 0.0 f0-2) f1-3))\n"
      "    )\n"
      "   ((>= f0-2 f2-7)\n"
      "    0.0\n"
      "    )\n"
      "   (else\n"
      "    (/ f0-2 f2-7)\n"
      "    )\n"
      "   )\n"
      "  )";
  test_with_stack_vars(func, type, expected, "[]");
}

TEST_F(FormRegressionTest, IterateProcessTree) {
  std::string func =
      "sll r0, r0, 0\n"

      "    daddiu sp, sp, -80\n"
      "    sd ra, 0(sp)\n"
      "    sq s3, 16(sp)\n"
      "    sq s4, 32(sp)\n"
      "    sq s5, 48(sp)\n"
      "    sq gp, 64(sp)\n"

      "    or s3, a0, r0\n"
      "    or gp, a1, r0\n"
      "    or s5, a2, r0\n"
      "    lwu v1, 4(s3)\n"
      "    andi v1, v1, 256\n"
      "    bnel v1, r0, L113\n"

      "    daddiu s4, s7, 8\n"

      "    or t9, gp, r0\n"
      "    or a0, s3, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    or s4, v0, r0\n"

      "L113:\n"
      "    daddiu v1, s7, dead\n"
      "    bne s4, v1, L114\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    beq r0, r0, L117\n"
      "    sll r0, r0, 0\n"

      "L114:\n"
      "    lwu v1, 16(s3)\n"
      "    beq r0, r0, L116\n"
      "    sll r0, r0, 0\n"

      "L115:\n"
      "    lwu a0, 0(v1)\n"
      "    lwu s3, 12(a0)\n"
      "    lw t9, iterate-process-tree(s7)\n"
      "    lwu a0, 0(v1)\n"
      "    or a1, gp, r0\n"
      "    or a2, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, s3, r0\n"
      "    or a0, v1, r0\n"

      "L116:\n"
      "    bne s7, v1, L115\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"

      "L117:\n"
      "    or v0, s4, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 64(sp)\n"
      "    lq s5, 48(sp)\n"
      "    lq s4, 32(sp)\n"
      "    lq s3, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 80";
  std::string type = "(function process-tree (function object object) kernel-context object)";
  std::string expected =
      "(let ((s4-0 (or (nonzero? (logand (-> arg0 mask) 256)) (arg1 arg0))))\n"
      "  (cond\n"
      "   ((= s4-0 (quote dead))\n"
      "    )\n"
      "   (else\n"
      "    (let ((v1-4 (-> arg0 child)))\n"
      "     (while v1-4\n"
      "      (let ((s3-1 (-> v1-4 0 brother)))\n"
      "       (iterate-process-tree (-> v1-4 0) arg1 arg2)\n"
      "       (set! v1-4 s3-1)\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  s4-0\n"
      "  )";
  test_with_stack_vars(func, type, expected, "[]");
}