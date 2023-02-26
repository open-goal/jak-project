#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

// tests stack variables
TEST_F(FormRegressionTestJak1, MatrixPMult) {
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
      "  (let ((s5-0 (new-stack-matrix0)))\n"
      "   (matrix*! s5-0 arg1 arg2)\n"
      "   (set! (-> arg0 vector 0 quad) (-> s5-0 vector 0 quad))\n"
      "   (set! (-> arg0 vector 1 quad) (-> s5-0 vector 1 quad))\n"
      "   (set! (-> arg0 vector 2 quad) (-> s5-0 vector 2 quad))\n"
      "   (set! (-> arg0 vector 3 quad) (-> s5-0 vector 3 quad))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_structures(func, type, expected,
                             "[\n"
                             "        [16, \"matrix\"]\n"
                             "    ]");
}

// TODO- this should also work without the cast, but be uglier.
TEST_F(FormRegressionTestJak1, VectorXQuaternionWithCast) {
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
      "  (let ((s5-0 (new-stack-matrix0)))\n"
      "   (quaternion->matrix s5-0 arg1)\n"
      "   (set! (-> arg0 vec quad) (-> (the-as (pointer uint128) (-> s5-0 vector)) 0))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_structures(func, type, expected,
                             "[\n"
                             "        [16, \"matrix\"]\n"
                             "    ]",
                             "[[10, \"v1\", \"(pointer uint128)\"]]");
}

TEST_F(FormRegressionTestJak1, EliminateFloatDeadSet) {
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
      "        (the float (mod (the-as uint (-> *display* base-frame-counter)) v1-0))\n"
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
  test_with_stack_structures(func, type, expected, "[]");
}

TEST_F(FormRegressionTestJak1, IterateProcessTree) {
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
      "(let\n"
      "  ((s4-0 (or (logtest? (-> arg0 mask) (process-mask process-tree)) (arg1 arg0)))\n"
      "   )\n"
      "  (cond\n"
      "   ((= s4-0 'dead)\n"
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
  test_with_stack_structures(func, type, expected, "[]");
}

TEST_F(FormRegressionTestJak1, InspectVifStatBitfield) {
  std::string func =
      "sll r0, r0, 0\n"

      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L37\n"
      "    or a2, gp, r0\n"
      "    daddiu a3, s7, vif-stat\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L36\n"
      "    dsll32 v1, gp, 30\n"
      "    dsrl32 a2, v1, 30\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L35\n"
      "    dsll32 v1, gp, 29\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L34\n"
      "    dsll32 v1, gp, 25\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L33\n"
      "    dsll32 v1, gp, 23\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L32\n"
      "    dsll32 v1, gp, 22\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L31\n"
      "    dsll32 v1, gp, 21\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L30\n"
      "    dsll32 v1, gp, 20\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L29\n"
      "    dsll32 v1, gp, 19\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L28\n"
      "    dsll32 v1, gp, 18\n"
      "    dsrl32 a2, v1, 31\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L27\n"
      "    dsll32 v1, gp, 4\n"
      "    dsrl32 a2, v1, 28\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function vif-stat vif-stat)";
  std::string expected =
      "(begin\n"
      "  (format #t \"[~8x] ~A~%\" arg0 (quote vif-stat))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 vps))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 vew))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 mrk))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 vss))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 vfs))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 vis))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 int))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 er0))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 er1))\n"
      "  (format #t \"~T ~D~%\" (-> arg0 fqc))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {{"L37", "[~8x] ~A~%"},
                  {"L36", "~T ~D~%"},
                  {"L35", "~T ~D~%"},
                  {"L34", "~T ~D~%"},
                  {"L33", "~T ~D~%"},
                  {"L32", "~T ~D~%"},
                  {"L31", "~T ~D~%"},
                  {"L30", "~T ~D~%"},
                  {"L29", "~T ~D~%"},
                  {"L28", "~T ~D~%"},
                  {"L27", "~T ~D~%"}});
}

TEST_F(FormRegressionTestJak1, InspectHandleBitfield) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L79\n"
      "    or a2, gp, r0\n"
      "    daddiu a3, s7, handle\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L32\n"
      "    dsll32 v1, gp, 0\n"
      "    dsrl32 a2, v1, 0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L31\n"
      "    dsra32 a2, gp, 0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function handle handle)";
  std::string expected =
      "(begin\n"
      "  (format #t \"[~8x] ~A~%\" arg0 (quote handle))\n"
      "  (format #t \"~Tprocess: #x~X~%\" (-> arg0 process))\n"
      "  (format #t \"~Tpid: ~D~%\" (-> arg0 pid))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {{"L79", "[~8x] ~A~%"}, {"L32", "~Tprocess: #x~X~%"}, {"L31", "~Tpid: ~D~%"}});
}

TEST_F(FormRegressionTestJak1, InspectDmaTagBitfield) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L65\n"
      "    or a2, gp, r0\n"
      "    daddiu a3, s7, dma-tag\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L37\n"
      "    dsll32 v1, gp, 16\n"
      "    dsrl32 a2, v1, 16\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L36\n"
      "    dsll32 v1, gp, 4\n"
      "    dsrl32 a2, v1, 30\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L35\n"
      "    dsll32 v1, gp, 1\n"
      "    dsrl32 a2, v1, 29\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L34\n"
      "    srl a2, gp, 31\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L33\n"
      "    dsll v1, gp, 1\n"
      "    dsrl32 a2, v1, 1\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L32\n"
      "    dsrl32 a2, gp, 31\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function dma-tag dma-tag)";
  std::string expected =
      "(begin\n"
      "  (format #t \"[~8x] ~A~%\" arg0 (quote dma-tag))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 qwc))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 pce))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 id))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 irq))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 addr))\n"
      "  (format #t \"~Ta: ~D~%\" (-> arg0 spr))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {
                     {"L65", "[~8x] ~A~%"},
                     {"L37", "~Ta: ~D~%"},
                     {"L36", "~Ta: ~D~%"},
                     {"L35", "~Ta: ~D~%"},
                     {"L34", "~Ta: ~D~%"},
                     {"L33", "~Ta: ~D~%"},
                     {"L32", "~Ta: ~D~%"},
                 });
}

// Tests nonzero-check on bitfield
TEST_F(FormRegressionTestJak1, DmaSyncCrash) {
  std::string func =
      "sll r0, r0, 0\n"
      "L46:\n"
      "    lui v1, 76\n"
      "    ori v1, v1, 19264\n"
      "    beq r0, r0, L49\n"
      "    sll r0, r0, 0\n"

      "L47:\n"
      "    bne v1, r0, L48\n"
      "    sll r0, r0, 0\n"

      "    sd r0, 2(r0)\n"
      "    or a1, r0, r0\n"
      "    beq r0, r0, L49\n"
      "    sll r0, r0, 0\n"

      "L48:\n"
      "    daddiu v1, v1, -1\n"
      "    or a1, v1, r0\n"

      "L49:\n"
      "    lwu a1, 0(a0)\n"
      "    andi a1, a1, 256\n"
      "    bne a1, r0, L47\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v0, r0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dma-bank int)";
  std::string expected =
      "(begin\n"
      "  (let ((v1-0 #x4c4b40))\n"
      "   (while (nonzero? (-> arg0 chcr str))\n"
      "    (cond\n"
      "     ((zero? v1-0)\n"
      "      (crash!)\n"
      "      0\n"
      "      )\n"
      "     (else\n"
      "      (+! v1-0 -1)\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, DmaSend) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or gp, a0, r0\n"
      "    or s4, a1, r0\n"
      "    or s5, a2, r0\n"
      "    lw t9, dma-sync(s7)\n"
      "    or a0, gp, r0\n"
      "    addiu a1, r0, 0\n"
      "    addiu a2, r0, 0\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    lw t9, flush-cache(s7)\n"
      "    addiu a0, r0, 0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sync.l\n"
      "    lui v1, 4095\n"
      "    ori v1, v1, 65535\n"
      "    and v1, v1, s4\n"
      "    lui a0, 28672\n"
      "    lui a1, 28672\n"
      "    and a1, a1, s4\n"
      "    bne a1, a0, L44\n"
      "    sll r0, r0, 0\n"

      "    ori a0, r0, 32768\n"
      "    dsll a0, a0, 16\n"
      "    beq r0, r0, L45\n"
      "    sll r0, r0, 0\n"

      "L44:\n"
      "    addiu a0, r0, 0\n"

      "L45:\n"
      "    or v1, v1, a0\n"
      "    sw v1, 16(gp)\n"
      "    sw s5, 32(gp)\n"
      "    sync.l\n"
      "    addiu v1, r0, 256\n"
      "    sw v1, 0(gp)\n"
      "    sync.l\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function dma-bank uint uint int)";
  std::string expected =
      "(begin\n"
      "  (dma-sync (the-as pointer arg0) 0 0)\n"
      "  (flush-cache 0)\n"
      "  (.sync.l)\n"
      "  (set!\n"
      "   (-> arg0 madr)\n"
      "   (logior (logand #xfffffff arg1) (if (= (logand #x70000000 arg1) #x70000000)\n"
      "                                    (shl #x8000 16)\n"
      "                                    0\n"
      "                                    )\n"
      "    )\n"
      "   )\n"
      "  (set! (-> arg0 qwc) arg2)\n"
      "  (.sync.l)\n"
      "  (set! (-> arg0 chcr) (new 'static 'dma-chcr :str 1))\n"
      "  (.sync.l)\n"
      "  0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, DmaInitialize) {
  std::string func =
      "sll r0, r0, 0\n"
      "    lui v1, 4096\n"
      "    ori v1, v1, 14336\n"
      "    lwu v1, 32(v1)\n"
      "    addiu a0, r0, -3\n"
      "    and v1, v1, a0\n"
      "    ori v1, v1, 2\n"
      "    lui a0, 4096\n"
      "    ori a0, a0, 14336\n"
      "    sw v1, 32(a0)\n"
      "    lui v1, 4096\n"
      "    ori v1, v1, 15360\n"
      "    lwu v1, 32(v1)\n"
      "    addiu a0, r0, -3\n"
      "    and v1, v1, a0\n"
      "    ori v1, v1, 2\n"
      "    lui a0, 4096\n"
      "    ori a0, a0, 15360\n"
      "    sw v1, 32(a0)\n"
      "    or v0, r0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int)";
  std::string expected =
      "(begin\n"
      "  (set! (-> (the-as vif-bank #x10003800) err me0) 1)\n"
      "  (set! (-> (the-as vif-bank #x10003c00) err me0) 1)\n"
      "  0\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 "[[1, \"v1\", \"vif-bank\"], [8, \"v1\", \"vif-bank\"], [6, \"a0\", "
                 "\"vif-bank\"], [13, \"a0\", \"vif-bank\"]]");
}

// Dynamic bitfield stuff.
TEST_F(FormRegressionTestJak1, SetDisplayEnv) {
  std::string func =
      "sll r0, r0, 0\n"
      "    ori v1, r0, 65441\n"
      "    sd v1, 0(a0)\n"
      "    addiu v1, r0, 3\n"
      "    sd v1, 8(a0)\n"
      "    dsll32 v1, t2, 23\n"
      "    dsrl32 v1, v1, 23\n"
      "    dsra t2, a2, 6\n"
      "    dsll32 t2, t2, 26\n"
      "    dsrl32 t2, t2, 17\n"
      "    or v1, v1, t2\n"
      "    dsll32 a1, a1, 27\n"
      "    dsrl32 a1, a1, 12\n"
      "    or v1, v1, a1\n"
      "    sd v1, 16(a0)\n"
      "    addiu v1, r0, 2559\n"
      "    dsll32 v1, v1, 0\n"
      "    daddiu a1, a2, 2559\n"
      "    div a1, a2\n"
      "    mflo a1\n"
      "    daddiu a1, a1, -1\n"
      "    dsll32 a1, a1, 28\n"
      "    dsrl32 a1, a1, 5\n"
      "    or v1, v1, a1\n"
      "    dsll a1, a3, 1\n"
      "    daddiu a1, a1, -1\n"
      "    dsll32 a1, a1, 21\n"
      "    dsrl a1, a1, 9\n"
      "    or v1, v1, a1\n"
      "    addiu a1, r0, 2560\n"
      "    div a1, a2\n"
      "    mflo a1\n"
      "    mult3 a1, t0, a1\n"
      "    daddiu a1, a1, 652\n"
      "    dsll32 a1, a1, 20\n"
      "    dsrl32 a1, a1, 20\n"
      "    or v1, v1, a1\n"
      "    daddiu a1, t1, 50\n"
      "    dsll32 a1, a1, 21\n"
      "    dsrl32 a1, a1, 9\n"
      "    or v1, v1, a1\n"
      "    sd v1, 24(a0)\n"
      "    sd r0, 32(a0)\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function display-env int int int int int int display-env)";
  std::string expected =
      "(begin\n"
      "  (set! (-> arg0 pmode) (new 'static 'gs-pmode :en1 1 :mmod 1 :slbg 1 :alp 255))\n"
      "  (set! (-> arg0 smode2) (new 'static 'gs-smode2 :int 1 :ffmd 1))\n"
      "  (set!\n"
      "   (-> arg0 dspfb)\n"
      "   (new 'static 'gs-display-fb :psm arg1 :fbw (/ arg2 64) :fbp arg6)\n"
      "   )\n"
      "  (set!\n"
      "   (-> arg0 display)\n"
      "   (new\n"
      "    'static\n"
      "    'gs-display\n"
      "    :dw\n"
      "    2559\n"
      "    :dy\n"
      "    (+ arg5 50)\n"
      "    :dx\n"
      "    (+ (* arg4 (/ 2560 arg2)) 652)\n"
      "    :dh\n"
      "    (+ (* arg3 2) -1)\n"
      "    :magh\n"
      "    (+ (/ (+ arg2 2559) arg2) -1)\n"
      "    )\n"
      "   )\n"
      "  (set! (-> arg0 bgcolor) (new 'static 'gs-bgcolor))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, DmaBufferAddVuFunction) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu v1, a1, 16\n"
      "    lw a3, 8(a1)\n"
      "    lw a1, 4(a1)\n"
      "    beq r0, r0, L9\n"
      "    sll r0, r0, 0\n"

      "L6:\n"
      "    addiu t0, r0, 127\n"
      "    or t1, a3, r0\n"
      "    slt t2, t0, t1\n"
      "    movz t0, t1, t2\n"
      "    or t1, a0, r0\n"
      "    lwu t2, 4(t1)\n"
      "    lui t3, 12288\n"
      "    dsll32 t4, t0, 16\n"
      "    dsrl32 t4, t4, 16\n"
      "    or t3, t3, t4\n"
      "    dsll32 t4, v1, 1\n"
      "    dsrl t4, t4, 1\n"
      "    or t3, t3, t4\n"
      "    sd t3, 0(t2)\n"
      "    bne a2, r0, L7\n"
      "    sll r0, r0, 0\n"

      "    addiu t3, r0, 16\n"
      "    beq r0, r0, L8\n"
      "    sll r0, r0, 0\n"

      "L7:\n"
      "    addiu t3, r0, 19\n"

      "L8:\n"
      "    dsll32 t3, t3, 25\n"
      "    dsrl32 t3, t3, 1\n"
      "    sw t3, 8(t2)\n"
      "    lui t3, 18944\n"
      "    dsll32 t4, a1, 16\n"
      "    dsrl32 t4, t4, 16\n"
      "    or t3, t3, t4\n"
      "    dsll t4, t0, 1\n"
      "    dsll32 t4, t4, 24\n"
      "    dsrl32 t4, t4, 8\n"
      "    or t3, t3, t4\n"
      "    sw t3, 12(t2)\n"
      "    daddiu t2, t2, 16\n"
      "    sw t2, 4(t1)\n"
      "    dsll t1, t0, 4\n"
      "    daddu v1, v1, t1\n"

      "    dsubu a3, a3, t0\n"
      "    dsll t0, t0, 1\n"
      "    daddu a1, a1, t0\n"
      "    or t0, a1, r0\n"

      "L9:\n"
      "    slt t0, r0, a3\n"
      "    bne t0, r0, L6\n"
      "    sll r0, r0, 0\n"

      "    or v0, s7, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dma-buffer vu-function int symbol)";
  std::string expected =
      "(begin\n"
      "  (let ((v1-0 (the-as object (+ (the-as uint arg1) 16)))\n"
      "        (a3-0 (-> arg1 qlength))\n"
      "        (a1-1 (-> arg1 origin))\n"
      "        )\n"
      "    (while (> a3-0 0)\n"
      "      (let ((t0-1 (min 127 a3-0)))\n"
      "        (let* ((t1-1 arg0)\n"
      "               (t2-0 (the-as object (-> t1-1 base)))\n"
      "               )\n"
      "          (set! (-> (the-as dma-packet t2-0) dma)\n"
      "                (new 'static 'dma-tag :id (dma-tag-id ref) :addr (the-as int v1-0) :qwc "
      "t0-1)\n"
      "                )\n"
      "          (set! (-> (the-as dma-packet t2-0) vif0) (new 'static 'vif-tag :cmd (if (zero? "
      "arg2)\n"
      "                                                                                  16\n"
      "                                                                                  19\n"
      "                                                                                  )\n"
      "                                                        )\n"
      "                )\n"
      "          (set! (-> (the-as dma-packet t2-0) vif1) (new 'static 'vif-tag :cmd (vif-cmd mpg) "
      ":num (* t0-1 2) :imm a1-1))\n"
      "          (set! (-> t1-1 base) (&+ (the-as pointer t2-0) 16))\n"
      "          )\n"
      "        (set! v1-0 (+ (the-as uint v1-0) (* t0-1 16)))\n"
      "        (set! a3-0 (- a3-0 t0-1))\n"
      "        (+! a1-1 (* t0-1 2))\n"
      "        )\n"
      "      )\n"
      "    )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {}, "[[[9, 33], \"t2\", \"dma-packet\"]]");
}

TEST_F(FormRegressionTestJak1, DmaBucketInsertTag) {
  std::string func =
      "sll r0, r0, 0\n"
      "    dsll v1, a1, 4\n"
      "    daddu v1, a0, v1\n"
      "    lwu a0, 8(v1)\n"
      "    sw a2, 4(a0)\n"
      "    sw a3, 8(v1)\n"
      "    or v0, a2, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dma-bucket int pointer (pointer dma-tag) pointer)";
  std::string expected =
      "(begin\n"
      "  (let ((v1-1 (the-as dma-bucket (+ (the-as uint arg0) (* arg1 16)))))\n"
      "   (set! (-> (the-as dma-bucket (-> v1-1 last)) next) (the-as uint arg2))\n"
      "   (set! (-> v1-1 last) arg3)\n"
      "   )\n"
      "  arg2\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 "[\n"
                 "    [[2, 6], \"v1\", \"dma-bucket\"],\n"
                 "    [3, \"a0\", \"dma-bucket\"]\n"
                 "  ]");
}

TEST_F(FormRegressionTestJak1, StupidFloatMove) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or gp, a0, r0\n"
      //"    lwc1 f0, L47(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, a1\n"
      "    min.s f0, f0, f1\n"
      "    mfc1 s5, f0\n"
      "    mtc1 f0, s5\n"
      "    swc1 f0, 900(gp)\n"
      "    lw t9, get-video-mode(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    daddiu a0, s7, pal\n"
      "    bne v1, a0, L37\n"
      "    sll r0, r0, 0\n"

      //"    lwc1 f0, L42(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, s5\n"
      "    mul.s f0, f0, f1\n"
      "    swc1 f0, 916(gp)\n"
      //"    lwc1 f0, L43(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, s5\n"
      "    mul.s f0, f0, f1\n"
      "    swc1 f0, 904(gp)\n"
      //"    lwc1 f0, L46(fp)\n"
      "    mtc1 f0, r0\n"
      //"    lwc1 f1, L50(fp)\n"
      "    mtc1 f1, r0\n"
      "    mtc1 f2, s5\n"
      "    div.s f1, f1, f2\n"
      "    mul.s f0, f0, f1\n"
      "    swc1 f0, 908(gp)\n"
      //"    lwc1 f0, L49(fp)\n"
      "    mtc1 f0, r0\n"
      "    swc1 f0, 912(gp)\n"
      "    mfc1 v1, f0\n"
      "    beq r0, r0, L38\n"
      "    sll r0, r0, 0\n"

      "L37:\n"
      "    mtc1 f0, s5\n"
      "    swc1 f0, 916(gp)\n"
      //"    lwc1 f0, L44(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, s5\n"
      "    mul.s f0, f0, f1\n"
      "    swc1 f0, 904(gp)\n"
      //"    lwc1 f0, L45(fp)\n"
      "    mtc1 f0, r0\n"
      //"    lwc1 f1, L50(fp)\n"
      "    mtc1 f1, r0\n"
      "    mtc1 f2, s5\n"
      "    div.s f1, f1, f2\n"
      "    mul.s f0, f0, f1\n"
      "    swc1 f0, 908(gp)\n"
      //"    lwc1 f0, L48(fp)\n"
      "    mtc1 f0, r0\n"
      "    swc1 f0, 912(gp)\n"
      "    mfc1 v1, f0\n"

      "L38:\n"
      //"    lwc1 f0, 900(gp)\n"
      "    mtc1 f0, r0\n"
      "    mfc1 v0, f0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function display float float)";
  std::string expected =
      "(begin\n"
      "  (let ((s5-0 (fmin 0.0 arg1)))\n"
      "   (set! (-> arg0 time-ratio) s5-0)\n"
      "   (case (get-video-mode) \n"
      "    (('pal)\n"
      "      (set! (-> arg0 time-adjust-ratio) (* 0.0 s5-0))\n"
      "      (set! (-> arg0 seconds-per-frame) (* 0.0 s5-0))\n"
      "      (set! (-> arg0 frames-per-second) (* 0.0 (/ 0.0 s5-0)))\n"
      "      (set! (-> arg0 time-factor) 0.0)\n"
      "      )\n"
      "    (else\n"
      "     (set! (-> arg0 time-adjust-ratio) s5-0)\n"
      "     (set! (-> arg0 seconds-per-frame) (* 0.0 s5-0))\n"
      "     (set! (-> arg0 frames-per-second) (* 0.0 (/ 0.0 s5-0)))\n"
      "     (set! (-> arg0 time-factor) 0.0)\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  0.0\n"
      "  )";
  test_with_expr(func, type, expected);
}

// gpr->fpr not being propagated
TEST_F(FormRegressionTestJak1, Method11FontContext) {
  std::string func =
      "sll r0, r0, 0\n"
      "    mtc1 f0, a1\n"
      "    cvt.s.w f0, f0\n"
      "    swc1 f0, 20(a0)\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function font-context int font-context)";
  std::string expected =
      "(begin\n"
      "  (set! (-> arg0 origin z) (the float arg1))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected);
}

// 128-bit bitfields, also type for pextuw
TEST_F(FormRegressionTestJak1, Method4ResTag) {
  std::string func =
      "sll r0, r0, 0\n"
      "L135:\n"
      "    pcpyud v1, a0, r0\n"
      "    dsrl32 v1, v1, 31\n"
      "    bne v1, r0, L136\n"
      "    sll r0, r0, 0\n"

      "    pcpyud v1, a0, r0\n"
      "    dsll v1, v1, 1\n"
      "    dsrl32 v1, v1, 17\n"
      "    dsll v0, v1, 2\n"
      "    beq r0, r0, L137\n"
      "    sll r0, r0, 0\n"

      "L136:\n"
      "    pcpyud v1, a0, r0\n"
      "    dsll v1, v1, 1\n"
      "    dsrl32 v1, v1, 17\n"
      "    pextuw a0, r0, a0\n"
      "    lhu a0, 8(a0)\n"
      "    multu3 v0, v1, a0\n"

      "L137:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function res-tag int)";
  std::string expected =
      "(the-as int (if (zero? (-> arg0 inlined?))\n"
      "            (* (-> arg0 elt-count) 4)\n"
      "            (* (-> arg0 elt-count) (-> arg0 elt-type size))\n"
      "            )\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, MakeSqrtTable) {
  std::string func =
      "sll r0, r0, 0\n"
      "L149:\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L190\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    addiu gp, r0, 0\n"
      "    beq r0, r0, L151\n"
      "    sll r0, r0, 0\n"
      "L150:\n"
      //"    lwc1 f0, L232(fp)\n"
      "    mtc1 f0, r0\n"
      "    mtc1 f1, gp\n"
      "    cvt.s.w f1, f1\n"
      "    mul.s f0, f0, f1\n"
      "    sqrt.s f0, f0\n"
      //"    lwc1 f1, L233(fp)\n"
      "    mtc1 f1, r0\n"
      "    add.s f0, f1, f0\n"
      "    cvt.w.s f0, f0\n"
      "    mfc1 a2, f0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L189\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    or v1, v0, r0\n"
      "    daddiu gp, gp, 1\n"
      "L151:\n"
      "    slti v1, gp, 256\n"
      "    bne v1, r0, L150\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L188\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0";
  std::string type = "(function none)";
  std::string expected =
      "(begin\n"
      "  (format #t \"static int sqrt_table[256] =~%{~%\")\n"
      "  (dotimes (gp-0 256)\n"
      "   (let* ((f0-2 (sqrtf (* 0.0 (the float gp-0))))\n"
      "          (a2-0 (the int (+ 0.0 f0-2)))\n"
      "          )\n"
      "    (format #t \"~D,~%\" a2-0)\n"
      "    )\n"
      "   )\n"
      "  (format #t \"};~%\")\n"
      "  0\n"
      "  (none)\n"
      "  )";
  test_with_expr(
      func, type, expected, false, "",
      {{"L190", "static int sqrt_table[256] =~%{~%"}, {"L189", "~D,~%"}, {"L188", "};~%"}});
}

TEST_F(FormRegressionTestJak1, Method2Vec4s) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    por gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L344\n"
      "    sllv a2, gp, r0\n"
      "    dsra32 a3, gp, 0\n"
      "    pcpyud v1, gp, r0\n"
      "    sllv t0, v1, r0\n"
      "    pcpyud v1, gp, r0\n"
      "    dsra32 t1, v1, 0\n"
      "    por t2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    por v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function vec4s vec4s)";
  std::string expected =
      "(begin\n"
      "  (format\n"
      "   #t\n"
      "   \"#<vector ~F ~F ~F ~F @ #x~X>\"\n"
      "   (-> arg0 x)\n"
      "   (-> arg0 y)\n"
      "   (-> arg0 z)\n"
      "   (-> arg0 w)\n"
      "   arg0\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L344", "#<vector ~F ~F ~F ~F @ #x~X>"}});
}

TEST_F(FormRegressionTestJak1, SoundNameEqual) {
  std::string func =
      "sll r0, r0, 0\n"
      "    dsubu v1, a0, a1\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, v1\n"
      "    beql s7, a2, L125\n"
      "    or v0, a2, r0\n"
      "    pcpyud v1, a0, r0\n"
      "    pcpyud a0, a1, r0\n"
      "    dsubu v1, v1, a0\n"
      "    daddiu v0, s7, 8\n"
      "    movn v0, s7, v1\n"
      "L125:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function sound-name sound-name symbol)";
  std::string expected =
      "(and (= (the-as uint arg0) (the-as uint arg1)) (= (-> arg0 hi) (-> arg1 hi)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, DebugMenuFuncDecode) {
  std::string func =
      "sll r0, r0, 0\n"
      "    dsll32 v1, a0, 29\n"
      "    beql v1, r0, L203\n"
      "    lw v1, binteger(s7)\n"

      "    bgtzl v1, L203\n"
      "    lw v1, pair(s7)\n"

      "    lwu v1, -4(a0)\n"

      "L203:\n"
      "    lw a1, symbol(s7)\n"
      "    dsubu a1, v1, a1\n"
      "    daddiu a2, s7, 8\n"
      "    movn a2, s7, a1\n"
      "    bnel s7, a2, L204\n"
      "    or a1, a2, r0\n"

      "    lw a1, type(s7)\n"
      "    dsubu a2, v1, a1\n"
      "    daddiu a1, s7, 8\n"
      "    movn a1, s7, a2\n"
      "L204:\n"
      "    beq s7, a1, L205\n"
      "    sll r0, r0, 0\n"

      "    lw v0, 0(a0)\n"
      "    beq r0, r0, L207\n"
      "    sll r0, r0, 0\n"

      "L205:\n"
      "    lw a1, function(s7)\n"
      "    bne v1, a1, L206\n"
      "    sll r0, r0, 0\n"

      "    or v0, a0, r0\n"
      "    beq r0, r0, L207\n"
      "    sll r0, r0, 0\n"

      "L206:\n"
      "    lw v0, nothing(s7)\n"

      "L207:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object function)";
  std::string expected =
      "(let ((v1-1 (rtype-of arg0)))\n"
      "  (the-as function (cond\n"
      "                    ((or (= v1-1 symbol) (= v1-1 type))\n"
      "                     (-> (the-as symbol arg0) value)\n"
      "                     )\n"
      "                    ((= v1-1 function)\n"
      "                     arg0\n"
      "                     )\n"
      "                    (else\n"
      "                     nothing\n"
      "                     )\n"
      "                    )\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {}, "[[13, \"a0\", \"symbol\"]]");
}

TEST_F(FormRegressionTestJak1, MatrixNewInlineProp) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -112\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 80(sp)\n"
      "    sq gp, 96(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a2, r0\n"
      "    lw t9, matrix-rotate-y!(s7)\n"
      "    or a0, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, matrix-rotate-x!(s7)\n"
      "    daddiu a0, sp, 16\n"
      "    sq r0, 0(a0)\n"
      "    sq r0, 16(a0)\n"
      "    sq r0, 32(a0)\n"
      "    sq r0, 48(a0)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or a1, v0, r0\n"
      "    lw t9, matrix*!(s7)\n"
      "    or a0, gp, r0\n"
      "    or a2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112\n"

      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n";
  std::string type = "(function matrix float float matrix)";
  std::string expected =
      "(begin\n"
      "  (matrix-rotate-y! arg0 arg1)\n"
      "  (let ((a1-2 (matrix-rotate-x! (new-stack-matrix0) arg2)))\n"
      "   (matrix*! arg0 a1-2 arg0)\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_structures(func, type, expected, R"([[16, "matrix"]])");
}

TEST_F(FormRegressionTestJak1, VectorNewInlineProp) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"
      "    or gp, a0, r0\n"
      "    daddiu s5, sp, 16\n"
      "    sq r0, 0(s5)\n"
      "    or v1, s5, r0\n"
      "    lwc1 f0, 0(a1)\n"
      "    swc1 f0, 0(v1)\n"
      "    lwc1 f0, 4(a1)\n"
      "    swc1 f0, 4(v1)\n"
      "    lwc1 f0, 8(a1)\n"
      "    swc1 f0, 8(v1)\n"
      "    mtc1 f0, r0\n"
      "    swc1 f0, 12(v1)\n"
      "    lw t9, vector-matrix*!(s7)\n"
      "    or a0, s5, r0\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lwc1 f0, 0(s5)\n"
      "    swc1 f0, 0(gp)\n"
      "    lwc1 f0, 4(s5)\n"
      "    swc1 f0, 4(gp)\n"
      "    lwc1 f0, 8(s5)\n"
      "    swc1 f0, 8(gp)\n"
      "    or v1, gp, r0\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64\n"

      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n";
  std::string type = "(function vector3s vector3s matrix vector3s)";
  std::string expected =
      "(begin\n"
      "  (let ((s5-0 (new-stack-vector0)))\n"
      "   (set-vector! s5-0 (-> arg1 x) (-> arg1 y) (-> arg1 z) 0.0)\n"
      "   (vector-matrix*! s5-0 s5-0 arg2)\n"
      "   (set! (-> arg0 x) (-> s5-0 x))\n"
      "   (set! (-> arg0 y) (-> s5-0 y))\n"
      "   (set! (-> arg0 z) (-> s5-0 z))\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_stack_structures(func, type, expected, R"([[16, "vector"]])");
}

TEST_F(FormRegressionTestJak1, Method23Trsqv) {
  std::string func =
      "sll r0, r0, 0\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"

      "    lw t9, vector-y-angle(s7)\n"
      "    daddiu v1, sp, 16\n"
      "    daddiu a0, a0, 12\n"
      "    lqc2 vf4, 0(a1)\n"
      "    lqc2 vf5, 0(a0)\n"
      "    vmove.w vf6, vf0\n"
      "    vsub.xyz vf6, vf4, vf5\n"
      "    sqc2 vf6, 0(v1)\n"
      "    or a0, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32\n";
  std::string type = "(function trsqv vector float)";
  std::string expected =
      "(vector-y-angle (vector-! (new 'stack-no-clear 'vector) arg1 (-> arg0 trans)))";
  test_with_stack_structures(func, type, expected, R"([[16, "vector"]])");
}

TEST_F(FormRegressionTestJak1, VectorLineDistance) {
  std::string func =
      "sll r0, r0, 0\n"
      "\n"
      "    daddiu sp, sp, -128\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s4, 80(sp)\n"
      "    sq s5, 96(sp)\n"
      "    sq gp, 112(sp)\n"
      "    or s5, a0, r0\n"
      "    or s4, a1, r0\n"
      "    lw t9, vector-normalize!(s7)\n"
      "    daddiu a0, sp, 16\n"
      "    sq r0, 0(a0)\n"
      "    or v1, a2, r0\n"
      "    or a1, s4, r0\n"
      "    lqc2 vf4, 0(v1)\n"
      "    lqc2 vf5, 0(a1)\n"
      "    vmove.w vf6, vf0\n"
      "    vsub.xyz vf6, vf4, vf5\n"
      "    sqc2 vf6, 0(a0)\n"
      //"    lw a1, L125(fp)\n"
      "    or a1, r0, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or a1, v0, r0\n"
      "    daddiu gp, sp, 32\n"
      "    sq r0, 0(gp)\n"
      "    lqc2 vf4, 0(s5)\n"
      "    lqc2 vf5, 0(s4)\n"
      "    vmove.w vf6, vf0\n"
      "    vsub.xyz vf6, vf4, vf5\n"
      "    sqc2 vf6, 0(gp)\n"
      "    or a0, a1, r0\n"
      "    or v1, gp, r0\n"
      "    lwc1 f0, 0(a0)\n"
      "    lwc1 f1, 4(a0)\n"
      "    lwc1 f2, 8(a0)\n"
      "    lwc1 f3, 0(v1)\n"
      "    lwc1 f4, 4(v1)\n"
      "    lwc1 f5, 8(v1)\n"
      "    mula.s f0, f3\n"
      "    madda.s f1, f4\n"
      "    madd.s f0, f2, f5\n"
      "    mfc1 v1, f0\n"
      "    mtc1 f0, v1\n"
      "    lw t9, vector-float*!(s7)\n"
      "    daddiu a0, sp, 48\n"
      "    sq r0, 0(a0)\n"
      "    mfc1 a2, f0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    lw t9, vector-length(s7)\n"
      "    daddiu a0, sp, 64\n"
      "    sq r0, 0(a0)\n"
      "    lqc2 vf4, 0(gp)\n"
      "    lqc2 vf5, 0(v1)\n"
      "    vmove.w vf6, vf0\n"
      "    vsub.xyz vf6, vf4, vf5\n"
      "    sqc2 vf6, 0(a0)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 112(sp)\n"
      "    lq s5, 96(sp)\n"
      "    lq s4, 80(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 128\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0\n";
  std::string type = "(function vector vector vector float)";
  std::string expected =
      "(let*\n"
      "  ((a1-3\n"
      "    (vector-normalize!\n"
      "     (vector-! (new-stack-vector0) arg2 arg1)\n"
      "     (the-as float 0)\n"
      "     )\n"
      "    )\n"
      "   (gp-1 (vector-! (new-stack-vector0) arg0 arg1))\n"
      "   (f0-1 (vector-dot a1-3 gp-1))\n"
      "   (v1-3 (vector-float*! (new-stack-vector0) a1-3 f0-1))\n"
      "   )\n"
      "  (vector-length (vector-! (new-stack-vector0) gp-1 v1-3))\n"
      "  )";
  test_with_stack_structures(func, type, expected,
                             R"([[16, "vector"], [32, "vector"], [48, "vector"], [64, "vector"]])");
}
