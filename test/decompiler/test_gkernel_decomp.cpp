#include "gtest/gtest.h"
#include "FormRegressionTest.h"

using namespace decompiler;

TEST_F(FormRegressionTest, ExprMethod7Object) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n";
  std::string type = "(function object int object)";
  std::string expected = "arg0";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprLoadPackage) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L278:\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    lw t9, nmember(s7)\n"
      "    or a0, gp, r0\n"
      "    lw a1, *kernel-packages*(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    bne s7, v0, L279\n"
      "    or v0, s7, r0\n"

      "    lw t9, dgo-load(s7)\n"
      "    or a0, gp, r0\n"
      "    addiu a2, r0, 15\n"
      "    lui a3, 32\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw v1, pair(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    daddiu a0, s7, global\n"
      "    lw a1, pair(s7)\n"
      "    lw a3, *kernel-packages*(s7)\n"
      "    or a2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sw v0, *kernel-packages*(s7)\n"
      "L279:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48\n";
  std::string type = "(function string kheap pair)";
  std::string expected =
      "(when\n"
      "  (not (nmember arg0 *kernel-packages*))\n"
      "  (dgo-load arg0 arg1 15 2097152)\n"
      "  (set! v0-1 (cons arg0 *kernel-packages*))\n"
      "  (set! *kernel-packages* v0-1)\n"
      "  v0-1\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTest, ExprUnloadPackage) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    lw t9, nmember(s7)\n"
      "    lw a1, *kernel-packages*(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq s7, v1, L277\n"
      "    or a0, s7, r0\n"

      "    lw t9, delete!(s7)\n"
      "    lw a0, -2(v1)\n"
      "    lw a1, *kernel-packages*(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sw v0, *kernel-packages*(s7)\n"
      "    or v1, v0, r0\n"
      "L277:\n"
      "    lw v0, *kernel-packages*(s7)\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16\n";
  std::string type = "(function string pair)";
  std::string expected =
      "(begin\n"
      "  (set! v1-0 (nmember arg0 *kernel-packages*))\n"
      "  (if v1-0 (set! *kernel-packages* (delete! (car v1-0) *kernel-packages*)))\n"
      "  *kernel-packages*\n"
      "  )";
  test_with_expr(func, type, expected, true);
}

TEST_F(FormRegressionTest, ExprMethod1Thread) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L274:\n"
      "    lwu v1, 4(a0)\n"
      "    lwu v1, 40(v1)\n"
      "    bne a0, v1, L275\n"
      "    or v1, s7, r0\n"

      "    lw r0, 2(r0)\n"
      "    addiu v1, r0, 0\n"

      "L275:\n"
      "    lwu v0, 8(a0)\n"
      "    lwu v1, 4(a0)\n"
      "    sw v0, 44(v1)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function thread none)";
  std::string expected =
      "(begin\n"
      "  (when (= arg0 (-> arg0 process main-thread)) (break!) (set! v1-3 0))\n"
      "  (set! (-> arg0 process top-thread) (-> arg0 previous))\n"
      "  )";
  test_with_expr(func, type, expected, false);
}

TEST_F(FormRegressionTest, ExprMethod2Thread) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L273:\n"
      "    daddiu sp, sp, -32\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq gp, 16(sp)\n"

      "    or gp, a0, r0\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L343\n"
      "    lwu a2, -4(gp)\n"
      "    lwu a3, 0(gp)\n"
      "    lwu v1, 4(gp)\n"
      "    lwu t0, 0(v1)\n"
      "    lwu t1, 20(gp)\n"
      "    or t2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function thread thread)";
  std::string expected =
      "(begin\n"
      "  (format\n"
      "   (quote #t)\n"
      "   \"#<~A ~S of ~S pc: #x~X @ #x~X>\"\n"
      "   (-> arg0 type)\n"
      "   (-> arg0 name)\n"
      "   (-> arg0 process name)\n"
      "   (-> arg0 pc)\n"
      "   arg0\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L343", "#<~A ~S of ~S pc: #x~X @ #x~X>"}});
}

TEST_F(FormRegressionTest, ExprMethod9Thread) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L268:\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"

      "    lwu a2, 4(a0)\n"
      "    lwu v1, 40(a2)\n"
      "    beq a0, v1, L269\n"
      "    sll r0, r0, 0\n"

      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L342\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L272\n"
      "    sll r0, r0, 0\n"

      "L269:\n"
      "    lw v1, 32(a0)\n"
      "    bne v1, a1, L270\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    beq r0, r0, L272\n"
      "    sll r0, r0, 0\n"

      "L270:\n"
      "    lw v1, 32(a0)\n"
      "    daddiu v1, v1, -4\n"
      "    lwu a3, -4(a0)\n"
      "    lhu a3, 8(a3)\n"
      "    daddu v1, v1, a3\n"
      "    daddu v1, v1, a0\n"
      "    lwu a3, 84(a2)\n"
      "    bne a3, v1, L271\n"
      "    sll r0, r0, 0\n"

      "    daddiu v1, a1, -4\n"
      "    lwu a3, -4(a0)\n"
      "    lhu a3, 8(a3)\n"
      "    daddu v1, v1, a3\n"
      "    daddu v1, v1, a0\n"
      "    sw v1, 84(a2)\n"
      "    sw a1, 32(a0)\n"
      "    or v1, a1, r0\n"
      "    beq r0, r0, L272\n"
      "    sll r0, r0, 0\n"

      "L271:\n"
      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L341\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"

      "L272:\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function thread int none)";
  std::string expected =
      "(begin\n"
      "  (set! a2-0 (-> arg0 process))\n"
      "  (cond\n"
      "   ((!= arg0 (-> a2-0 main-thread)) (format 0 \"1 ~A ~%\" a2-0))\n"
      "   ((= (-> arg0 stack-size) arg1))\n"
      "   ((=\n"
      "     (-> a2-0 heap-cur)\n"
      "     (+\n"
      "      (+ (+ (-> arg0 stack-size) -4) (the-as int (-> arg0 type size)))\n"
      "      (the-as int arg0)\n"
      "      )\n"
      "     )\n"
      "    (set!\n"
      "     (-> a2-0 heap-cur)\n"
      "     (+ (+ (+ arg1 -4) (the-as int (-> arg0 type size))) (the-as int arg0))\n"
      "     )\n"
      "    (set! (-> arg0 stack-size) arg1)\n"
      "    )\n"
      "   (else (format 0 \"2 ~A ~%\" a2-0))\n"
      "   )\n"
      "  (set! v0-2 0)\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L342", "1 ~A ~%"}, {"L341", "2 ~A ~%"}});
}
