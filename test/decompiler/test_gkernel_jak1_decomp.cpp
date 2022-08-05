#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

TEST_F(FormRegressionTestJak1, ExprMethod7Object) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n";
  std::string type = "(function object int object)";
  std::string expected = "arg0";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprLoadPackage) {
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
      "  (dgo-load arg0 arg1 (link-flag output-load-msg output-load-true-msg execute-login "
      "print-login) #x200000)\n"
      "  (let\n"
      "   ((v0-1 (cons arg0 *kernel-packages*)))\n"
      "   (set! *kernel-packages* v0-1)\n"
      "   v0-1\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprUnloadPackage) {
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
      "  (let\n"
      "   ((v1-0 (nmember arg0 *kernel-packages*)))\n"
      "   (if v1-0 (set! *kernel-packages* (delete! (car v1-0) *kernel-packages*)))\n"
      "   )\n"
      "  *kernel-packages*\n"
      "  )";
  test_with_expr(func, type, expected, true);
}

TEST_F(FormRegressionTestJak1, ExprMethod1Thread) {
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
      "  (when (= arg0 (-> arg0 process main-thread))\n"
      "   (break!)\n"
      "   0\n"
      "   )\n"
      "  (set! (-> arg0 process top-thread) (-> arg0 previous))\n"
      "  (none)\n"
      "  )";
  test_with_expr(func, type, expected, false);
}

TEST_F(FormRegressionTestJak1, ExprMethod2Thread) {
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
      "   #t\n"
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

TEST_F(FormRegressionTestJak1, ExprMethod9Thread) {
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
      "  (let ((a2-0 (-> arg0 process)))\n"
      "   (cond\n"
      "    ((!= arg0 (-> a2-0 main-thread))\n"
      "     (format 0 \"1 ~A ~%\" a2-0)\n"
      "     )\n"
      "    ((= (-> arg0 stack-size) arg1)\n"
      "     )\n"
      "    ((=\n"
      "      (-> a2-0 heap-cur)\n"
      "      (+ (+ (-> arg0 stack-size) -4 (-> arg0 type size)) (the-as int arg0))\n"
      "      )\n"
      "     (set!\n"
      "      (-> a2-0 heap-cur)\n"
      "      (the-as pointer (+ (+ arg1 -4 (-> arg0 type size)) (the-as int arg0)))\n"
      "      )\n"
      "     (set! (-> arg0 stack-size) arg1)\n"
      "     )\n"
      "    (else\n"
      "     (format 0 \"2 ~A ~%\" a2-0)\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  0\n"
      "  (none)\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L342", "1 ~A ~%"}, {"L341", "2 ~A ~%"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod0Thread) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lwu v1, 44(a2)\n"
      "    beq s7, v1, L266\n"
      "    sll r0, r0, 0\n"

      "    daddiu v0, t1, -7164\n"
      "    beq r0, r0, L267\n"
      "    sll r0, r0, 0\n"

      "L266:\n"
      "    addiu v1, r0, -16\n"
      "    lwu a0, 84(a2)\n"
      "    daddiu a0, a0, 15\n"
      "    and v1, v1, a0\n"
      "    lhu a0, 8(a1)\n"
      "    daddu a0, v1, a0\n"
      "    daddu a0, a0, t0\n"
      "    sw a0, 84(a2)\n"
      "    daddiu v0, v1, 4\n"

      "L267:\n"
      "    sw a1, -4(v0)\n"
      "    sw a3, 0(v0)\n"
      "    sw a2, 4(v0)\n"
      "    sw t1, 24(v0)\n"
      "    sw t1, 28(v0)\n"
      "    lwu v1, 44(a2)\n"
      "    sw v1, 8(v0)\n"
      "    sw v0, 44(a2)\n"
      "    lwu v1, -4(v0)\n"
      "    lwu v1, 56(v1)\n"
      "    sw v1, 12(v0)\n"
      "    lwu v1, -4(v0)\n"
      "    lwu v1, 60(v1)\n"
      "    sw v1, 16(v0)\n"
      "    sw t0, 32(v0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function symbol type process symbol int pointer cpu-thread)";
  std::string expected =
      "(let ((obj (the-as cpu-thread (cond\n"
      "                              ((-> arg2 top-thread)\n"
      "                               (the-as cpu-thread (&+ arg5 -7164))\n"
      "                               )\n"
      "                              (else\n"
      "                               (let\n"
      "                                ((v1-2 (logand -16 (&+ (-> arg2 heap-cur) 15))))\n"
      "                                (set!\n"
      "                                 (-> arg2 heap-cur)\n"
      "                                 (&+ (&+ v1-2 (-> arg1 size)) arg4)\n"
      "                                 )\n"
      "                                (the-as cpu-thread (&+ v1-2 4))\n"
      "                                )\n"
      "                               )\n"
      "                              )\n"
      "           )\n"
      "      )\n"
      "     )\n"
      "  (set! (-> obj type) arg1)\n"
      "  (set! (-> obj name) arg3)\n"
      "  (set! (-> obj process) arg2)\n"
      "  (set! (-> obj sp) arg5)\n"
      "  (set! (-> obj stack-top) arg5)\n"
      "  (set! (-> obj previous) (-> arg2 top-thread))\n"
      "  (set! (-> arg2 top-thread) obj)\n"
      "  (set! (-> obj suspend-hook) (method-of-object obj thread-suspend))\n"
      "  (set! (-> obj resume-hook) (method-of-object obj thread-resume))\n"
      "  (set! (-> obj stack-size) arg4)\n"
      "  (the-as cpu-thread obj)\n"
      "  )";
  test_with_expr(func, type, expected, false, "cpu-thread", {},
                 "[[[13, 28], \"v0\", \"cpu-thread\"]]",
                 "{\"vars\":{\"v0-0\":[\"obj\", \"cpu-thread\"]}}");
}

TEST_F(FormRegressionTestJak1, ExprMethod5CpuThread) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L264:\n"
      "    lwu v1, -4(a0)\n"
      "    lhu v1, 8(v1)\n"
      "    lw a0, 32(a0)\n"
      "    daddu v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function cpu-thread int)";
  std::string expected = "(the-as int (+ (-> arg0 type size) (-> arg0 stack-size)))";
  test_with_expr(func, type, expected, false);
}

TEST_F(FormRegressionTestJak1, RemoveExit) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L262:\n"
      "    lwu v1, 88(s6)\n"
      "    beq s7, v1, L263\n"
      "    or v0, s7, r0\n"

      "    lwu v1, 88(s6)\n"
      "    lwu v0, 4(v1)\n"
      "    sw v0, 88(s6)\n"

      "L263:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function stack-frame)";
  std::string expected =
      "(when (-> pp stack-frame-top)\n"
      "  (let ((v0-0 (-> pp stack-frame-top next)))\n"
      "   (set! (-> pp stack-frame-top) v0-0)\n"
      "   v0-0\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, false);
}

TEST_F(FormRegressionTestJak1, RemoveMethod0ProcessTree) {
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
      "    or a1, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sw gp, 0(v0)\n"
      "    addiu v1, r0, 256\n"
      "    sw v1, 4(v0)\n"
      "    sw s7, 8(v0)\n"
      "    sw s7, 12(v0)\n"
      "    sw s7, 16(v0)\n"
      "    sw v0, 24(v0)\n"
      "    daddiu v1, v0, 24\n"
      "    sw v1, 20(v0)\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function symbol type basic process-tree)";
  std::string expected =
      "(let\n"
      "  ((v0-0 (object-new arg0 arg1 (the-as int (-> arg1 size)))))\n"
      "  (set! (-> v0-0 name) arg2)\n"
      "  (set! (-> v0-0 mask) (process-mask process-tree))\n"
      "  (set! (-> v0-0 parent) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> v0-0 brother) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> v0-0 child) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> v0-0 self) v0-0)\n"
      "  (set! (-> v0-0 ppointer) (the-as (pointer process) (&-> v0-0 self)))\n"
      "  v0-0\n"
      "  )";
  test_with_expr(func, type, expected, false, "process-tree");
}

TEST_F(FormRegressionTestJak1, RemoveMethod3ProcessTree) {
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
      "    daddiu a1, fp, L340\n"
      "    or a2, gp, r0\n"
      "    lwu a3, -4(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L339\n"
      "    lwu a2, 0(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L338\n"
      "    lwu a2, 4(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L337\n"
      "    lwu v1, 8(gp)\n"
      "    beq s7, v1, L258\n"
      "    or a2, s7, r0\n"

      "    lwu v1, 0(v1)\n"
      "    lwu a2, 24(v1)\n"

      "L258:\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L336\n"
      "    lwu v1, 12(gp)\n"
      "    beq s7, v1, L259\n"
      "    or a2, s7, r0\n"

      "    lwu v1, 0(v1)\n"
      "    lwu a2, 24(v1)\n"

      "L259:\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L335\n"
      "    lwu v1, 16(gp)\n"
      "    beq s7, v1, L260\n"
      "    or a2, s7, r0\n"

      "    lwu v1, 0(v1)\n"
      "    lwu a2, 24(v1)\n"

      "L260:\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function process-tree process-tree)";
  // gross, but expected. see https://github.com/water111/jak-project/issues/254
  std::string expected =
      "(begin\n"
      "  (format #t \"[~8x] ~A~%\" arg0 (-> arg0 type))\n"
      "  (format #t \"~Tname: ~S~%\" (-> arg0 name))\n"
      "  (format #t \"~Tmask: #x~X~%\" (-> arg0 mask))\n"
      "  (format #t \"~Tparent: ~A~%\" (ppointer->process (-> arg0 parent)))\n"
      "  (format #t \"~Tbrother: ~A~%\" (ppointer->process (-> arg0 brother)))\n"
      "  (format #t \"~Tchild: ~A~%\" (ppointer->process (-> arg0 child)))\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "process-tree",
                 {{"L340", "[~8x] ~A~%"},
                  {"L339", "~Tname: ~S~%"},
                  {"L338", "~Tmask: #x~X~%"},
                  {"L337", "~Tparent: ~A~%"},
                  {"L336", "~Tbrother: ~A~%"},
                  {"L335", "~Tchild: ~A~%"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod0Process) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L254:\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or s5, a2, r0\n"
      "    or gp, a3, r0\n"
      "    lw v1, symbol(s7)\n"
      "    lwu a2, -4(a0)\n"
      "    bne a2, v1, L255\n"
      "    sll r0, r0, 0\n"

      "    lw v1, object(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    lw v1, process(s7)\n"
      "    lhu v1, 8(v1)\n"
      "    daddu a2, v1, gp\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq r0, r0, L256\n"
      "    sll r0, r0, 0\n"

      "L255:\n"
      "    daddiu v0, a0, 4\n"

      "L256:\n"
      "    sw s5, 0(v0)\n"
      "    daddiu v1, s7, dead\n"
      "    sw v1, 32(v0)\n"
      "    sw r0, 36(v0)\n"
      "    sw s7, 28(v0)\n"
      "    sw gp, 68(v0)\n"
      "    sw s7, 44(v0)\n"
      "    sw s7, 40(v0)\n"
      "    daddiu v1, v0, 108\n"
      "    sw v1, 84(v0)\n"
      "    sw v1, 76(v0)\n"
      "    lw v1, 68(v0)\n"
      "    daddiu v1, v1, 108\n"
      "    daddu v1, v1, v0\n"
      "    sw v1, 80(v0)\n"
      "    lwu v1, 80(v0)\n"
      "    sw v1, 88(v0)\n"
      "    sw s7, 88(v0)\n"
      "    sw s7, 52(v0)\n"
      "    sw s7, 72(v0)\n"
      "    sw s7, 48(v0)\n"
      "    sw s7, 56(v0)\n"
      "    sw s7, 60(v0)\n"
      "    sw s7, 64(v0)\n"
      "    sw s7, 8(v0)\n"
      "    sw s7, 12(v0)\n"
      "    sw s7, 16(v0)\n"
      "    sw v0, 24(v0)\n"
      "    daddiu v1, v0, 24\n"
      "    sw v1, 20(v0)\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function symbol type basic int process)";
  std::string expected =
      "(let ((v0-0 (if (= (-> arg0 type) symbol)\n"
      "            (object-new arg0 arg1 (the-as int (+ (-> process size) arg3)))\n"
      "            (+ (the-as int arg0) 4)\n"
      "            )\n"
      "      )\n"
      "     )\n"
      "  (set! (-> (the-as process v0-0) name) arg2)\n"
      "  (set! (-> (the-as process v0-0) status) 'dead)\n"
      "  (set! (-> (the-as process v0-0) pid) 0)\n"
      "  (set! (-> (the-as process v0-0) pool) #f)\n"
      "  (set! (-> (the-as process v0-0) allocated-length) arg3)\n"
      "  (set! (-> (the-as process v0-0) top-thread) #f)\n"
      "  (set! (-> (the-as process v0-0) main-thread) #f)\n"
      "  (let ((v1-5 (-> (the-as process v0-0) stack)))\n"
      "   (set! (-> (the-as process v0-0) heap-cur) v1-5)\n"
      "   (set! (-> (the-as process v0-0) heap-base) v1-5)\n"
      "   )\n"
      "  (set!\n"
      "   (-> (the-as process v0-0) heap-top)\n"
      "   (&-> (the-as process v0-0) stack (-> (the-as process v0-0) allocated-length))\n"
      "   )\n"
      "  (set!\n"
      "   (-> (the-as process v0-0) stack-frame-top)\n"
      "   (the-as stack-frame (-> (the-as process v0-0) heap-top))\n"
      "   )\n"
      "  (set! (-> (the-as process v0-0) stack-frame-top) #f)\n"
      "  (set! (-> (the-as process v0-0) state) #f)\n"
      "  (set! (-> (the-as process v0-0) next-state) #f)\n"
      "  (set! (-> (the-as process v0-0) entity) #f)\n"
      "  (set! (-> (the-as process v0-0) trans-hook) #f)\n"
      "  (set! (-> (the-as process v0-0) post-hook) #f)\n"
      "  (set! (-> (the-as process v0-0) event-hook) #f)\n"
      "  (set! (-> (the-as process v0-0) parent) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> (the-as process v0-0) brother) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> (the-as process v0-0) child) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> (the-as process v0-0) self) (the-as process v0-0))\n"
      "  (set!\n"
      "   (-> (the-as process v0-0) ppointer)\n"
      "   (the-as (pointer process) (&-> (the-as process v0-0) self))\n"
      "   )\n"
      "  (the-as process v0-0)\n"
      "  )";
  test_with_expr(func, type, expected, false, "process", {},
                 "[\t\t[12, \"a0\", \"int\"],\n"
                 "\t\t[[13, 43], \"v0\", \"process\"]]");
}

TEST_F(FormRegressionTestJak1, ExprInspectProcessHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L251:\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or gp, a0, r0\n"
      "    lwu v1, 76(gp)\n"
      "    daddiu s5, v1, 4\n"
      "    beq r0, r0, L253\n"
      "    sll r0, r0, 0\n"

      "L252:\n"
      "    or a0, s5, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 28(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    addiu s4, r0, -16\n"
      "    or a0, s5, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 36(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    daddiu v1, v1, 15\n"
      "    and v1, s4, v1\n"
      "    daddu s5, s5, v1\n"

      "L253:\n"
      "    lwu v1, 84(gp)\n"
      "    slt v1, s5, v1\n"
      "    bne v1, r0, L252\n"
      "    sll r0, r0, 0\n"

      "    or v0, s7, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function process symbol)";
  std::string expected =
      "(begin\n"
      "  (let ((obj (&+ (-> arg0 heap-base) 4)))\n"
      "   (while (< (the-as int obj) (the-as int (-> arg0 heap-cur)))\n"
      "    (inspect (the-as basic obj))\n"
      "    (&+! obj (logand -16 (+ (asize-of (the-as basic obj)) 15)))\n"
      "    )\n"
      "   )\n"
      "  #f\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 "[\t\t[[4,11], \"s5\", \"basic\"],\n"
                 "\t\t[[17,20], \"s5\", \"pointer\"]]",
                 "{\"vars\":{\"s5-0\":[\"obj\", \"pointer\"]}}");
}

// note: skipped method 3 process

TEST_F(FormRegressionTestJak1, ExprMethod5Process) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lw v1, process(s7)\n"
      "    lhu v1, 8(v1)\n"
      "    lw a0, 68(a0)\n"
      "    daddu v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function process int)";
  std::string expected = "(the-as int (+ (-> process size) (-> arg0 allocated-length)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod2Process) {
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
      "    daddiu a1, fp, L317\n"
      "    lwu a2, -4(gp)\n"
      "    lwu a3, 0(gp)\n"
      "    lwu t0, 32(gp)\n"
      "    lwu v1, 52(gp)\n"
      "    beq s7, v1, L245\n"
      "    or t1, s7, r0\n"

      "    lwu v1, 52(gp)\n"
      "    lwu t1, 0(v1)\n"

      "L245:\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L316\n"
      "    lwu v1, 44(gp)\n"
      "    lwu v1, 28(v1)\n"
      "    lwu a2, 44(gp)\n"
      "    lwu a2, 24(a2)\n"
      "    dsubu a2, v1, a2\n"
      "    lwu v1, 40(gp)\n"
      "    lw a3, 32(v1)\n"
      "    lw v1, 68(gp)\n"
      "    lwu t0, 80(gp)\n"
      "    lwu t1, 84(gp)\n"
      "    dsubu t0, t0, t1\n"
      "    dsubu t0, v1, t0\n"
      "    lw t1, 68(gp)\n"
      "    or t2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 32";
  std::string type = "(function process process)";
  std::string expected =
      "(begin\n"
      "  (format\n"
      "   #t\n"
      "   \"#<~A ~S ~A :state ~S \"\n"
      "   (-> arg0 type)\n"
      "   (-> arg0 name)\n"
      "   (-> arg0 status)\n"
      "   (if (-> arg0 state) (-> arg0 state name))\n"
      "   )\n"
      "  (format\n"
      "   #t\n"
      "   \":stack ~D/~D :heap ~D/~D @ #x~X>\"\n"
      "   (&- (-> arg0 top-thread stack-top) (the-as uint (-> arg0 top-thread sp)))\n"
      "   (-> arg0 main-thread stack-size)\n"
      "   (-\n"
      "    (-> arg0 allocated-length)\n"
      "    (&- (-> arg0 heap-top) (the-as uint (-> arg0 heap-cur)))\n"
      "    )\n"
      "   (-> arg0 allocated-length)\n"
      "   arg0\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {{"L317", "#<~A ~S ~A :state ~S "}, {"L316", ":stack ~D/~D :heap ~D/~D @ #x~X>"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod0DeadPool) {
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
      "    or s5, a2, r0\n"
      "    or s4, a3, r0\n"
      "    or s2, t0, r0\n"
      "    lw v1, object(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    or a0, gp, r0\n"
      "    or v1, a1, r0\n"
      "    lhu a2, 8(a1)\n"
      "    or a1, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or s3, v0, r0\n"
      "    sw s2, 0(s3)\n"
      "    addiu v1, r0, 256\n"
      "    sw v1, 4(s3)\n"
      "    sw s7, 8(s3)\n"
      "    sw s7, 12(s3)\n"
      "    sw s7, 16(s3)\n"
      "    sw s3, 24(s3)\n"
      "    daddiu v1, s3, 24\n"
      "    sw v1, 20(s3)\n"
      "    addiu s2, r0, 0\n"
      "    beq r0, r0, L233\n"
      "    sll r0, r0, 0\n"

      "L230:\n"
      "    lwu s1, 16(s3)\n"
      "    lw v1, process(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    or a0, gp, r0\n"
      "    lw a1, process(s7)\n"
      "    daddiu a2, s7, dead\n"
      "    or a3, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    or a0, v1, r0\n"
      "    beq s7, a0, L231\n"
      "    or a1, s7, r0\n"

      "    lwu a1, 20(a0)\n"

      "L231:\n"
      "    sw a1, 16(s3)\n"
      "    or a0, s3, r0\n"
      "    beq s7, a0, L232\n"
      "    or a1, s7, r0\n"

      "    lwu a1, 20(a0)\n"

      "L232:\n"
      "    sw a1, 8(v1)\n"
      "    sw s3, 28(v1)\n"
      "    sw s1, 12(v1)\n"
      "    daddiu s2, s2, 1\n"

      "L233:\n"
      "    slt v1, s2, s5\n"
      "    bne v1, r0, L230\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    or v0, s3, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    lq s4, 64(sp)\n"
      "    lq s3, 48(sp)\n"
      "    lq s2, 32(sp)\n"
      "    lq s1, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function symbol type int int basic dead-pool)";
  std::string expected =
      "(let ((s3-0 (object-new arg0 arg1 (the-as int (-> arg1 size)))))\n"
      "  (set! (-> s3-0 name) arg4)\n"
      "  (set! (-> s3-0 mask) (process-mask process-tree))\n"
      "  (set! (-> s3-0 parent) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> s3-0 brother) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> s3-0 child) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> s3-0 self) s3-0)\n"
      "  (set! (-> s3-0 ppointer) (the-as (pointer process) (&-> s3-0 self)))\n"
      "  (dotimes (s2-1 arg2)\n"
      "   (let ((s1-0 (-> s3-0 child))\n"
      "         (v1-5 ((method-of-type process new) arg0 process 'dead arg3))\n"
      "         )\n"
      "    (set! (-> s3-0 child) (process->ppointer v1-5))\n"
      "    (set! (-> v1-5 parent) (process->ppointer (the-as process s3-0)))\n"
      "    (set! (-> v1-5 pool) s3-0)\n"
      "    (set! (-> v1-5 brother) s1-0)\n"
      "    )\n"
      "   )\n"
      "  s3-0\n"
      "  )";
  test_with_expr(func, type, expected, false, "dead-pool");
}

TEST_F(FormRegressionTestJak1, ExprMethod14DeadPool) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or s5, a0, r0\n"
      "    or gp, a1, r0\n"
      "    lwu s4, 16(s5)\n"
      "    bnel s7, s4, L223\n"

      "    or v1, s7, r0\n"

      "    lw v1, *debug-segment*(s7)\n"
      "    beql s7, v1, L223\n"

      "    or v1, v1, r0\n"

      "    lw v1, *debug-dead-pool*(s7)\n"
      "    dsubu a0, s5, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L223:\n"
      "    beq s7, v1, L225\n"
      "    or v1, s7, r0\n"

      "    lw a0, *debug-dead-pool*(s7)\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 72(v1)\n"
      "    or a1, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or s4, v0, r0\n"
      "    beq s7, s4, L225\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L315\n"
      "    or a2, gp, r0\n"
      "    or v1, s4, r0\n"
      "    beq s7, v1, L224\n"
      "    or a3, s7, r0\n"

      "    lwu v1, 0(v1)\n"
      "    lwu a3, 24(v1)\n"

      "L224:\n"
      "    lwu t0, 0(s5)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L225:\n"
      "    beq s7, s4, L226\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 0(s4)\n"
      "    sw gp, -4(v1)\n"
      "    lwu v0, 0(s4)\n"
      "    beq r0, r0, L228\n"
      "    sll r0, r0, 0\n"

      "L226:\n"
      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L314\n"
      "    beq s7, s4, L227\n"
      "    or a3, s7, r0\n"

      "    lwu v1, 0(s4)\n"
      "    lwu a3, 24(v1)\n"

      "L227:\n"
      "    lwu t0, 0(s5)\n"
      "    or a2, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v0, s7, r0\n"

      "L228:\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function dead-pool type int process)";
  // todo - why does one work but not the other??
  std::string expected =
      "(let ((s4-0 (the-as object (-> arg0 child))))\n"
      "  (when\n"
      "   (and\n"
      "    (not (the-as (pointer process-tree) s4-0))\n"
      "    *debug-segment*\n"
      "    (!= arg0 *debug-dead-pool*)\n"
      "    )\n"
      "   (set! s4-0 (get-process *debug-dead-pool* arg1 arg2))\n"
      "   (if (the-as process s4-0)\n"
      "    (format\n"
      "     0\n"
      "     \"WARNING: ~A ~A had to be allocated from the debug pool, because ~A was empty.~%\"\n"
      "     arg1\n"
      "     (ppointer->process (the-as process s4-0))\n"
      "     (-> arg0 name)\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (cond\n"
      "   (s4-0\n"
      "    (set! (-> (the-as (pointer process) s4-0) 0 type) arg1)\n"
      "    (-> (the-as (pointer process) s4-0) 0)\n"
      "    )\n"
      "   (else\n"
      "    (format\n"
      "     0\n"
      "     \"WARNING: ~A ~A could not be allocated, because ~A was empty.~%\"\n"
      "     arg1\n"
      "     (ppointer->process (the-as (pointer process) s4-0))\n"
      "     (-> arg0 name)\n"
      "     )\n"
      "    (the-as process #f)\n"
      "    )\n"
      "   )\n"
      "  )";

  // note - there's likely an actual bug here.
  test_with_expr(
      func, type, expected, false, "dead-pool",
      {{"L315", "WARNING: ~A ~A had to be allocated from the debug pool, because ~A was empty.~%"},
       {"L314", "WARNING: ~A ~A could not be allocated, because ~A was empty.~%"}},
      "[\t\t[24, \"v1\", \"(pointer process)\"],\n"
      "\t\t[[30,39], \"s4\", \"(pointer process)\"]]");
}

TEST_F(FormRegressionTestJak1, ExprMethod15DeadPool) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"

      "    lw t9, change-parent(s7)\n"
      "    or v1, a1, r0\n"
      "    or a1, a0, r0\n"
      "    or a0, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function dead-pool process none)";
  std::string expected = "(begin (change-parent arg1 arg0) (none))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod0DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or s4, a2, r0\n"
      "    or s5, a3, r0\n"
      "    or gp, t0, r0\n"
      "    lw v1, object(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    or v1, a1, r0\n"
      "    lhu a1, 8(a1)\n"
      "    addiu a2, r0, -16\n"
      "    addiu a3, r0, 12\n"
      "    mult3 a3, a3, s5\n"
      "    daddiu a3, a3, 15\n"
      "    and a2, a2, a3\n"
      "    daddu a1, a1, a2\n"
      "    daddu a2, a1, gp\n"
      "    or a1, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    sw s4, 0(v0)\n"
      "    addiu v1, r0, 256\n"
      "    sw v1, 4(v0)\n"
      "    sw s5, 28(v0)\n"
      "    sw s7, 8(v0)\n"
      "    sw s7, 12(v0)\n"
      "    sw s7, 16(v0)\n"
      "    sw v0, 24(v0)\n"
      "    daddiu v1, v0, 24\n"
      "    sw v1, 20(v0)\n"
      "    or v1, s5, r0\n"
      "    beq r0, r0, L220\n"
      "    sll r0, r0, 0\n"

      "L219:\n"
      "    daddiu v1, v1, -1\n"
      "    addiu a0, r0, 12\n"
      "    mult3 a0, a0, v1\n"
      "    daddiu a0, a0, 100\n"
      "    daddu a0, a0, v0\n"
      "    lw a1, *null-process*(s7)\n"
      "    sw a1, 0(a0)\n"
      "    addiu a1, r0, 12\n"
      "    daddiu a2, v1, 1\n"
      "    mult3 a1, a1, a2\n"
      "    daddiu a1, a1, 100\n"
      "    daddu a1, a1, v0\n"
      "    sw a1, 8(a0)\n"

      "L220:\n"
      "    bne v1, r0, L219\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    daddiu v1, v0, 100\n"
      "    sw v1, 96(v0)\n"
      "    sw s7, 76(v0)\n"
      "    addiu v1, r0, 12\n"
      "    daddiu a0, s5, -1\n"
      "    mult3 v1, v1, a0\n"
      "    daddu v1, v0, v1\n"
      "    sw s7, 108(v1)\n"
      "    daddiu v1, v0, 76\n"
      "    sw v1, 80(v0)\n"
      "    sw s7, 84(v0)\n"
      "    sw s7, 76(v0)\n"
      "    daddiu v1, v0, 76\n"
      "    sw v1, 48(v0)\n"
      "    sw s7, 52(v0)\n"
      "    addiu v1, r0, -16\n"
      "    daddiu a0, v0, 115\n"
      "    addiu a1, r0, 12\n"
      "    mult3 a1, a1, s5\n"
      "    daddu a0, a0, a1\n"
      "    and v1, v1, a0\n"
      "    sw v1, 60(v0)\n"
      "    lwu v1, 60(v0)\n"
      "    sw v1, 68(v0)\n"
      "    lwu v1, 60(v0)\n"
      "    daddu v1, v1, gp\n"
      "    sw v1, 64(v0)\n"
      "    lwu v1, 64(v0)\n"
      "    sw v1, 72(v0)\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function symbol type basic int int dead-pool-heap)";
  std::string expected =
      "(let\n"
      "  ((obj\n"
      "    (object-new\n"
      "     arg0\n"
      "     arg1\n"
      "     (the-as int (+ (-> arg1 size) (logand -16 (+ (* 12 arg3) 15)) arg4))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (set! (-> obj name) arg2)\n"
      "  (set! (-> obj mask) (process-mask process-tree))\n"
      "  (set! (-> obj allocated-length) arg3)\n"
      "  (set! (-> obj parent) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> obj brother) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> obj child) (the-as (pointer process-tree) #f))\n"
      "  (set! (-> obj self) obj)\n"
      "  (set! (-> obj ppointer) (the-as (pointer process) (&-> obj self)))\n"
      "  (countdown (v1-4 arg3)\n"
      "   (let ((a0-4 (-> obj process-list v1-4)))\n"
      "    (set! (-> a0-4 process) *null-process*)\n"
      "    (set! (-> a0-4 next) (-> obj process-list (+ v1-4 1)))\n"
      "    )\n"
      "   )\n"
      "  (set!\n"
      "   (-> obj dead-list next)\n"
      "   (the-as dead-pool-heap-rec (-> obj process-list))\n"
      "   )\n"
      "  (set! (-> obj alive-list process) #f)\n"
      "  (set! (-> obj process-list (+ arg3 -1) next) #f)\n"
      "  (set! (-> obj alive-list prev) (-> obj alive-list))\n"
      "  (set! (-> obj alive-list next) #f)\n"
      "  (set! (-> obj alive-list process) #f)\n"
      "  (set! (-> obj first-gap) (-> obj alive-list))\n"
      "  (set! (-> obj first-shrink) #f)\n"
      "  (set!\n"
      "   (-> obj heap base)\n"
      "   (the-as pointer (logand -16 (+ (the-as int obj) 115 (* 12 arg3))))\n"
      "   )\n"
      "  (set! (-> obj heap current) (-> obj heap base))\n"
      "  (set! (-> obj heap top) (&+ (-> obj heap base) arg4))\n"
      "  (set! (-> obj heap top-base) (-> obj heap top))\n"
      "  obj\n"
      "  )";
  test_with_expr(func, type, expected, false, "dead-pool-heap", {},
                 "[\t\t[60, \"v0\", \"int\"],\n"
                 "\t\t[61, \"a0\", \"pointer\"], [61, \"v0\", \"dead-pool-heap\"]]",
                 "{\"vars\":{\"v0-0\":[\"obj\", \"dead-pool-heap\"]}}");
}

TEST_F(FormRegressionTestJak1, ExprMethod22DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lwu v1, 0(a1)\n"
      "    beq s7, v1, L216\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 0(a1)\n"
      "    lw v1, 68(v1)\n"
      "    daddiu v1, v1, -4\n"
      "    lw a0, process(s7)\n"
      "    lhu a0, 8(a0)\n"
      "    daddu v1, v1, a0\n"
      "    lwu a0, 0(a1)\n"
      "    daddu v0, v1, a0\n"
      "    beq r0, r0, L217\n"
      "    sll r0, r0, 0\n"

      "L216:\n"
      "    lwu v0, 60(a0)\n"

      "L217:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dead-pool-heap dead-pool-heap-rec pointer)";
  std::string expected =
      "(the-as pointer (if (-> arg1 process)\n"
      "                (+\n"
      "                 (+ (-> arg1 process allocated-length) -4 (-> process size))\n"
      "                 (the-as int (-> arg1 process))\n"
      "                 )\n"
      "                (-> arg0 heap base)\n"
      "                )\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod21DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L209:\n"
      "    lwu v1, 0(a1)\n"
      "    beq s7, v1, L212\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 0(a1)\n"
      "    lw a2, process(s7)\n"
      "    lhu a2, 8(a2)\n"
      "    daddu v1, v1, a2\n"
      "    lwu a2, 0(a1)\n"
      "    lw a2, 68(a2)\n"
      "    daddu v1, v1, a2\n"
      "    lwu a2, 8(a1)\n"
      "    beq s7, a2, L210\n"
      "    sll r0, r0, 0\n"

      "    lwu a0, 8(a1)\n"
      "    lwu a0, 0(a0)\n"
      "    dsubu v0, a0, v1\n"
      "    beq r0, r0, L211\n"
      "    sll r0, r0, 0\n"

      "L210:\n"
      "    lwu a0, 64(a0)\n"
      "    daddiu v1, v1, 4\n"
      "    dsubu v0, a0, v1\n"

      "L211:\n"
      "    beq r0, r0, L214\n"
      "    sll r0, r0, 0\n"

      "L212:\n"
      "    lwu v1, 8(a1)\n"
      "    beq s7, v1, L213\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 8(a1)\n"
      "    lwu v1, 0(v1)\n"
      "    lwu a0, 60(a0)\n"
      "    daddiu a0, a0, 4\n"
      "    dsubu v0, v1, a0\n"
      "    beq r0, r0, L214\n"
      "    sll r0, r0, 0\n"

      "L213:\n"
      "    lwu v1, 64(a0)\n"
      "    lwu a0, 60(a0)\n"
      "    dsubu v0, v1, a0\n"

      "L214:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dead-pool-heap dead-pool-heap-rec int)";
  std::string expected =
      "(cond\n"
      "  ((-> arg1 process)\n"
      "   (let\n"
      "    ((v1-3\n"
      "      (&+\n"
      "       (&+ (the-as pointer (-> arg1 process)) (-> process size))\n"
      "       (-> arg1 process allocated-length)\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    (if (-> arg1 next)\n"
      "     (&- (the-as pointer (-> arg1 next process)) (the-as uint v1-3))\n"
      "     (&- (-> arg0 heap top) (the-as uint (&+ v1-3 4)))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  ((-> arg1 next)\n"
      "   (&-\n"
      "    (the-as pointer (-> arg1 next process))\n"
      "    (the-as uint (&+ (-> arg0 heap base) 4))\n"
      "    )\n"
      "   )\n"
      "  (else\n"
      "   (&- (-> arg0 heap top) (the-as uint (-> arg0 heap base)))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {},
                 "[\t\t[5, \"v1\", \"pointer\"],\n"
                 "\t\t[13, \"a0\", \"pointer\"],\n"
                 "\t\t[25, \"v1\", \"pointer\"]]");
}

TEST_F(FormRegressionTestJak1, ExprMethod3DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -128\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s0, 16(sp)\n"
      "    sq s1, 32(sp)\n"
      "    sq s2, 48(sp)\n"
      "    sq s3, 64(sp)\n"
      "    sq s4, 80(sp)\n"
      "    sq s5, 96(sp)\n"
      "    sq gp, 112(sp)\n"

      "    or gp, a0, r0\n"

      // CUT HERE

      "    lwu v1, 64(gp)\n"
      "    lwu a0, 60(gp)\n"
      "    dsubu s5, v1, a0\n"
      "    lwu v1, 80(gp)\n"
      "    beq s7, v1, L199\n"
      "    sll r0, r0, 0\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 100(v1)\n"
      "    lwu a1, 80(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    beq r0, r0, L200\n"
      "    sll r0, r0, 0\n"

      "L199:\n"
      "    or v1, s5, r0\n"

      "L200:\n"
      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L300\n"
      "    daddiu a2, gp, 100\n"
      "    dsubu a3, s5, v1\n"
      "    or t0, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    daddiu s5, gp, 76\n"
      "    addiu s4, r0, 0\n"
      "    beq r0, r0, L204\n"
      "    sll r0, r0, 0\n"

      "L201:\n"
      "    lwu v1, 0(s5)\n"
      "    beq s7, v1, L202\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    daddiu a0, s7, #t\n"
      "    daddiu a1, fp, L299\n"
      "    or a2, s4, r0\n"
      "    or a3, s5, r0\n"
      "    lwu t0, 0(s5)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L202:\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 100(v1)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s3, v0, r0\n"
      "    beq s3, r0, L203\n"
      "    or v1, s7, r0\n"

      "    lw s2, format(s7)\n"
      "    daddiu s1, s7, #t\n"
      "    daddiu s0, fp, L298\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or a3, v0, r0\n"
      "    or t9, s2, r0\n"
      "    or a0, s1, r0\n"
      "    or a1, s0, r0\n"
      "    or a2, s3, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L203:\n"
      "    lwu s5, 8(s5)\n"
      "    daddiu s4, s4, 1\n"

      "L204:\n"
      "    bne s7, s5, L201\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 112(sp)\n"
      "    lq s5, 96(sp)\n"
      "    lq s4, 80(sp)\n"
      "    lq s3, 64(sp)\n"
      "    lq s2, 48(sp)\n"
      "    lq s1, 32(sp)\n"
      "    lq s0, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 128";
  std::string type = "(function dead-pool-heap dead-pool-heap)";
  std::string expected =
      "(begin\n"
      "  (let*\n"
      "   ((s5-0 (&- (-> arg0 heap top) (the-as uint (-> arg0 heap base))))\n"
      "    (v1-3\n"
      "     (if\n"
      "      (-> arg0 alive-list prev)\n"
      "      (gap-size arg0 (-> arg0 alive-list prev))\n"
      "      s5-0\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (format\n"
      "    #t\n"
      "    \"~Tprocess-list[0] @ #x~X     ~D/~D bytes used~%\"\n"
      "    (-> arg0 process-list)\n"
      "    (- s5-0 v1-3)\n"
      "    s5-0\n"
      "    )\n"
      "   )\n"
      "  (let\n"
      "   ((s5-1 (-> arg0 alive-list)) (s4-0 0))\n"
      "   (while\n"
      "    s5-1\n"
      "    (if\n"
      "     (-> s5-1 process)\n"
      "     (format\n"
      "      #t\n"
      "      \"~T  [~3D] #<dead-pool-heap-rec @ #x~X>  ~A~%\"\n"
      "      s4-0\n"
      "      s5-1\n"
      "      (-> s5-1 process)\n"
      "      )\n"
      "     )\n"
      "    (let\n"
      "     ((s3-0 (gap-size arg0 s5-1)))\n"
      "     (if\n"
      "      (nonzero? s3-0)\n"
      "      (format #t \"~T   gap: ~D bytes @ #x~X~%\" s3-0 (gap-location arg0 s5-1))\n"
      "      )\n"
      "     )\n"
      "    (set! s5-1 (-> s5-1 next))\n"
      "    (+! s4-0 1)\n"
      "    )\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {{"L300", "~Tprocess-list[0] @ #x~X     ~D/~D bytes used~%"},
                  {"L299", "~T  [~3D] #<dead-pool-heap-rec @ #x~X>  ~A~%"},
                  {"L298", "~T   gap: ~D bytes @ #x~X~%"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod5DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    addiu v1, r0, -4\n"
      "    dsubu v1, v1, a0\n"
      "    lwu a0, 64(a0)\n"
      "    daddu v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dead-pool-heap int)";
  std::string expected =
      "(+ (the-as int (- -4 (the-as int arg0))) (the-as int (-> arg0 heap top)))";
  test_with_expr(func, type, expected, false, "", {},
                 "[[3, \"v1\", \"int\"], [3, \"a0\", \"int\"]]");
}

TEST_F(FormRegressionTestJak1, ExprMethod19DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L194:\n"
      "    daddiu sp, sp, -48\n"
      "    sd ra, 0(sp)\n"
      "    sq s5, 16(sp)\n"
      "    sq gp, 32(sp)\n"

      "    or gp, a0, r0\n"
      "    lwu v1, 80(gp)\n"
      "    beq s7, v1, L195\n"
      "    sll r0, r0, 0\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 96(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s5, v0, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 100(v1)\n"
      "    lwu a1, 80(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    dsubu v0, s5, v1\n"
      "    beq r0, r0, L196\n"
      "    sll r0, r0, 0\n"

      "L195:\n"
      "    addiu v0, r0, 0\n"

      "L196:\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 32(sp)\n"
      "    lq s5, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 48";
  std::string type = "(function dead-pool-heap int)";
  std::string expected =
      "(if\n"
      "  (-> arg0 alive-list prev)\n"
      "  (- (memory-total arg0) (gap-size arg0 (-> arg0 alive-list prev)))\n"
      "  0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod20DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lwu v1, 64(a0)\n"
      "    lwu a0, 60(a0)\n"
      "    dsubu v0, v1, a0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dead-pool-heap int)";
  std::string expected = "(&- (-> arg0 heap top) (the-as uint (-> arg0 heap base)))";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod25DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -16\n"
      "    sd ra, 0(sp)\n"

      "    lwu v1, 64(a0)\n"
      "    lwu a1, 80(a0)\n"
      "    beq s7, a1, L191\n"
      "    sll r0, r0, 0\n"

      "    or v1, a0, r0\n"
      "    lwu a1, -4(v1)\n"
      "    lwu t9, 100(a1)\n"
      "    lwu a1, 80(a0)\n"
      "    or a0, v1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    beq r0, r0, L192\n"
      "    sll r0, r0, 0\n"

      "L191:\n"
      "    lwu a0, 60(a0)\n"
      "    dsubu v0, v1, a0\n"

      "L192:\n"
      "    ld ra, 0(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 16";
  std::string type = "(function dead-pool-heap int)";
  std::string expected =
      "(let\n"
      "  ((v1-0 (-> arg0 heap top)))\n"
      "  (if\n"
      "   (-> arg0 alive-list prev)\n"
      "   (gap-size arg0 (-> arg0 alive-list prev))\n"
      "   (&- v1-0 (the-as uint (-> arg0 heap base)))\n"
      "   )\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod26DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    lwu v0, 32(a0)\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function dead-pool-heap uint)";
  std::string expected = "(-> arg0 compact-time)";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod24DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or s5, a0, r0\n"
      "    or s4, a1, r0\n"
      "    lwu gp, 48(s5)\n"
      "    beq r0, r0, L187\n"
      "    sll r0, r0, 0\n"

      "L186:\n"
      "    lwu gp, 8(gp)\n"

      "L187:\n"
      "    beql s7, gp, L188\n"

      "    or v1, gp, r0\n"

      "    or a0, s5, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 100(v1)\n"
      "    or a1, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    slt a0, v1, s4\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L188:\n"
      "    bne s7, v1, L186\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function dead-pool-heap int dead-pool-heap-rec)";
  std::string expected =
      "(let\n"
      "  ((gp-0 (-> arg0 first-gap)))\n"
      "  (while (and gp-0 (< (gap-size arg0 gp-0) arg1)) (set! gp-0 (-> gp-0 next)))\n"
      "  gp-0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod14DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -112\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s1, 16(sp)\n"
      "    sq s2, 32(sp)\n"
      "    sq s3, 48(sp)\n"
      "    sq s4, 64(sp)\n"
      "    sq s5, 80(sp)\n"
      "    sq gp, 96(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    or s2, a2, r0\n"
      "    lwu s4, 96(gp)\n"
      "    or s3, s7, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 112(v1)\n"
      "    lw v1, process(s7)\n"
      "    lhu v1, 8(v1)\n"
      "    daddu a1, v1, s2\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or s1, v0, r0\n"
      "    beql s7, s4, L173\n"

      "    or v1, s4, r0\n"

      "    or v1, s1, r0\n"

      "L173:\n"
      "    beq s7, v1, L179\n"
      "    sll r0, r0, 0\n"

      "    lwu v1, 8(s4)\n"
      "    sw v1, 96(gp)\n"
      "    lwu v1, 8(s1)\n"
      "    sw s4, 8(s1)\n"
      "    sw v1, 8(s4)\n"
      "    beq s7, v1, L174\n"
      "    or a0, s7, r0\n"

      "    sw s4, 4(v1)\n"
      "    or v1, s4, r0\n"

      "L174:\n"
      "    sw s1, 4(s4)\n"
      "    lwu v1, 80(gp)\n"
      "    bne s1, v1, L175\n"
      "    or v1, s7, r0\n"

      "    sw s4, 80(gp)\n"
      "    or v1, s4, r0\n"

      "L175:\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    or a1, s1, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or a0, v0, r0\n"
      "    lw v1, process(s7)\n"
      "    lwu t9, 16(v1)\n"
      "    lw a1, process(s7)\n"
      "    daddiu a2, s7, process\n"
      "    or a3, s2, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s3, v0, r0\n"
      "    sw s3, 0(s4)\n"
      "    daddu v1, r0, s4\n"
      "    sw v1, 20(s3)\n"
      "    lwu v1, 48(gp)\n"
      "    bne v1, s1, L176\n"
      "    or v1, s7, r0\n"
      "\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 108(v1)\n"
      "    or a1, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    sw v1, 48(gp)\n"

      "L176:\n"
      "    lwu v1, 52(gp)\n"
      "    beql s7, v1, L177\n"

      "    daddiu v1, s7, 8\n"

      "    lwu v1, 52(gp)\n"
      "    lwu v1, 0(v1)\n"
      "    slt a0, s3, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L177:\n"
      "    beq s7, v1, L178\n"
      "    or v1, s7, r0\n"

      "    sw s4, 52(gp)\n"
      "    or v1, s4, r0\n"

      "L178:\n"
      "    lwu v1, 20(gp)\n"
      "    sw v1, 8(s3)\n"
      "    sw gp, 28(s3)\n"
      "    daddu v1, r0, s4\n"
      "    sw v1, 16(gp)\n"
      "    beq r0, r0, L182\n"
      "    sll r0, r0, 0\n"

      "L179:\n"
      "    lw v1, *debug-segment*(s7)\n"
      "    beql s7, v1, L180\n"

      "    or v1, v1, r0\n"

      "    lw v1, *debug-dead-pool*(s7)\n"
      "    dsubu a0, gp, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L180:\n"
      "    beq s7, v1, L182\n"
      "    or v1, s7, r0\n"
      "\n"

      "    lw a0, *debug-dead-pool*(s7)\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 72(v1)\n"
      "    or a1, s5, r0\n"
      "    or a2, s2, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s3, v0, r0\n"
      "    beql s7, s3, L181\n"

      "    or v1, s3, r0\n"
      "\n"

      "    lw v1, *vis-boot*(s7)\n"

      "L181:\n"
      "    beq s7, v1, L182\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L315\n"
      "    or a2, s5, r0\n"
      "    or a3, s3, r0\n"
      "    lwu t0, 0(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L182:\n"
      "    beq s7, s3, L183\n"
      "    sll r0, r0, 0\n"

      "    sw s5, -4(s3)\n"
      "    beq r0, r0, L184\n"
      "    sll r0, r0, 0\n"

      "L183:\n"
      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L314\n"
      "    or a2, s5, r0\n"
      "    or a3, s3, r0\n"
      "    lwu t0, 0(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L184:\n"
      "    or v0, s3, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 96(sp)\n"
      "    lq s5, 80(sp)\n"
      "    lq s4, 64(sp)\n"
      "    lq s3, 48(sp)\n"
      "    lq s2, 32(sp)\n"
      "    lq s1, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 112";
  std::string type = "(function dead-pool-heap type int process)";
  std::string expected =
      "(let ((s4-0 (-> arg0 dead-list next))\n"
      "     (s3-0 (the-as process #f))\n"
      "     )\n"
      "  (let ((s1-0 (find-gap-by-size arg0 (the-as int (+ (-> process size) arg2)))))\n"
      "   (cond\n"
      "    ((and s4-0 s1-0)\n"
      "     (set! (-> arg0 dead-list next) (-> s4-0 next))\n"
      "     (let ((v1-5 (-> s1-0 next)))\n"
      "      (set! (-> s1-0 next) s4-0)\n"
      "      (set! (-> s4-0 next) v1-5)\n"
      "      (if v1-5\n"
      "       (set! (-> v1-5 prev) s4-0)\n"
      "       )\n"
      "      )\n"
      "     (set! (-> s4-0 prev) s1-0)\n"
      "     (if (= s1-0 (-> arg0 alive-list prev))\n"
      "      (set! (-> arg0 alive-list prev) s4-0)\n"
      "      )\n"
      "     (let ((a0-4 (gap-location arg0 s1-0)))\n"
      "      (set!\n"
      "       s3-0\n"
      "       ((method-of-type process new) (the-as symbol a0-4) process 'process arg2)\n"
      "       )\n"
      "      )\n"
      "     (set! (-> s4-0 process) s3-0)\n"
      "     (set! (-> s3-0 ppointer) (&-> s4-0 process))\n"
      "     (if (= (-> arg0 first-gap) s1-0)\n"
      "      (set! (-> arg0 first-gap) (find-gap arg0 s4-0))\n"
      "      )\n"
      "     (if\n"
      "      (or\n"
      "       (not (-> arg0 first-shrink))\n"
      "       (< (the-as int s3-0) (the-as int (-> arg0 first-shrink process)))\n"
      "       )\n"
      "      (set! (-> arg0 first-shrink) s4-0)\n"
      "      )\n"
      "     (set! (-> s3-0 parent) (-> arg0 ppointer))\n"
      "     (set! (-> s3-0 pool) arg0)\n"
      "     (set! (-> arg0 child) (&-> s4-0 process))\n"
      "     )\n"
      "    (else\n"
      "     (when (and *debug-segment* (!= arg0 *debug-dead-pool*))\n"
      "      (set! s3-0 (get-process *debug-dead-pool* arg1 arg2))\n"
      "      (if (and s3-0 *vis-boot*)\n"
      "       (format\n"
      "        0\n"
      "        \"WARNING: ~A ~A had to be allocated from the debug pool, because ~A was "
      "empty.~%\"\n"
      "        arg1\n"
      "        s3-0\n"
      "        (-> arg0 name)\n"
      "        )\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (if s3-0\n"
      "   (set! (-> s3-0 type) arg1)\n"
      "   (format\n"
      "    0\n"
      "    \"WARNING: ~A ~A could not be allocated, because ~A was empty.~%\"\n"
      "    arg1\n"
      "    s3-0\n"
      "    (-> arg0 name)\n"
      "    )\n"
      "   )\n"
      "  s3-0\n"
      "  )";
  test_with_expr(
      func, type, expected, false, "",
      {{"L315", "WARNING: ~A ~A had to be allocated from the debug pool, because ~A was empty.~%"},
       {"L314", "WARNING: ~A ~A could not be allocated, because ~A was empty.~%"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod15DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      " daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    lwu v1, 28(s5)\n"
      "    beq gp, v1, L166\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    addiu a0, r0, 0\n"
      "    daddiu a1, fp, L297\n"
      "    or a2, s5, r0\n"
      "    or a3, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L166:\n"
      "    lw t9, change-parent(s7)\n"
      "    or a0, s5, r0\n"
      "    or a1, gp, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    sw s7, 16(gp)\n"
      "    lwu s5, 20(s5)\n"
      "    lwu v1, 48(gp)\n"
      "    dsubu v1, v1, s5\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L167\n"

      "    or v1, a0, r0\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s4, v0, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    lwu a1, 48(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    slt a0, s4, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L167:\n"
      "    beq s7, v1, L168\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 4(s5)\n"
      "    sw v1, 48(gp)\n"

      "L168:\n"
      "    lwu v1, 52(gp)\n"
      "    bne v1, s5, L169\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 4(s5)\n"
      "    sw v1, 52(gp)\n"
      "    lwu v1, 52(gp)\n"
      "    lwu v1, 0(v1)\n"
      "    bne s7, v1, L169\n"
      "    or v1, s7, r0\n"

      "    sw s7, 52(gp)\n"
      "    or v1, s7, r0\n"

      "L169:\n"
      "    lwu v1, 8(s5)\n"
      "    lwu a0, 4(s5)\n"
      "    sw v1, 8(a0)\n"
      "    lwu v1, 8(s5)\n"
      "    beq s7, v1, L170\n"
      "    sll r0, r0, 0\n"

      "    lwu a0, 4(s5)\n"
      "    lwu v1, 8(s5)\n"
      "    sw a0, 4(v1)\n"
      "    beq r0, r0, L171\n"
      "    sll r0, r0, 0\n"

      "L170:\n"
      "    lwu v1, 4(s5)\n"
      "    sw v1, 80(gp)\n"

      "L171:\n"
      "    lwu v1, 96(gp)\n"
      "    sw v1, 8(s5)\n"
      "    sw s5, 96(gp)\n"
      "    lw v1, *null-process*(s7)\n"
      "    sw v1, 0(s5)\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function dead-pool-heap process none)";
  // NOTE: has wrong types for s5-1, but it's okay
  std::string expected =
      "(begin\n"
      "  (if (!= arg0 (-> arg1 pool))\n"
      "   (format\n"
      "    0\n"
      "    \"ERROR: process ~A does not belong to dead-pool-heap ~A.~%\"\n"
      "    arg1\n"
      "    arg0\n"
      "    )\n"
      "   )\n"
      "  (change-parent arg1 arg0)\n"
      "  (set! (-> arg0 child) (the-as (pointer process-tree) #f))\n"
      "  (let ((s5-1 (-> arg1 ppointer)))\n"
      "   (if\n"
      "    (or\n"
      "     (= (-> arg0 first-gap) s5-1)\n"
      "     (<\n"
      "      (the-as int (gap-location arg0 (the-as dead-pool-heap-rec s5-1)))\n"
      "      (the-as int (gap-location arg0 (-> arg0 first-gap)))\n"
      "      )\n"
      "     )\n"
      "    (set! (-> arg0 first-gap) (the-as dead-pool-heap-rec (-> s5-1 1)))\n"
      "    )\n"
      "   (when (= (-> arg0 first-shrink) s5-1)\n"
      "    (set! (-> arg0 first-shrink) (the-as dead-pool-heap-rec (-> s5-1 1)))\n"
      "    (if (not (-> arg0 first-shrink process))\n"
      "     (set! (-> arg0 first-shrink) #f)\n"
      "     )\n"
      "    )\n"
      "   (set! (-> s5-1 1 parent) (the-as (pointer process-tree) (-> s5-1 2)))\n"
      "   (if (-> s5-1 2)\n"
      "    (set! (-> s5-1 2 mask) (the-as process-mask (-> s5-1 1)))\n"
      "    (set! (-> arg0 alive-list prev) (the-as dead-pool-heap-rec (-> s5-1 1)))\n"
      "    )\n"
      "   (set! (-> s5-1 2) (the-as process (-> arg0 dead-list next)))\n"
      "   (set! (-> arg0 dead-list next) (the-as dead-pool-heap-rec s5-1))\n"
      "   (set! (-> s5-1 0) *null-process*)\n"
      "   )\n"
      "  0\n"
      "  (none)\n"
      "  )";
  test_with_expr(func, type, expected, false, "",
                 {{"L297", "ERROR: process ~A does not belong to dead-pool-heap ~A.~%"}});
}

TEST_F(FormRegressionTestJak1, ExprMethod17DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -64\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 16(sp)\n"
      "    sq s5, 32(sp)\n"
      "    sq gp, 48(sp)\n"

      "    or gp, a0, r0\n"
      "    or s4, a1, r0\n"
      "    beq s7, s4, L164\n"
      "    or v1, s7, r0\n"

      "    lwu s5, 20(s4)\n"
      "    lwu v1, 4(s4)\n"
      "    andi v1, v1, 512\n"
      "    bnel v1, r0, L161\n"

      "    daddiu v1, s7, 8\n"

      "    lwu v1, 72(s4)\n"
      "    bnel s7, v1, L161\n"

      "    or v1, s7, r0\n"

      "    lwu v1, 52(s4)\n"
      "    beq s7, v1, L161\n"
      "    daddiu v1, s7, 8\n"  // one

      "    or v1, s7, r0\n"  // two

      "L161:\n"
      "    bne s7, v1, L163\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 84(s4)\n"
      "    daddiu a0, s4, 108\n"
      "    dsubu v1, v1, a0\n"
      "    sw v1, 68(s4)\n"
      "    lw v1, 68(s4)\n"
      "    daddiu v1, v1, 108\n"
      "    daddu v1, v1, s4\n"
      "    sw v1, 80(s4)\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    lwu a1, 48(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    slt v1, s4, v1\n"
      "    beq v1, r0, L162\n"
      "    or v1, s7, r0\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 108(v1)\n"
      "    or a1, s5, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    sw v1, 48(gp)\n"

      "L162:\n"
      "    lwu v1, 4(s4)\n"
      "    ori v1, v1, 512\n"
      "    sw v1, 4(s4)\n"

      "L163:\n"
      "    lwu v1, 52(gp)\n"
      "    bne v1, s5, L164\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 8(s5)\n"
      "    sw v1, 52(gp)\n"

      "L164:\n"
      "    or v0, gp, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 48(sp)\n"
      "    lq s5, 32(sp)\n"
      "    lq s4, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 64";
  std::string type = "(function dead-pool-heap process dead-pool-heap)";

  // NOTE - this has bad types.
  std::string expected =
      "(begin\n"
      "  (when arg1\n"
      "   (let ((s5-0 (-> arg1 ppointer)))\n"
      "    (when\n"
      "     (not\n"
      "      (or\n"
      "       (logtest? (-> arg1 mask) (process-mask heap-shrunk))\n"
      "       (and (not (-> arg1 next-state)) (not (-> arg1 state)))\n"
      "       )\n"
      "      )\n"
      "     (set!\n"
      "      (-> arg1 allocated-length)\n"
      "      (&- (-> arg1 heap-cur) (the-as uint (-> arg1 stack)))\n"
      "      )\n"
      "     (set! (-> arg1 heap-top) (&-> arg1 stack (-> arg1 allocated-length)))\n"
      "     (if\n"
      "      (< (the-as int arg1) (the-as int (gap-location arg0 (-> arg0 first-gap))))\n"
      "      (set!\n"
      "       (-> arg0 first-gap)\n"
      "       (find-gap arg0 (the-as dead-pool-heap-rec s5-0))\n"
      "       )\n"
      "      )\n"
      "     (logior! (-> arg1 mask) (process-mask heap-shrunk))\n"
      "     )\n"
      "    (if (= (-> arg0 first-shrink) s5-0)\n"
      "     (set! (-> arg0 first-shrink) (the-as dead-pool-heap-rec (-> s5-0 2)))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  arg0\n"
      "  )";
  test_with_expr(func, type, expected);
}

TEST_F(FormRegressionTestJak1, ExprMethod16DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    daddiu sp, sp, -96\n"
      "    sd ra, 0(sp)\n"
      "    sd fp, 8(sp)\n"
      "    or fp, t9, r0\n"
      "    sq s2, 16(sp)\n"
      "    sq s3, 32(sp)\n"
      "    sq s4, 48(sp)\n"
      "    sq s5, 64(sp)\n"
      "    sq gp, 80(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 116(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s4, v0, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 96(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    mtc1 f0, s4\n"
      "    cvt.s.w f0, f0\n"
      "    mtc1 f1, v1\n"
      "    cvt.s.w f1, f1\n"
      "    div.s f0, f0, f1\n"
      //"    lwc1 f1, L346(fp)\n"
      "    mtc1 f1, r0\n"
      "    c.lt.s f0, f1\n"
      "    bc1f L152\n"
      "    or v1, s7, r0\n"

      "    addiu s5, r0, 1000\n"
      "    lw v1, *debug-segment*(s7)\n"
      "    beql s7, v1, L150\n"

      "    or v1, v1, r0\n"

      "    lw v1, *kernel-context*(s7)\n"
      "    lwu v1, 40(v1)\n"

      "L150:\n"
      "    beq s7, v1, L151\n"
      "    or v1, s7, r0\n"

      "    lw t9, format(s7)\n"
      "    lw a0, *stdcon*(s7)\n"
      "    daddiu a1, fp, L296\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"

      "L151:\n"
      "    beq r0, r0, L154\n"
      "    sll r0, r0, 0\n"

      "L152:\n"
      "    mtc1 f1, r0\n"
      //"    lwc1 f1, L347(fp)\n"
      "    c.lt.s f0, f1\n"
      "    bc1f L153\n"
      "    or v1, s7, r0\n"

      "    dsll s5, s5, 2\n"
      "    or v1, s5, r0\n"
      "    beq r0, r0, L154\n"
      "    sll r0, r0, 0\n"

      "L153:\n"
      "    mtc1 f1, r0\n"
      // "    lwc1 f1, L348(fp)\n"
      "    c.lt.s f0, f1\n"
      "    bc1f L154\n"
      "    or v1, s7, r0\n"

      "    dsll s5, s5, 1\n"
      "    or v1, s5, r0\n"

      "L154:\n"
      "    sw s5, 36(gp)\n"
      "    sw r0, 40(gp)\n"
      "    beq r0, r0, L159\n"
      "    sll r0, r0, 0\n"

      "L155:\n"
      "    daddiu s5, s5, -1\n"
      "    lwu v1, 52(gp)\n"
      "    bne s7, v1, L156\n"
      "    or a0, s7, r0\n"

      "    lwu v1, 84(gp)\n"
      "    sw v1, 52(gp)\n"
      "    or a0, v1, r0\n"

      "L156:\n"
      "    beq s7, v1, L157\n"
      "    or a0, s7, r0\n"

      "    or a0, gp, r0\n"
      "    lwu a1, -4(a0)\n"
      "    lwu t9, 84(a1)\n"
      "    lwu a1, 0(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or a0, v0, r0\n"

      "L157:\n"
      "    lwu s4, 48(gp)\n"
      "    lwu v1, 8(s4)\n"
      "    beq s7, v1, L159\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 8(s4)\n"
      "    lwu s3, 0(v1)\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 100(v1)\n"
      "    or a1, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or s2, v0, r0\n"
      "    beq s2, r0, L159\n"
      "    or v1, s7, r0\n"

      "    slt v1, s2, r0\n"
      "    beq v1, r0, L158\n"
      "    or v1, s7, r0\n"

      "    lw r0, 2(r0)\n"
      "    addiu v1, r0, 0\n"

      "L158:\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 84(v1)\n"
      "    or a1, s3, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    lwu v1, -4(s3)\n"
      "    lwu t9, 44(v1)\n"
      "    dsubu a1, r0, s2\n"
      "    or a0, s3, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 108(v1)\n"
      "    or a1, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    sw v1, 48(gp)\n"
      "    lwu v1, 40(gp)\n"
      "    daddiu v1, v1, 1\n"
      "    sw v1, 40(gp)\n"

      "L159:\n"
      "    bne s5, r0, L155\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    ld fp, 8(sp)\n"
      "    lq gp, 80(sp)\n"
      "    lq s5, 64(sp)\n"
      "    lq s4, 48(sp)\n"
      "    lq s3, 32(sp)\n"
      "    lq s2, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 96";
  std::string type = "(function dead-pool-heap int none)";

  std::string expected =
      "(begin\n"
      "  (let* ((s4-0 (memory-free arg0))\n"
      "         (v1-2 (memory-total arg0))\n"
      "         (f0-2 (/ (the float s4-0) (the float v1-2)))\n"
      "         )\n"
      "   (cond\n"
      "    ((< f0-2 0.0)\n"
      "     (set! arg1 1000)\n"
      "     (if (and *debug-segment* (-> *kernel-context* low-memory-message))\n"
      "      (format *stdcon* \"~3LLow Actor Memory~%~0L\")\n"
      "      )\n"
      "     )\n"
      "    ((< f0-2 0.0)\n"
      "     (set! arg1 (* arg1 4))\n"
      "     )\n"
      "    ((< f0-2 0.0)\n"
      "     (set! arg1 (* arg1 2))\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  (set! (-> arg0 compact-count-targ) (the-as uint arg1))\n"
      "  (set! (-> arg0 compact-count) (the-as uint 0))\n"
      "  (while (nonzero? arg1)\n"
      "   (+! arg1 -1)\n"
      "   (let ((v1-13 (-> arg0 first-shrink)))\n"
      "    (when (not v1-13)\n"
      "     (set! v1-13 (-> arg0 alive-list next))\n"
      "     (set! (-> arg0 first-shrink) v1-13)\n"
      "     )\n"
      "    (if v1-13\n"
      "     (shrink-heap arg0 (-> v1-13 process))\n"
      "     )\n"
      "    )\n"
      "   (let ((s4-1 (-> arg0 first-gap)))\n"
      "    (when (-> s4-1 next)\n"
      "     (let ((s3-0 (-> s4-1 next process))\n"
      "           (s2-0 (gap-size arg0 s4-1))\n"
      "           )\n"
      "      (when (nonzero? s2-0)\n"
      "       (when (< s2-0 0)\n"
      "        (break!)\n"
      "        0\n"
      "        )\n"
      "       (shrink-heap arg0 s3-0)\n"
      "       (relocate s3-0 (- s2-0))\n"
      "       (set! (-> arg0 first-gap) (find-gap arg0 s4-1))\n"
      "       (+! (-> arg0 compact-count) 1)\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  0\n"
      "  (none)\n"
      "  )";
  test_with_expr(func, type, expected, false, "", {{"L296", "~3LLow Actor Memory~%~0L"}});
}

// nested method calls
TEST_F(FormRegressionTestJak1, ExprMethod18DeadPoolHeap) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L140:\n"
      "    daddiu sp, sp, -96\n"
      "    sd ra, 0(sp)\n"
      "    sq s2, 16(sp)\n"
      "    sq s3, 32(sp)\n"
      "    sq s4, 48(sp)\n"
      "    sq s5, 64(sp)\n"
      "    sq gp, 80(sp)\n"

      "    or gp, a0, r0\n"
      "    or s5, a1, r0\n"
      "    beq r0, r0, L148\n"
      "    sll r0, r0, 0\n"

      "L141:\n"
      "    daddiu s5, s5, -1\n"
      "    lwu s4, 84(gp)\n"
      "    beq s7, s4, L148\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 48(gp)\n"
      "    dsubu v1, v1, s4\n"
      "    daddiu a0, s7, 8\n"
      "    movn a0, s7, v1\n"
      "    bnel s7, a0, L142\n"

      "    or v1, a0, r0\n"

      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    or a1, s4, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or s3, v0, r0\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    lwu a1, 48(gp)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    slt a0, s3, v1\n"
      "    daddiu v1, s7, 8\n"
      "    movz v1, s7, a0\n"

      "L142:\n"
      "    beq s7, v1, L143\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 4(s4)\n"
      "    sw v1, 48(gp)\n"

      "L143:\n"
      "    lwu v1, 52(gp)\n"
      "    bne v1, s4, L144\n"
      "    or v1, s7, r0\n"

      "    lwu v1, 4(s4)\n"
      "    sw v1, 52(gp)\n"
      "    lwu v1, 52(gp)\n"
      "    lwu v1, 0(v1)\n"
      "    bne s7, v1, L144\n"
      "    or v1, s7, r0\n"

      "    sw s7, 52(gp)\n"
      "    or v1, s7, r0\n"

      "L144:\n"
      "    lwu v1, 8(s4)\n"
      "    lwu a0, 4(s4)\n"
      "    sw v1, 8(a0)\n"
      "    lwu v1, 8(s4)\n"
      "    beq s7, v1, L145\n"
      "    sll r0, r0, 0\n"

      "    lwu a0, 4(s4)\n"
      "    lwu v1, 8(s4)\n"
      "    sw a0, 4(v1)\n"
      "    beq r0, r0, L146\n"
      "    sll r0, r0, 0\n"

      "L145:\n"
      "    lwu v1, 4(s4)\n"
      "    sw v1, 80(gp)\n"

      "L146:\n"
      "    lwu a1, 80(gp)\n"
      "    lwu v1, 8(a1)\n"
      "    sw s4, 8(a1)\n"
      "    sw v1, 8(s4)\n"
      "    beq s7, v1, L147\n"
      "    or a0, s7, r0\n"

      "    sw s4, 4(v1)\n"
      "    or v1, s4, r0\n"

      "L147:\n"
      "    sw a1, 4(s4)\n"
      "    sw s4, 80(gp)\n"
      "    lwu s3, 0(s4)\n"
      "    lwu v1, -4(s3)\n"
      "    lwu s2, 44(v1)\n"
      "    or a0, gp, r0\n"
      "    lwu v1, -4(a0)\n"
      "    lwu t9, 104(v1)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    lwu a0, 0(s4)\n"
      "    daddiu a0, a0, -4\n"
      "    dsubu a1, v1, a0\n"
      "    or t9, s2, r0\n"
      "    or a0, s3, r0\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"
      "\n"
      "    or v1, v0, r0\n"
      "    sw v1, 0(s4)\n"

      "L148:\n"
      "    bne s5, r0, L141\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v1, s7, r0\n"
      "    or v0, r0, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 80(sp)\n"
      "    lq s5, 64(sp)\n"
      "    lq s4, 48(sp)\n"
      "    lq s3, 32(sp)\n"
      "    lq s2, 16(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 96";
  std::string type = "(function dead-pool-heap int none)";
  std::string expected =
      "(begin\n"
      "  (while (nonzero? arg1)\n"
      "   (+! arg1 -1)\n"
      "   (let ((s4-0 (-> arg0 alive-list next)))\n"
      "    (when s4-0\n"
      "     (if\n"
      "      (or\n"
      "       (= (-> arg0 first-gap) s4-0)\n"
      "       (<\n"
      "        (the-as int (gap-location arg0 s4-0))\n"
      "        (the-as int (gap-location arg0 (-> arg0 first-gap)))\n"
      "        )\n"
      "       )\n"
      "      (set! (-> arg0 first-gap) (-> s4-0 prev))\n"
      "      )\n"
      "     (when (= (-> arg0 first-shrink) s4-0)\n"
      "      (set! (-> arg0 first-shrink) (-> s4-0 prev))\n"
      "      (if (not (-> arg0 first-shrink process))\n"
      "       (set! (-> arg0 first-shrink) #f)\n"
      "       )\n"
      "      )\n"
      "     (set! (-> s4-0 prev next) (-> s4-0 next))\n"
      "     (if (-> s4-0 next)\n"
      "      (set! (-> s4-0 next prev) (-> s4-0 prev))\n"
      "      (set! (-> arg0 alive-list prev) (-> s4-0 prev))\n"
      "      )\n"
      "     (let ((a1-3 (-> arg0 alive-list prev)))\n"
      "      (let ((v1-19 (-> a1-3 next)))\n"
      "       (set! (-> a1-3 next) s4-0)\n"
      "       (set! (-> s4-0 next) v1-19)\n"
      "       (if v1-19\n"
      "        (set! (-> v1-19 prev) s4-0)\n"
      "        )\n"
      "       )\n"
      "      (set! (-> s4-0 prev) a1-3)\n"
      "      (set! (-> arg0 alive-list prev) s4-0)\n"
      "      (set!\n"
      "       (-> s4-0 process)\n"
      "       (relocate\n"
      "        (-> s4-0 process)\n"
      "        (&- (gap-location arg0 a1-3) (the-as uint (&-> (-> s4-0 process) type)))\n"
      "        )\n"
      "       )\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   )\n"
      "  0\n"
      "  (none)\n"
      "  )";
  test_with_expr(func, type, expected);
}
