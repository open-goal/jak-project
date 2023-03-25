#include "FormRegressionTest.h"

#include "gtest/gtest.h"

using namespace decompiler;

// vector-rad<-vector-deg/2!
TEST_F(FormRegressionTestJak1, VectorDegToVectorRad) {
  std::string func =
      "sll r0, r0, 0\n"
      "    lui v1, 14537\n"
      "    ori v1, v1, 4058\n"
      "    lui a2, 16128\n"
      "    lqc2 vf1, 0(a1)\n"
      "    qmtc2.i vf2, a2\n"
      "    vmulx.xyzw vf1, vf1, vf2\n"
      "    vftoi0.xyzw vf1, vf1\n"
      "    qmtc2.i vf2, v1\n"
      "    qmfc2.i v1, vf1\n"
      "    psllw v1, v1, 16\n"
      "    psraw v1, v1, 16\n"
      "    qmtc2.i vf1, v1\n"
      "    vitof0.xyzw vf1, vf1\n"
      "    vmulx.xyzw vf1, vf1, vf2\n"
      "    sqc2 vf1, 0(a0)\n"
      "    qmfc2.i v0, vf1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function vector vector none)";
  std::string expected =
      "(defun test-function ((arg0 vector) (arg1 vector))\n"
      "  (local-vars (v0-0 float) (v1-1 uint128) (v1-2 uint128) (v1-3 uint128))\n"
      "  (rlet ((vf1 :class vf)\n"
      "         (vf2 :class vf)\n"
      "         )\n"
      "   (let ((v1-0 #x38c90fda))\n"
      "    (let ((a2-0 #x3f000000))\n"
      "     (.lvf vf1 (&-> arg1 quad))\n"
      "     (.mov vf2 a2-0)\n"
      "     )\n"
      "    (.mul.x.vf vf1 vf1 vf2)\n"
      "    (.ftoi.vf vf1 vf1)\n"
      "    (.mov vf2 v1-0)\n"
      "    )\n"
      "   (.mov v1-1 vf1)\n"
      "   (.pw.sll v1-2 v1-1 16)\n"
      "   (.pw.sra v1-3 v1-2 16)\n"
      "   (.mov vf1 v1-3)\n"
      "   (.itof.vf vf1 vf1)\n"
      "   (.mul.x.vf vf1 vf1 vf2)\n"
      "   (.svf (&-> arg0 quad) vf1)\n"
      "   (.mov v0-0 vf1)\n"
      "   (none)\n"
      "   )\n"
      "  )";
  test_final_function(func, type, expected);
}

// weird short circuit thing
TEST_F(FormRegressionTestJak1, WeirdShortCircuit) {
  std::string func =
      "sll r0, r0, 0\n"
      "   daddiu sp, sp, -144\n"
      "    sd ra, 0(sp)\n"
      "    sq s4, 96(sp)\n"
      "    sq s5, 112(sp)\n"
      "    sq gp, 128(sp)\n"

      "    or gp, a1, r0\n"
      "    or v1, a0, r0\n"
      "    lwu s4, 4(v1)\n"
      "    or s5, s7, r0\n"  // s5 = result
      "    beq r0, r0, L43\n"
      "    sll r0, r0, 0\n"

      "L40:\n"
      "    lwu v1, 20(s4)\n"
      "    lwu a0, 12(v1)\n"
      "    beq s7, a0, L42\n"
      "    or v1, s7, r0\n"

      "    daddiu a1, sp, 16\n"
      "    sw s6, 4(a1)\n"
      "    sw r0, 8(a1)\n"
      "    sw gp, 12(a1)\n"
      "    lw t9, send-event-function(s7)\n"
      "    jalr ra, t9\n"
      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    bnel s7, v1, L41\n"

      "    or s5, v1, r0\n"

      // there's nothing here!

      "L41:\n"
      "    or v1, s5, r0\n"

      "L42:\n"
      "    lw t9, entity-actor-lookup(s7)\n"
      "    daddiu a1, s7, next-actor\n"
      "    addiu a2, r0, 0\n"
      "    or a0, s4, r0\n"
      "    jalr ra, t9\n"

      "    sll v0, ra, 0\n"

      "    or v1, v0, r0\n"
      "    or s4, v1, r0\n"

      "L43:\n"
      "    bne s7, s4, L40\n"
      "    sll r0, r0, 0\n"

      "    or v1, s7, r0\n"
      "    or v0, s5, r0\n"
      "    ld ra, 0(sp)\n"
      "    lq gp, 128(sp)\n"
      "    lq s5, 112(sp)\n"
      "    lq s4, 96(sp)\n"
      "    jr ra\n"
      "    daddiu sp, sp, 144";
  std::string type = "(function actor-link-info symbol object)";
  std::string expected =
      "(let ((s4-0 (-> arg0 next))\n"
      "     (s5-0 (the-as object #f))\n"
      "     )\n"
      "  (while s4-0\n"
      "   (let ((a0-1 (-> s4-0 extra process)))\n"
      "    (when a0-1\n"
      "     (let ((a1-1 (new 'stack-no-clear 'event-message-block)))\n"
      "      (set! (-> a1-1 from) pp)\n"
      "      (set! (-> a1-1 num-params) 0)\n"
      "      (set! (-> a1-1 message) arg1)\n"
      "      (set! s5-0 (or (send-event-function a0-1 a1-1) s5-0))\n"
      "      )\n"
      "     )\n"
      "    )\n"
      "   (set! s4-0 (entity-actor-lookup s4-0 'next-actor 0))\n"
      "   )\n"
      "  s5-0\n"
      "  )";
  test_with_stack_structures(func, type, expected, "[[16, \"event-message-block\"]]");
}

TEST_F(FormRegressionTestJak1, WeirdShortCircuit2) {
  std::string func =
      "sll r0, r0, 0\n"
      "L62:\n"
      "    lwu v1, 8(a0)\n"
      "    beql s7, v1, L63\n"

      "    or v0, v1, r0\n"

      "    lwu v1, 8(a0)\n"  // here's the case
      "    lwu v1, 20(v1)\n"
      "    lwu v0, 12(v1)\n"

      "L63:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function actor-link-info object)";
  std::string expected = "(the-as object (and (-> arg0 prev) (-> arg0 prev extra process)))";
  test_with_stack_structures(func, type, expected, "[[16, \"event-message-block\"]]");
}
