#include "gtest/gtest.h"
#include "FormRegressionTest.h"

using namespace decompiler;

// vector-rad<-vector-deg/2!
TEST_F(FormRegressionTest, VectorDegToVectorRad) {
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