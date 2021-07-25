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
  test_final_function(func, type, expected);
}