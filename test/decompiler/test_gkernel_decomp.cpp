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