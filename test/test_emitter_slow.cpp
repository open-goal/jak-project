/*!
 * @file test_emitter_slow.cpp
 * Tests for the emitter which take over 1 second. (Checking 10,000's of functions).
 *
 * It may make sense to exclude these tests when developing to save time.
 */

#include "gtest/gtest.h"
#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
//
using namespace emitter;



TEST(EmitterSlow, xmm32_move) {
  std::vector<u32> u32_constants = {0, INT32_MAX, UINT32_MAX, 17};

  // test moving between xmms (32-bit) and gprs.
  CodeTester tester;
  tester.init_code_buffer(512);

  for (auto constant : u32_constants) {
    for (int r1 = 0; r1 < 16; r1++) {
      if (r1 == RSP) {
        continue;
      }
      for (int r2 = 0; r2 < 16; r2++) {
        if (r2 == RSP) {
          continue;
        }
        for (int r3 = 0; r3 < 16; r3++) {
          for (int r4 = 0; r4 < 16; r4++) {
            tester.clear();
            tester.emit_push_all_xmms();
            tester.emit_push_all_gprs(true);
            // move constant to gpr
            tester.emit(IGen::mov_gpr64_u32(r1, constant));
            // move gpr to xmm
            tester.emit(IGen::movd_xmm32_gpr32(XMM0 + r3, r1));
            // move xmm to xmm
            tester.emit(IGen::mov_xmm32_xmm32(XMM0 + r4, XMM0 + r3));
            // move xmm to gpr
            tester.emit(IGen::movd_gpr32_xmm32(r2, XMM0 + r4));
            // return!
            tester.emit(IGen::mov_gpr64_gpr64(RAX, r2));
            tester.emit_pop_all_gprs(true);
            tester.emit_pop_all_xmms();
            tester.emit_return();
          }
        }
      }
    }
  }
}
