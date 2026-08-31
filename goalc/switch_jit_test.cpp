/*!
 * @file switch_jit_test.cpp
 * Minimal sanity check for real Switch hardware: JIT a trivial ARM64 function through the
 * existing GOAL emitter (IGenARM64 + CodeTester), run it via libnx's Jit API, and check the
 * result. This isolates "does JIT'd code actually execute correctly on Horizon OS" from
 * everything else in the runtime -- it's the single biggest unproven risk in this whole port.
 */

#include <cstdio>

#if defined(__SWITCH__)
// See CodeTester.h -- avoids a struct-u128 vs typedef-u128 collision with libnx.
#define u128 nx_u128
#include <switch.h>
#undef u128
#endif

#include "common/arm64/encoding.h"

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "goalc/emitter/InstructionSet.h"

// Emits a fixed 4-instruction (16-byte) movz/movk sequence loading a 64-bit absolute address,
// matching exactly what the real GOAL->native trampolines use for loading function pointers.
static void emit_mov64_fixed(emitter::CodeTester& t, u32 reg, u64 val) {
  t.emit_data<u32>(arm64::encode_movz_64(reg, u16(val & 0xffff), 0));
  t.emit_data<u32>(arm64::encode_movk_64(reg, u16((val >> 16) & 0xffff), 1));
  t.emit_data<u32>(arm64::encode_movk_64(reg, u16((val >> 32) & 0xffff), 2));
  t.emit_data<u32>(arm64::encode_movk_64(reg, u16((val >> 48) & 0xffff), 3));
}

int main() {
#if defined(__SWITCH__)
  consoleInit(NULL);
#endif

  bool pass = false;
  u64 result = 0;
  const char* stage = "setup";

  try {
    stage = "emit";
    emitter::CodeTester tester(emitter::InstructionSet::ARM64);
    tester.init_code_buffer(4096);
    tester.emit(emitter::IGen::mov_gpr64_u32(tester.generator(), emitter::X0, 42));
    tester.emit_return();

    stage = "execute";
    result = tester.execute();
    pass = (result == 42);
  } catch (const std::exception& e) {
    printf("[%s] exception: %s\n", stage, e.what());
  } catch (...) {
    printf("[%s] unknown exception\n", stage);
  }

  printf("jak-project ARM64 JIT execution test\n");
  printf("result = %llu (expected 42)\n", (unsigned long long)result);
  printf(pass ? "PASS\n" : "FAIL\n");

  // Test 2: reproduce, in isolation, the exact 3-instruction sequence from gravity-h.o's
  // top-level (str x30,[sp,#-16]! / ldr x30,[sp],#16 / ret) that hangs/crashes during real GOAL
  // kernel boot. If this alone reproduces the crash, the bug is in dynarmic's JIT translation of
  // single-register pre/post-indexed STR/LDR through X30, not in anything GOAL-runtime-specific
  // -- a working mov (test 1 above) plus a paired stp/ldp x30,x19 (used by math.o, which loads
  // fine) already rules out a blanket "JIT execution is broken" or "X30 can never round-trip"
  // explanation.
  bool pass2 = false;
  u64 result2 = 0;
  const char* stage2 = "setup2";
  try {
    stage2 = "emit2";
    emitter::CodeTester tester2(emitter::InstructionSet::ARM64);
    tester2.init_code_buffer(4096);
    // marker value so we can tell the function ran to completion and returned correctly
    tester2.emit(emitter::IGen::mov_gpr64_u32(tester2.generator(), emitter::X0, 0x1234));
    // raw bytes copied verbatim from gravity-h.o's compiled top-level segment
    tester2.emit_data<u32>(0xf81f0ffe);  // str x30, [sp, #-16]!
    tester2.emit_data<u32>(0xf84107fe);  // ldr x30, [sp], #16
    tester2.emit_data<u32>(0xd65f03c0);  // ret

    stage2 = "execute2";
    result2 = tester2.execute();
    pass2 = (result2 == 0x1234);
  } catch (const std::exception& e) {
    printf("[%s] exception: %s\n", stage2, e.what());
  } catch (...) {
    printf("[%s] unknown exception\n", stage2);
  }

  printf("gravity-h repro test\n");
  printf("result2 = %llu (expected %d = 0x1234)\n", (unsigned long long)result2, 0x1234);
  printf(pass2 ? "PASS2\n" : "FAIL2\n");

#if defined(__SWITCH__)
  // Write results after test 2 too, in case test 3 crashes the whole process before the final
  // write below can run -- if that happens, this file having stage3=not-run but 1/2 present is
  // itself the evidence that test 3 is what triggers it.
  {
    FILE* f = fopen("sdmc:/switch_jit_test_result.txt", "w");
    if (f) {
      fprintf(f, "stage=%s\nresult=%llu\nexpected=42\n%s\nstage2=%s\nresult2=%llu\nexpected2=4660\n%s\nstage3=not_run\n",
              stage, (unsigned long long)result, pass ? "PASS" : "FAIL", stage2,
              (unsigned long long)result2, pass2 ? "PASS2" : "FAIL2");
      fclose(f);
    }
  }
#endif

  // Test 3: same gravity-h instruction sequence as test 2, but this time called through the
  // exact same stack-switching pattern _call_goal_on_stack_asm_arm64 uses for every real GOAL
  // call (mov x9,sp / mov sp,x0 / str x9,[sp,#-16]! ... blr x3 ... ldr x9,[sp],#16 / mov sp,x9).
  // Test 2 called gravity-h's code on CodeTester's own native stack via a plain call -- it never
  // exercised the stack switch itself. If test 3 fails where test 2 passed, the bug is
  // specifically in dynarmic's handling of str/ldr through X30 immediately after an SP switch
  // via `mov sp, xN`, not in the instructions alone.
  bool pass3 = false;
  u64 result3 = 0;
  const char* stage3 = "setup3";
  try {
    stage3 = "emit3";
    emitter::CodeTester tester3(emitter::InstructionSet::ARM64);
    tester3.init_code_buffer(4096);

    // scratch "goal stack" -- just needs to be valid, writable, 16-byte-aligned memory.
    alignas(16) static u8 fake_goal_stack[1024];
    u64 fake_stack_top = (u64)(fake_goal_stack + sizeof(fake_goal_stack));

    // Simplified/inlined version -- no separate blr'd function, no address patching, to rule out
    // a bug in the test harness itself. X30 is already a valid return address here (CodeTester's
    // own `((u64(*)())code_buffer_rx)()` call set it via a real call instruction) -- str/ldr it
    // through the switched-to goal stack exactly like gravity-h does, then switch back and ret
    // using that same X30. If this hangs too, it's the raw sp-switch + str/ldr(x30) mechanic.
    emit_mov64_fixed(tester3, 0 /*x0*/, fake_stack_top);
    tester3.emit_data<u32>(0x910003e9);  // mov x9, sp
    tester3.emit_data<u32>(0x9100001f);  // mov sp, x0
    tester3.emit_data<u32>(0xf81f0fe9);  // str x9, [sp, #-16]!   (trampoline's own save)
    tester3.emit_data<u32>(0xf81f0ffe);  // str x30, [sp, #-16]!  (gravity-h's str)
    tester3.emit_data<u32>(0xf84107fe);  // ldr x30, [sp], #16    (gravity-h's ldr)
    tester3.emit_data<u32>(0xf84107e9);  // ldr x9, [sp], #16     (trampoline's own restore)
    tester3.emit_data<u32>(0x9100013f);  // mov sp, x9
    emit_mov64_fixed(tester3, 0 /*x0*/, 0x5678);
    tester3.emit_data<u32>(0xd65f03c0);  // ret

    stage3 = "execute3";
    result3 = tester3.execute();
    pass3 = (result3 == 0x5678);
  } catch (const std::exception& e) {
    printf("[%s] exception: %s\n", stage3, e.what());
  } catch (...) {
    printf("[%s] unknown exception\n", stage3);
  }

  printf("gravity-h + stack-switch repro test\n");
  printf("result3 = %llu (expected %d = 0x5678)\n", (unsigned long long)result3, 0x5678);
  printf(pass3 ? "PASS3\n" : "FAIL3\n");

#if defined(__SWITCH__)
  {
    FILE* f = fopen("sdmc:/switch_jit_test_result.txt", "w");
    if (f) {
      fprintf(f,
              "stage=%s\nresult=%llu\nexpected=42\n%s\nstage2=%s\nresult2=%llu\nexpected2=4660\n%s\n"
              "stage3=%s\nresult3=%llu\nexpected3=22136\n%s\nstage4=not_run\n",
              stage, (unsigned long long)result, pass ? "PASS" : "FAIL", stage2,
              (unsigned long long)result2, pass2 ? "PASS2" : "FAIL2", stage3,
              (unsigned long long)result3, pass3 ? "PASS3" : "FAIL3");
      fclose(f);
    }
  }
#endif

  // Test 4: same as test 3 (full trampoline preamble/epilogue), but calling gravity-h via a real
  // `blr x3` to a genuinely, analytically computed absolute address (code_address() is known
  // before any emission, so the target offset is computed by hand, not patched at runtime --
  // test 3's original blr version used runtime patching via write(), which turned out to be
  // buggy in the test harness itself, not dynarmic; this isolates the *real* remaining variable:
  // an indirect call to a *separate* code address after the sp switch, as the real trampoline
  // does, vs test 3's fully inlined body).
  bool pass4 = false;
  u64 result4 = 0;
  const char* stage4 = "setup4";
  try {
    stage4 = "emit4";
    emitter::CodeTester tester4(emitter::InstructionSet::ARM64);
    tester4.init_code_buffer(4096);
    u64 base = tester4.code_address();

    alignas(16) static u8 fake_goal_stack4[1024];
    u64 fake_stack_top4 = (u64)(fake_goal_stack4 + sizeof(fake_goal_stack4));

    // FOUND IT (in the test harness, not dynarmic): the previous version of this test never
    // preserved its OWN x30 (the return address into CodeTester::execute()) across the nested
    // bl/blr -- bl overwrites x30 with the return-into-trampoline address, mock_gravity_h's own
    // str/ldr(x30) just round-trips THAT value, and the final `ret` then jumps back into the
    // middle of this function (the ldr x9 epilogue instruction) instead of returning, looping
    // forever and draining sp on every iteration until it reads unmapped memory -- exactly the
    // observed hang. The real trampoline never has this problem because it has a genuine
    // stp/ldp x29,x30 prologue/epilogue on the NATIVE stack around the whole thing. Add that
    // here too, matching the real trampoline exactly (see _call_goal_on_stack_asm_arm64).
    //   0  : stp x29, x30, [sp, #-16]!     (4)
    //   4  : mov x0, #fake_stack_top4      (16 bytes)
    //   20 : mov x9, sp                    (4)
    //   24 : mov sp, x0                    (4)
    //   28 : str x9, [sp, #-16]!           (4)
    //   32 : bl <mock_gravity_h>           (4)
    //   36 : ldr x9, [sp], #16             (4)
    //   40 : mov sp, x9                    (4)
    //   44 : mov x0, #0x9abc               (16 bytes)
    //   60 : ldp x29, x30, [sp], #16       (4)
    //   64 : ret                           (4)
    //   68 : mock_gravity_h: str x30,[sp,#-16]! / ldr x30,[sp],#16 / ret
    (void)base;
    const u32 bl_offset = 32;
    const u32 mock_gravity_h_offset = 68;
    const u32 bl_imm26 = (mock_gravity_h_offset - bl_offset) / 4;

    tester4.emit_data<u32>(0xa9bf7bfd);  // stp x29, x30, [sp, #-16]!
    emit_mov64_fixed(tester4, 0 /*x0*/, fake_stack_top4);
    tester4.emit_data<u32>(0x910003e9);  // mov x9, sp
    tester4.emit_data<u32>(0x9100001f);  // mov sp, x0
    tester4.emit_data<u32>(0xf81f0fe9);  // str x9, [sp, #-16]!
    tester4.emit_data<u32>(0x94000000 | bl_imm26);  // bl mock_gravity_h
    tester4.emit_data<u32>(0xf84107e9);  // ldr x9, [sp], #16
    tester4.emit_data<u32>(0x9100013f);  // mov sp, x9
    emit_mov64_fixed(tester4, 0 /*x0*/, 0x9abc);
    tester4.emit_data<u32>(0xa8c17bfd);  // ldp x29, x30, [sp], #16
    tester4.emit_data<u32>(0xd65f03c0);  // ret

    // sanity-check our hand-computed layout actually matches before relying on it
    if (tester4.size() != (int)mock_gravity_h_offset) {
      char err[128];
      snprintf(err, sizeof(err), "layout mismatch: expected offset %u, got %d",
               mock_gravity_h_offset, tester4.size());
      throw std::runtime_error(err);
    }

    tester4.emit_data<u32>(0xf81f0ffe);  // str x30, [sp, #-16]!
    tester4.emit_data<u32>(0xf84107fe);  // ldr x30, [sp], #16
    tester4.emit_data<u32>(0xd65f03c0);  // ret

    stage4 = "execute4";
    result4 = tester4.execute();
    pass4 = (result4 == 0x9abc);
  } catch (const std::exception& e) {
    printf("[%s] exception: %s\n", stage4, e.what());
  } catch (...) {
    printf("[%s] unknown exception\n", stage4);
  }

  printf("gravity-h + stack-switch + real blr repro test\n");
  printf("result4 = %llu (expected %d = 0x9abc)\n", (unsigned long long)result4, 0x9abc);
  printf(pass4 ? "PASS4\n" : "FAIL4\n");

#if defined(__SWITCH__)
  // Also write the result to the SD card -- there's no way to read the on-screen console
  // output back out of an emulator/headless run, but sdmc:/ is a real host-visible file.
  FILE* f = fopen("sdmc:/switch_jit_test_result.txt", "w");
  if (f) {
    fprintf(f,
            "stage=%s\nresult=%llu\nexpected=42\n%s\nstage2=%s\nresult2=%llu\nexpected2=4660\n%s\n"
            "stage3=%s\nresult3=%llu\nexpected3=22136\n%s\nstage4=%s\nresult4=%llu\nexpected4=39612\n%s\n",
            stage, (unsigned long long)result, pass ? "PASS" : "FAIL", stage2,
            (unsigned long long)result2, pass2 ? "PASS2" : "FAIL2", stage3,
            (unsigned long long)result3, pass3 ? "PASS3" : "FAIL3", stage4,
            (unsigned long long)result4, pass4 ? "PASS4" : "FAIL4");
    fclose(f);
  }

  consoleUpdate(NULL);

  PadState pad;
  padConfigureInput(1, HidNpadStyleSet_NpadStandard);
  padInitializeDefault(&pad);
  // Auto-exit after ~3 seconds (180 frames) so this can run unattended in an emulator; still
  // exits early on + if run interactively.
  for (int frame = 0; frame < 180 && appletMainLoop(); frame++) {
    padUpdate(&pad);
    if (padGetButtonsDown(&pad) & HidNpadButton_Plus) {
      break;
    }
    consoleUpdate(NULL);
  }
  consoleExit(NULL);
#endif

  return pass ? 0 : 1;
}
