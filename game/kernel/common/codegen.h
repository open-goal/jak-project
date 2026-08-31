#pragma once

/*!
 * @file codegen.h
 * Helpers for the small machine code stubs the kernel writes at runtime.
 */

#include <cstring>

#include "common/arm64/encoding.h"
#include "common/common_types.h"

#include "game/runtime.h"

#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#endif

#ifdef __SWITCH__
// Deliberately not #include <switch.h> (or even just switch/arm/cache.h) here: it transitively
// includes switch/types.h, which typedefs u128 to __uint128_t, conflicting with the struct u128
// this project already defines in common/common_types.h (included by nearly everything). The
// real symbols still link fine from libnx against a matching extern "C" prototype, so just
// declare the two we need instead of pulling in the whole header.
extern "C" {
void armDCacheFlush(void* addr, size_t size);
void armICacheInvalidate(void* addr, size_t size);
}
#endif

/*!
 * Makes freshly written instructions visible to the CPU.
 */
inline void flush_icache(void* addr, int size) {
#if defined(__SWITCH__)
  // Horizon's JIT memory model doesn't allow raw DC/IC cache-maintenance instructions
  // (__builtin___clear_cache) on Jit-mapped pages -- confirmed by the pre-existing TODO in
  // runtime.cpp about needing armDCacheFlush/armICacheInvalidate here. Using the raw builtin traps
  // instead of just being a no-op, and whatever handles that trap hangs rather than crashing, which
  // is why every kernel-boot function that JITs a C trampoline (make_function_from_c and friends)
  // was stalling forever the first time it tried to flush icache for freshly written code.
  armDCacheFlush(addr, (size_t)size);
  armICacheInvalidate(addr, (size_t)size);
#elif defined(__aarch64__)
#ifdef __APPLE__
  sys_icache_invalidate(addr, size);
#else
  __builtin___clear_cache((char*)addr, (char*)addr + size);
#endif
#else
  (void)addr;
  (void)size;
#endif
}

/*!
 * Makes EE instructions visible through their executable mapping.
 */
inline void flush_icache_goal(u32 goal_addr, u32 size) {
  flush_icache(g_ee_main_mem_exec + goal_addr, (int)size);
}

#ifdef __aarch64__

/*!
 * Emits a fixed movz/movk sequence for a 64-bit value.
 */
inline int emit_arm64_mov64(u8* dst, u32 reg, u64 val) {
  u32 instr = arm64::encode_movz_64(reg, u16(val & 0xffff), 0);
  memcpy(dst, &instr, 4);
  int offset = 4;
  for (u32 halfword = 1; halfword < 4; halfword++) {
    instr = arm64::encode_movk_64(reg, u16((val >> (halfword * 16)) & 0xffff), halfword);
    memcpy(dst + offset, &instr, 4);
    offset += 4;
  }
  return offset;
}

/*!
 * Emits a GOAL-to-C stub and returns its size in bytes.
 */
inline int emit_arm64_c_stub(u8* dst, u64 target, u64 trampoline, bool arg3_is_pp) {
  int offset = emit_arm64_mov64(dst, 8, target);

  if (arg3_is_pp) {
    // pp goes from x20 to C argument x3
    u32 instr = 0xaa0003e0 | (20 << 16) | 3;
    memcpy(dst + offset, &instr, 4);
    offset += 4;
  }

  offset += emit_arm64_mov64(dst + offset, 9, trampoline);

  u32 br = 0xd61f0000 | (9 << 5);  // br x9
  memcpy(dst + offset, &br, 4);
  offset += 4;

  return offset;
}

#endif

/*!
 * Emits the host return stub used by GOAL `nothing`.
 */
inline int emit_return_stub(u8* dst) {
#ifdef __aarch64__
  const u32 instr = 0xd65f03c0;  // ret
  memcpy(dst, &instr, 4);
  return 4;
#else
  dst[0] = 0xc3;  // ret
  return 1;
#endif
}

/*!
 * Emits a GOAL function that returns zero.
 */
inline int emit_zero_stub(u8* dst) {
#ifdef __aarch64__
  // movz x0, #0 followed by ret
  const u32 instrs[2] = {arm64::encode_movz_64(0, 0, 0), 0xd65f03c0};
  memcpy(dst, instrs, 8);
  return 8;
#else
  dst[0] = 0x31;  // xor eax, eax
  dst[1] = 0xc0;
  dst[2] = 0xc3;  // ret
  return 3;
#endif
}
