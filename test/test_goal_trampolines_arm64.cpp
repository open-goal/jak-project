#include <array>
#include <cstddef>
#include <vector>

#include "common/common_types.h"

#include "gtest/gtest.h"

#if defined(__aarch64__)

#include "game/kernel/common/codegen.h"

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <sys/mman.h>
#endif

extern "C" {
#ifdef __APPLE__
u64 _call_goal_asm_arm64(u64 a0,
                         u64 a1,
                         u64 a2,
                         void* fptr,
                         void* st_ptr,
                         void* offset,
                         void* exec_offset) asm("_call_goal_asm_arm64");
u64 _call_goal_on_stack_asm_arm64(u64 rsp,
                                  u64 u0,
                                  u64 u1,
                                  void* fptr,
                                  void* st_ptr,
                                  void* offset,
                                  void* exec_offset) asm("_call_goal_on_stack_asm_arm64");
u64 _call_goal8_asm_arm64(void* func,
                          u64* arg_array,
                          u64 zero,
                          u64 pp,
                          u64 st,
                          void* offset,
                          void* exec_offset) asm("_call_goal8_asm_arm64");
#else
u64 _call_goal_asm_arm64(u64 a0,
                         u64 a1,
                         u64 a2,
                         void* fptr,
                         void* st_ptr,
                         void* offset,
                         void* exec_offset);
u64 _call_goal_on_stack_asm_arm64(u64 rsp,
                                  u64 u0,
                                  u64 u1,
                                  void* fptr,
                                  void* st_ptr,
                                  void* offset,
                                  void* exec_offset);
u64 _call_goal8_asm_arm64(void* func,
                          u64* arg_array,
                          u64 zero,
                          u64 pp,
                          u64 st,
                          void* offset,
                          void* exec_offset);
#endif
}

struct alignas(16) Arm64SavedRegisters {
  u64 result = 0;
  u64 x20 = 0;
  u64 x21 = 0;
  u64 x22 = 0;
  u64 x27 = 0;
  u64 sp = 0;
  std::array<std::array<u64, 2>, 8> vectors = {};
};

static_assert(offsetof(Arm64SavedRegisters, vectors) == 48);
static_assert(sizeof(Arm64SavedRegisters) == 176);

extern "C" u64 arm64_test_clobber_goal_registers();
extern "C" u64 arm64_test_preservation_probe(Arm64SavedRegisters* out,
                                             u64 callee,
                                             u64 data,
                                             u64 mode);

#ifdef __APPLE__
#define ARM64_TEST_SYMBOL(name) "_" #name
#else
#define ARM64_TEST_SYMBOL(name) #name
#endif

asm(".text\n"
    ".p2align 2\n"
    ".globl " ARM64_TEST_SYMBOL(arm64_test_clobber_goal_registers) "\n"
    ARM64_TEST_SYMBOL(arm64_test_clobber_goal_registers) ":\n"
    "movi v8.16b, #0x88\n"
    "movi v9.16b, #0x89\n"
    "movi v10.16b, #0x8a\n"
    "movi v11.16b, #0x8b\n"
    "movi v12.16b, #0x8c\n"
    "movi v13.16b, #0x8d\n"
    "movi v14.16b, #0x8e\n"
    "movi v15.16b, #0x8f\n"
    "mov x20, #0xaaaa\n"
    "mov x21, #0xbbbb\n"
    "mov x22, #0xcccc\n"
    "mov x27, #0xdddd\n"
    "mov x0, #42\n"
    "ret\n"
    ".p2align 2\n"
    ".globl " ARM64_TEST_SYMBOL(arm64_test_preservation_probe) "\n"
    ARM64_TEST_SYMBOL(arm64_test_preservation_probe) ":\n"
    "stp x29, x30, [sp, #-16]!\n"
    "mov x29, sp\n"
    "stp x20, x21, [sp, #-16]!\n"
    "stp x22, x27, [sp, #-16]!\n"
    "sub sp, sp, #128\n"
    "stp q8, q9, [sp]\n"
    "stp q10, q11, [sp, #32]\n"
    "stp q12, q13, [sp, #64]\n"
    "stp q14, q15, [sp, #96]\n"
    "sub sp, sp, #32\n"
    "stp x0, x1, [sp]\n"
    "stp x2, x3, [sp, #16]\n"
    "mov x20, #0x2020\n"
    "mov x21, #0x2121\n"
    "mov x22, #0x2222\n"
    "mov x27, #0x2727\n"
    "movi v8.16b, #0x18\n"
    "movi v9.16b, #0x19\n"
    "movi v10.16b, #0x1a\n"
    "movi v11.16b, #0x1b\n"
    "movi v12.16b, #0x1c\n"
    "movi v13.16b, #0x1d\n"
    "movi v14.16b, #0x1e\n"
    "movi v15.16b, #0x1f\n"
    "ldr x11, [sp, #24]\n"
    "cmp x11, #1\n"
    "b.eq 1f\n"
    "cmp x11, #2\n"
    "b.eq 2f\n"
    "mov x0, #7\n"
    "mov x1, xzr\n"
    "mov x2, xzr\n"
    "ldr x3, [sp, #8]\n"
    "mov x4, #0x1111\n"
    "mov x5, #0x3333\n"
    "mov x6, #0x4444\n"
    "bl _call_goal_asm_arm64\n"
    "b 3f\n"
    "1:\n"
    "ldr x0, [sp, #16]\n"
    "mov x1, xzr\n"
    "mov x2, xzr\n"
    "ldr x3, [sp, #8]\n"
    "mov x4, #0x1111\n"
    "mov x5, #0x3333\n"
    "mov x6, #0x4444\n"
    "bl _call_goal_on_stack_asm_arm64\n"
    "b 3f\n"
    "2:\n"
    "ldr x0, [sp, #8]\n"
    "ldr x1, [sp, #16]\n"
    "mov x2, xzr\n"
    "mov x3, #0x1111\n"
    "mov x4, #0x2222\n"
    "mov x5, #0x3333\n"
    "mov x6, #0x4444\n"
    "bl _call_goal8_asm_arm64\n"
    "3:\n"
    "ldr x9, [sp]\n"
    "str x0, [x9]\n"
    "str x20, [x9, #8]\n"
    "str x21, [x9, #16]\n"
    "str x22, [x9, #24]\n"
    "str x27, [x9, #32]\n"
    "mov x10, sp\n"
    "str x10, [x9, #40]\n"
    "stp q8, q9, [x9, #48]\n"
    "stp q10, q11, [x9, #80]\n"
    "stp q12, q13, [x9, #112]\n"
    "stp q14, q15, [x9, #144]\n"
    "ldp q8, q9, [sp, #32]\n"
    "ldp q10, q11, [sp, #64]\n"
    "ldp q12, q13, [sp, #96]\n"
    "ldp q14, q15, [sp, #128]\n"
    "add sp, sp, #160\n"
    "ldp x22, x27, [sp], #16\n"
    "ldp x20, x21, [sp], #16\n"
    "ldp x29, x30, [sp], #16\n"
    "ret\n");

#undef ARM64_TEST_SYMBOL

namespace {
u64 g_callee_sp = 0;
u64 g_callee_arg0 = 0;
u64 g_callee_offset = 0;
u64 g_callee_exec_base = 0;

// AAPCS64 callee used by the trampoline tests
extern "C" u64 arm64_trampoline_test_callee(u64 a0);
extern "C" u64 arm64_trampoline_test_callee(u64 a0) {
  u64 sp, offset, exec_base;
  // writable and executable GOAL bases
  asm volatile("mov %0, x22\n\tmov %1, x27\n\tmov %2, sp"
               : "=r"(offset), "=r"(exec_base), "=r"(sp));
  g_callee_sp = sp;
  g_callee_offset = offset;
  g_callee_exec_base = exec_base;
  g_callee_arg0 = a0;
  return 42;
}
}  // namespace

TEST(ARM64Trampolines, call_goal) {
  g_callee_sp = 0;
  g_callee_arg0 = 0;
  u64 st = 0x1111, off = 0x2222;
  u64 exec = 0x3333;
  u64 result = _call_goal_asm_arm64(7, 0, 0, (void*)arm64_trampoline_test_callee, (void*)st,
                                    (void*)off, (void*)exec);
  EXPECT_EQ(result, 42u);
  EXPECT_EQ(g_callee_arg0, 7u);
  EXPECT_EQ(g_callee_offset, off);
  // x27 receives the executable base
  EXPECT_EQ(g_callee_exec_base, exec);
}

TEST(ARM64Trampolines, call_goal_on_stack) {
  const size_t kStackSize = 64 * 1024;
  std::vector<u8> goal_stack(kStackSize, 0);
  u64 stack_top = (u64)(goal_stack.data() + kStackSize) & ~u64(15);

  g_callee_sp = 0;
  u64 my_sp;
  asm volatile("mov %0, sp" : "=r"(my_sp));

  u64 result = _call_goal_on_stack_asm_arm64(stack_top, 0, 0, (void*)arm64_trampoline_test_callee,
                                             (void*)0x1111, (void*)0x2222, (void*)0x3333);
  EXPECT_EQ(result, 42u);
  EXPECT_EQ(g_callee_offset, 0x2222u);
  EXPECT_EQ(g_callee_exec_base, 0x3333u);
  // callee runs on the supplied stack
  EXPECT_GT(g_callee_sp, (u64)goal_stack.data());
  EXPECT_LE(g_callee_sp, stack_top);

  // caller stack is restored
  u64 sp_after;
  asm volatile("mov %0, sp" : "=r"(sp_after));
  EXPECT_EQ(sp_after, my_sp);
}

TEST(ARM64Trampolines, preserves_native_saved_registers) {
  const size_t kStackSize = 64 * 1024;
  std::vector<u8> goal_stack(kStackSize, 0);
  u64 stack_top = (u64)(goal_stack.data() + kStackSize) & ~u64(15);
  std::array<u64, 8> args = {1, 2, 3, 4, 5, 6, 7, 8};

  for (u64 mode = 0; mode < 3; mode++) {
    SCOPED_TRACE(mode);
    u64 data = 0;
    if (mode == 1) {
      data = stack_top;
    } else if (mode == 2) {
      data = (u64)args.data();
    }

    Arm64SavedRegisters saved;
    arm64_test_preservation_probe(&saved, (u64)arm64_test_clobber_goal_registers, data, mode);

    EXPECT_EQ(saved.result, 42u);
    EXPECT_EQ(saved.x20, 0x2020u);
    EXPECT_EQ(saved.x21, 0x2121u);
    EXPECT_EQ(saved.x22, 0x2222u);
    EXPECT_EQ(saved.x27, 0x2727u);
    EXPECT_EQ(saved.sp & 15, 0u);
    for (u64 i = 0; i < saved.vectors.size(); i++) {
      u64 expected = (0x18 + i) * 0x0101010101010101ull;
      EXPECT_EQ(saved.vectors[i][0], expected) << "v" << i + 8 << " low";
      EXPECT_EQ(saved.vectors[i][1], expected) << "v" << i + 8 << " high";
    }
  }
}

TEST(ARM64CStub, pp_argument) {
  u8 buf[64] = {};
  int len = emit_arm64_c_stub(buf, 0x1122334455667788ull, 0x99aabbccddeeff00ull, true);

  ASSERT_EQ(len, 10 * 4);

  u32 words[10];
  memcpy(words, buf, sizeof(words));
  EXPECT_EQ(words[4], 0xaa1403e3u) << "mov x3, x20";
  EXPECT_NE(words[4], 0xaa1403e2u) << "x2 is the third AAPCS64 argument register";
  EXPECT_EQ(words[9], 0xd61f0120u) << "br x9";

  // arg3_is_pp false leaves x2 alone
  u8 buf2[64] = {};
  int len2 = emit_arm64_c_stub(buf2, 0x1122334455667788ull, 0x99aabbccddeeff00ull, false);
  EXPECT_EQ(len2, 9 * 4);
}

#ifdef __APPLE__

TEST(ARM64ICache, icache_invalidation) {
  const size_t kSize = 64 * 1024;
  u8* rw = (u8*)mmap(nullptr, kSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  ASSERT_NE(rw, MAP_FAILED);

  mach_vm_address_t exec_addr = 0;
  vm_prot_t cur_prot = 0, max_prot = 0;
  ASSERT_EQ(
      mach_vm_remap(mach_task_self(), &exec_addr, kSize, 0, VM_FLAGS_ANYWHERE, mach_task_self(),
                    (mach_vm_address_t)rw, false, &cur_prot, &max_prot, VM_INHERIT_NONE),
      KERN_SUCCESS);
  ASSERT_EQ(
      mach_vm_protect(mach_task_self(), exec_addr, kSize, false, VM_PROT_READ | VM_PROT_EXECUTE),
      KERN_SUCCESS);

  auto write_ret_const = [](u32* p, u32 imm) {
    p[0] = 0x52800000u | (imm << 5);  // movz w0, #imm
    p[1] = 0xd65f03c0u;               // ret
  };

  int stale_without = 0, stale_with = 0, trials = 0;
  // one generated function per page
  for (size_t off = 0; off + 4096 < kSize; off += 4096) {
    auto* words = (u32*)(rw + off);
    auto fn = (int (*)())(void*)((u8*)exec_addr + off);

    write_ret_const(words, 11);
    flush_icache((void*)fn, 8);
    for (int i = 0; i < 100; i++) {
      ASSERT_EQ(fn(), 11);  // prime the instruction cache
    }

    write_ret_const(words, 22);
    // executable view sees the write before the flush
    ASSERT_EQ(*(volatile u32*)((u8*)exec_addr + off), words[0]);
    if (fn() != 22) {
      stale_without++;
    }

    flush_icache((void*)fn, 8);
    if (fn() != 22) {
      stale_with++;
    }
    trials++;
  }

  EXPECT_GT(trials, 0);
  EXPECT_EQ(stale_with, 0) << "rewritten code stayed stale after the cache flush";
  EXPECT_GT(stale_without, 0) << "expected stale code without a cache flush";
  munmap(rw, kSize);
}

#endif  // __APPLE__

#endif
