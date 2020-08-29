/*!
 * @file registers.h
 * Definitions and conventions for x86-64 registers.
 */

#ifndef JAK1_REGISTERS_H
#define JAK1_REGISTERS_H

#include "common/common_types.h"

namespace goal {
enum X86R : u8 {
  RAX,  // return, temp
  RCX,  // arg 3
  RDX,  // arg 2
  RBX,  // X saved

  RSP,  // stack pointer
  RBP,  // X base pointer (like fp)
  RSI,  // arg 1
  RDI,  // arg 0

  R8,   // arg 4
  R9,   // arg 5, saved
  R10,  // arg 6, saved (arg in GOAL only)
  R11,  // arg 7, saved (arg in GOAL only)
  R12,  // X saved - pp register (like s6)
  R13,  // X saved - function call register (like t9)
  R14,  // X saved - offset (added in GOAL x86)
  R15,  // X saved - st (like s7)
  XMM0,
  XMM1,
  XMM2,
  XMM3,
  XMM4,
  XMM5,
  XMM6,
  XMM7,
  XMM8,
  XMM9,
  XMM10,
  XMM11,
  XMM12,
  XMM13,
  XMM14,
  XMM15
};

// the argument registers of GOAL.
// We must have 8 to be compatible with GOAL's 8-argument function calls.
constexpr int ARG_REG_COUNT = 8;

// the first 6 are shared with Linux, and the last two are unique to GOAL.
constexpr X86R ARG_REGS[ARG_REG_COUNT] = {
    X86R::RDI, X86R::RSI, X86R::RDX, X86R::RCX, X86R::R8, X86R::R9, X86R::R10, X86R::R11,
};

// The saved registers of GOAL.  Note that RSP, RBP, R12, R13, R14, R15 shouldn't be changed by the
// caller, but these are special registers and won't be allocated to hold variables.
constexpr int SAVED_REG_COUNT = 4;
constexpr X86R SAVED_REGS[SAVED_REG_COUNT] = {X86R::RBX, X86R::R9, X86R::R10, X86R::R11};

// special registers
constexpr X86R PP_REG = X86R::R12;
constexpr X86R FUNC_REG = X86R::R13;
constexpr X86R OFF_REG = X86R::R14;
constexpr X86R ST_REG = X86R::R15;
constexpr X86R FP_REG = X86R::RBP;
constexpr X86R RET_REG = X86R::RAX;

// size in bytes of a pointer
constexpr int PTR_SIZE = 4;

// size in bytes of a general purpose register
constexpr int GPR_SIZE = 8;

constexpr const char* x86_gpr_names[] = {
    "rax",  "rcx",  "rdx",  "rbx",  "rsp",   "rbp",   "rsi",   "rdi",   "r8",    "r9",   "r10",
    "r11",  "r12",  "r13",  "r14",  "r15",   "xmm0",  "xmm1",  "xmm2",  "xmm3",  "xmm4", "xmm5",
    "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};

/*
 Name   Arg ID   Clobber?  Special
  RAX     -         y       return
  RCX     3         y       arg
  RDX     2         y       arg
  RBX     -         n

  RSP     -         n       stack pointer
  RBP     -         n       base pointer
  RSI     1         y       arg
  RDI     0         y       arg

  R8      4         y       arg
  R9      5         n       arg
  R10     6         n       arg
  R11     7         n       arg
  R12     -         n       pp
  R13     -         n       func
  R14     -         n
  R15
 */

bool is_gpr(u8 reg);
u8 get_nth_xmm(u8 id);
bool is_xmm(u8 reg);
u8 xmm_to_id(u8 reg);

}  // namespace goal

#endif  // JAK1_REGISTERS_H
