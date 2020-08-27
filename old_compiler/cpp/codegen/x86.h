/*!
 * @file x86.h
 * x86-64 register definitions and calling convention
 */

#ifndef JAK_X86_H
#define JAK_X86_H

// nicknames for gprs and xmm's
enum X86_Registers {
  RAX,  // return, temp
  RCX,  // arg 3
  RDX,  // arg 2
  RBX,  // X saved

  RSP,  // stack pointer
  RBP,  // X base pointer (like fp)
  RSI,  // arg 1
  RDI,  // arg 0

  R8,   // arg 4
  R9,   // arg 5
  R10,  // arg 6 - GOAL only
  R11,  // arg 7 - GOAL only
  R12,  // X saved
  R13,  // X saved - function call register (like t9)
  R14,  // X saved - offset
  R15,  // X saved - st
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
// the first 6 are shared with Linux.
constexpr uint8_t ARG_REGS[8] = {
    RDI, RSI, RDX, RCX, R8, R9, R10, R11,
};

constexpr int SAVED_REG_COUNT = 2;
constexpr uint8_t SAVED_REGS[SAVED_REG_COUNT] = {
    RBX,
    //  R12,
    R13  // we don't really have to do this...
};

// todo - move to an xmm?
constexpr uint8_t PP_REG = R12;

// register used to hold the address of the function we're calling
// this is a GOAL only thing
constexpr uint8_t T9_REG = R13;

// register used to hold the address of the current function
constexpr uint8_t BP_REG = RBP;

// register used to return data (GOAL and Linux)
constexpr uint8_t RET_REG = RAX;

// reserved register which holds the offset from GOAL pointers to real memory addresses
constexpr uint8_t OFF_REG = R14;

// reserved register which holds the pointer to the symbol table
// todo should this hold a GOAL pointer or real pointer? currently a real pointer
constexpr uint8_t ST_REG = R15;

constexpr int PTR_SIZE = 4;
constexpr int GPR_SIZE = 8;

static const char* x86_gpr_names[] = {
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
  R9      5         y       arg
  R10
  R11
  R12
  R13
  R14
  R15
 */

#endif  // JAK_X86_H
