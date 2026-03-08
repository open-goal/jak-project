#pragma once

#include "goalc/emitter/Instruction.h"
#include "goalc/emitter/Register.h"

namespace emitter {
namespace IGen {
namespace X86 {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Move data from src to dst. Moves all 64-bits of the GPR.
 */
InstructionX86 mov_gpr64_gpr64(Register dst, Register src);

/*!
 * Move a 64-bit constant into a register.
 */
InstructionX86 mov_gpr64_u64(Register dst, uint64_t val);

/*!
 * Move a 32-bit constant into a register. Zeros the upper 32 bits.
 */
InstructionX86 mov_gpr64_u32(Register dst, uint64_t val);

/*!
 * Move a signed 32-bit constant into a register. Sign extends for the upper 32 bits.
 * When possible prefer mov_gpr64_u32. (use this only for negative values...)
 * This is always bigger than mov_gpr64_u32, but smaller than a mov_gpr_u64.
 */
InstructionX86 mov_gpr64_s32(Register dst, int64_t val);

/*!
 * Move 32-bits of xmm to 32 bits of gpr (no sign extension).
 */
InstructionX86 movd_gpr32_xmm32(Register dst, Register src);

/*!
 * Move 32-bits of gpr to 32-bits of xmm (no sign extension)
 */
InstructionX86 movd_xmm32_gpr32(Register dst, Register src);

/*!
 * Move 64-bits of xmm to 64 bits of gpr (no sign extension).
 */
InstructionX86 movq_gpr64_xmm64(Register dst, Register src);

/*!
 * Move 64-bits of gpr to 64-bits of xmm (no sign extension)
 */
InstructionX86 movq_xmm64_gpr64(Register dst, Register src);

/*!
 * Move 32-bits between xmm's
 */
InstructionX86 mov_xmm32_xmm32(Register dst, Register src);

// todo - GPR64 -> XMM64 (zext)
// todo - XMM -> GPR64

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   GOAL Loads and Stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * movsx dst, BYTE PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value);

InstructionX86 load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                     Register addr1,
                                                     Register addr2,
                                                     s64 offset);

InstructionX86 store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                     Register addr2,
                                                     Register value,
                                                     s64 offset);

InstructionX86 load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                      Register addr2,
                                                      Register value,
                                                      s64 offset);

/*!
 * movzx dst, BYTE PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                     Register addr1,
                                                     Register addr2,
                                                     s64 offset);

InstructionX86 load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

/*!
 * movsx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 store16_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value);

InstructionX86 store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                      Register addr2,
                                                      Register value,
                                                      s64 offset);

InstructionX86 store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                       Register addr2,
                                                       Register value,
                                                       s64 offset);

InstructionX86 load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset);

/*!
 * movzx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset);

/*!
 * movsxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 store32_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value);

InstructionX86 load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 store32_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                      Register addr2,
                                                      Register value,
                                                      s64 offset);

InstructionX86 load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset);

InstructionX86 store32_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                       Register addr2,
                                                       Register value,
                                                       s64 offset);

/*!
 * movzxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset);

/*!
 * mov dst, QWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
InstructionX86 load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 store64_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value);

InstructionX86 load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                     Register addr1,
                                                     Register addr2,
                                                     s64 offset);

InstructionX86 store64_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                      Register addr2,
                                                      Register value,
                                                      s64 offset);

InstructionX86 load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 store64_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                       Register addr2,
                                                       Register value,
                                                       s64 offset);

InstructionX86 store_goal_vf(Register addr, Register value, Register off, s64 offset);

InstructionX86 store_goal_gpr(Register addr, Register value, Register off, int offset, int size);

InstructionX86 load_goal_xmm128(Register dst, Register addr, Register off, int offset);

/*!
 * Load memory at addr + offset, where addr is a GOAL pointer and off is the offset register.
 * This will pick the appropriate fancy addressing mode instruction.
 */
InstructionX86 load_goal_gpr(Register dst,
                             Register addr,
                             Register off,
                             int offset,
                             int size,
                             bool sign_extend);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM32
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InstructionX86 store32_xmm32_gpr64_plus_gpr64(Register addr1, Register addr2, Register xmm_value);

InstructionX86 load32_xmm32_gpr64_plus_gpr64(Register xmm_dest, Register addr1, Register addr2);

InstructionX86 store32_xmm32_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                      Register addr2,
                                                      Register xmm_value,
                                                      s64 offset);

InstructionX86 load32_xmm32_gpr64_plus_gpr64_plus_s8(Register xmm_dest,
                                                     Register addr1,
                                                     Register addr2,
                                                     s64 offset);

InstructionX86 store32_xmm32_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                       Register addr2,
                                                       Register xmm_value,
                                                       s64 offset);

InstructionX86 lea_reg_plus_off32(Register dest, Register base, s64 offset);

InstructionX86 lea_reg_plus_off8(Register dest, Register base, s64 offset);

InstructionX86 lea_reg_plus_off(Register dest, Register base, s64 offset);

InstructionX86 store32_xmm32_gpr64_plus_s32(Register base, Register xmm_value, s64 offset);

InstructionX86 store32_xmm32_gpr64_plus_s8(Register base, Register xmm_value, s64 offset);

InstructionX86 load32_xmm32_gpr64_plus_gpr64_plus_s32(Register xmm_dest,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset);

InstructionX86 load32_xmm32_gpr64_plus_s32(Register xmm_dest, Register base, s64 offset);

InstructionX86 load32_xmm32_gpr64_plus_s8(Register xmm_dest, Register base, s64 offset);

InstructionX86 load_goal_xmm32(Register xmm_dest, Register addr, Register off, s64 offset);

InstructionX86 store_goal_xmm32(Register addr, Register xmm_value, Register off, s64 offset);

InstructionX86 store_reg_offset_xmm32(Register base, Register xmm_value, s64 offset);

InstructionX86 load_reg_offset_xmm32(Register xmm_dest, Register base, s64 offset);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM128
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Store a 128-bit xmm into an address stored in a register, no offset
 */
InstructionX86 store128_gpr64_xmm128(Register gpr_addr, Register xmm_value);

InstructionX86 store128_gpr64_xmm128_s32(Register gpr_addr, Register xmm_value, s64 offset);

InstructionX86 store128_gpr64_xmm128_s8(Register gpr_addr, Register xmm_value, s64 offset);

InstructionX86 load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr);

InstructionX86 load128_xmm128_gpr64_s32(Register xmm_dest, Register gpr_addr, s64 offset);

InstructionX86 load128_xmm128_gpr64_s8(Register xmm_dest, Register gpr_addr, s64 offset);

InstructionX86 load128_xmm128_reg_offset(Register xmm_dest, Register base, s64 offset);

InstructionX86 store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   RIP loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionX86 load64_rip_s32(Register dest, s64 offset);

InstructionX86 load32s_rip_s32(Register dest, s64 offset);

InstructionX86 load32u_rip_s32(Register dest, s64 offset);

InstructionX86 load16u_rip_s32(Register dest, s64 offset);

InstructionX86 load16s_rip_s32(Register dest, s64 offset);

InstructionX86 load8u_rip_s32(Register dest, s64 offset);

InstructionX86 load8s_rip_s32(Register dest, s64 offset);

InstructionX86 static_load(Register dest, s64 offset, int size, bool sign_extend);

InstructionX86 store64_rip_s32(Register src, s64 offset);

InstructionX86 store32_rip_s32(Register src, s64 offset);

InstructionX86 store16_rip_s32(Register src, s64 offset);

InstructionX86 store8_rip_s32(Register src, s64 offset);

InstructionX86 static_store(Register value, s64 offset, int size);

InstructionX86 static_addr(Register dst, s64 offset);

InstructionX86 static_load_xmm32(Register xmm_dest, s64 offset);

InstructionX86 static_store_xmm32(Register xmm_value, s64 offset);

// TODO, special load/stores of 128 bit values.

// TODO, consider specialized stack loads and stores?
InstructionX86 load64_gpr64_plus_s32(Register dst_reg, int32_t offset, Register src_reg);

/*!
 * Store 64-bits from gpr into memory located at 64-bit reg + 32-bit signed offset.
 */
InstructionX86 store64_gpr64_plus_s32(Register addr, int32_t offset, Register value);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Function return. Pops the 64-bit return address (real) off the stack and jumps to it.
 */
InstructionX86 ret();

/*!
 * Instruction to push gpr (64-bits) onto the stack
 */
InstructionX86 push_gpr64(Register reg);

/*!
 * Instruction to pop 64 bit gpr from the stack
 */
InstructionX86 pop_gpr64(Register reg);

/*!
 * Call a function stored in a 64-bit gpr
 */
InstructionX86 call_r64(Register reg_);

/*!
 * Jump to an x86-64 address stored in a 64-bit gpr.
 */
InstructionX86 jmp_r64(Register reg_);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InstructionX86 sub_gpr64_imm8s(Register reg, int64_t imm);

InstructionX86 sub_gpr64_imm32s(Register reg, int64_t imm);

InstructionX86 add_gpr64_imm8s(Register reg, int64_t v);

InstructionX86 add_gpr64_imm32s(Register reg, int64_t v);

InstructionX86 add_gpr64_imm(Register reg, int64_t imm);

InstructionX86 sub_gpr64_imm(Register reg, int64_t imm);

InstructionX86 add_gpr64_gpr64(Register dst, Register src);

InstructionX86 sub_gpr64_gpr64(Register dst, Register src);

/*!
 * Multiply gprs (32-bit, signed).
 * (Note - probably worth doing imul on gpr64's to implement the EE's unsigned multiply)
 */
InstructionX86 imul_gpr32_gpr32(Register dst, Register src);

/*!
 * Multiply gprs (64-bit, signed).
 * DANGER - this treats all operands as 64-bit. This is not like the EE.
 */
InstructionX86 imul_gpr64_gpr64(Register dst, Register src);

/*!
 * Divide (idiv, 32 bit)
 */
InstructionX86 idiv_gpr32(Register reg);

InstructionX86 unsigned_div_gpr32(Register reg);

/*!
 * Convert doubleword to quadword for division.
 */
InstructionX86 cdq();

/*!
 * Move from gpr32 to gpr64, with sign extension.
 * Needed for multiplication/divsion madness.
 */
InstructionX86 movsx_r64_r32(Register dst, Register src);

/*!
 * Compare gpr64.  This sets the flags for the jumps.
 * todo UNTESTED
 */
InstructionX86 cmp_gpr64_gpr64(Register a, Register b);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Or of two gprs
 */
InstructionX86 or_gpr64_gpr64(Register dst, Register src);

/*!
 * And of two gprs
 */
InstructionX86 and_gpr64_gpr64(Register dst, Register src);

/*!
 * Xor of two gprs
 */
InstructionX86 xor_gpr64_gpr64(Register dst, Register src);

/*!
 * Bitwise not a gpr
 */
InstructionX86 not_gpr64(Register reg);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Shift 64-bit gpr left by CL register
 */
InstructionX86 shl_gpr64_cl(Register reg);

/*!
 * Shift 64-bit gpr right (logical) by CL register
 */
InstructionX86 shr_gpr64_cl(Register reg);

/*!
 * Shift 64-bit gpr right (arithmetic) by CL register
 */
InstructionX86 sar_gpr64_cl(Register reg);

/*!
 * Shift 64-ptr left (logical) by the constant shift amount "sa".
 */
InstructionX86 shl_gpr64_u8(Register reg, uint8_t sa);

/*!
 * Shift 64-ptr right (logical) by the constant shift amount "sa".
 */
InstructionX86 shr_gpr64_u8(Register reg, uint8_t sa);

/*!
 * Shift 64-ptr right (arithmetic) by the constant shift amount "sa".
 */
InstructionX86 sar_gpr64_u8(Register reg, uint8_t sa);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Jump, 32-bit constant offset.  The offset is by default 0 and must be patched later.
 */
InstructionX86 jmp_32();

/*!
 * Jump if equal.
 */
InstructionX86 je_32();

/*!
 * Jump not equal.
 */
InstructionX86 jne_32();

/*!
 * Jump less than or equal.
 */
InstructionX86 jle_32();

/*!
 * Jump greater than or equal.
 */
InstructionX86 jge_32();

/*!
 * Jump less than
 */
InstructionX86 jl_32();

/*!
 * Jump greater than
 */
InstructionX86 jg_32();

/*!
 * Jump below or equal
 */
InstructionX86 jbe_32();

/*!
 * Jump above or equal
 */
InstructionX86 jae_32();

/*!
 * Jump below
 */
InstructionX86 jb_32();

/*!
 * Jump above
 */
InstructionX86 ja_32();

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Compare two floats and set flag register for jump (ucomiss)
 */
InstructionX86 cmp_flt_flt(Register a, Register b);

InstructionX86 sqrts_xmm(Register dst, Register src);

/*!
 * Multiply two floats in xmm's
 */
InstructionX86 mulss_xmm_xmm(Register dst, Register src);

/*!
 * Divide two floats in xmm's
 */
InstructionX86 divss_xmm_xmm(Register dst, Register src);

/*!
 * Subtract two floats in xmm's
 */
InstructionX86 subss_xmm_xmm(Register dst, Register src);

/*!
 * Add two floats in xmm's
 */
InstructionX86 addss_xmm_xmm(Register dst, Register src);

/*!
 * Floating point minimum.
 */
InstructionX86 minss_xmm_xmm(Register dst, Register src);

/*!
 * Floating point maximum.
 */
InstructionX86 maxss_xmm_xmm(Register dst, Register src);

/*!
 * Convert GPR int32 to XMM float (single precision)
 */
InstructionX86 int32_to_float(Register dst, Register src);

/*!
 * Convert XMM float to GPR int32(single precision) (truncate)
 */
InstructionX86 float_to_int32(Register dst, Register src);

InstructionX86 nop();

// TODO - rsqrt / abs / sqrt

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * A "null" instruction.  This instruction does not generate any bytes
 * but can be referred to by a label.  Useful to insert in place of a real instruction
 * if the real instruction has been optimized out.
 */
InstructionX86 null();

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

InstructionX86 nop_vf();

InstructionX86 wait_vf();

InstructionX86 mov_vf_vf(Register dst, Register src);

InstructionX86 loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2);

InstructionX86 loadvf_gpr64_plus_gpr64_plus_s8(Register dst,
                                               Register addr1,
                                               Register addr2,
                                               s64 offset);

InstructionX86 loadvf_gpr64_plus_gpr64_plus_s32(Register dst,
                                                Register addr1,
                                                Register addr2,
                                                s64 offset);

InstructionX86 storevf_gpr64_plus_gpr64(Register value, Register addr1, Register addr2);

InstructionX86 storevf_gpr64_plus_gpr64_plus_s8(Register value,
                                                Register addr1,
                                                Register addr2,
                                                s64 offset);

InstructionX86 storevf_gpr64_plus_gpr64_plus_s32(Register value,
                                                 Register addr1,
                                                 Register addr2,
                                                 s64 offset);

InstructionX86 loadvf_rip_plus_s32(Register dest, s64 offset);

// TODO - rip relative loads and stores.

InstructionX86 blend_vf(Register dst, Register src1, Register src2, u8 mask);

InstructionX86 shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw);

/*
  Generic Swizzle (re-arrangment of packed FPs) operation, the control bytes are quite involved.
  Here's a brief run-down:
  - 8-bits / 4 groups of 2 bits
  - Right-to-left, each group is used to determine which element in `src` gets copied into
  `dst`'s element (W->X).
  - GROUP OPTIONS
  - 00b - Copy the least-significant element (X)
  - 01b - Copy the second element (from the right) (Y)
  - 10b - Copy the third element (from the right) (Z)
  - 11b - Copy the most significant element (W)
  Examples
  ; xmm1 = (1.5, 2.5, 3.5, 4.5) (W,Z,Y,X in x86 land)
  SHUFPS xmm1, xmm1, 0xff ; Copy the most significant element to all positions
  > (1.5, 1.5, 1.5, 1.5)
  SHUFPS xmm1, xmm1, 0x39 ; Rotate right
  > (4.5, 1.5, 2.5, 3.5)
  */
InstructionX86 swizzle_vf(Register dst, Register src, u8 controlBytes);

/*
  Splats a single element in 'src' to all elements in 'dst'
  For example (pseudocode):
  xmm1 = (1.5, 2.5, 3.5, 4.5)
  xmm2 = (1, 2, 3, 4)
  splat_vf(xmm1, xmm2, XMM_ELEMENT::X);
  xmm1 = (4, 4, 4, 4)
  */
InstructionX86 splat_vf(Register dst, Register src, Register::VF_ELEMENT element);

InstructionX86 xor_vf(Register dst, Register src1, Register src2);

InstructionX86 sub_vf(Register dst, Register src1, Register src2);

InstructionX86 add_vf(Register dst, Register src1, Register src2);

InstructionX86 mul_vf(Register dst, Register src1, Register src2);

InstructionX86 max_vf(Register dst, Register src1, Register src2);

InstructionX86 min_vf(Register dst, Register src1, Register src2);

InstructionX86 div_vf(Register dst, Register src1, Register src2);

InstructionX86 sqrt_vf(Register dst, Register src);

InstructionX86 itof_vf(Register dst, Register src);

InstructionX86 ftoi_vf(Register dst, Register src);

InstructionX86 pw_sra(Register dst, Register src, u8 imm);

InstructionX86 pw_srl(Register dst, Register src, u8 imm);

InstructionX86 ph_srl(Register dst, Register src, u8 imm);

InstructionX86 pw_sll(Register dst, Register src, u8 imm);

InstructionX86 ph_sll(Register dst, Register src, u8 imm);

InstructionX86 parallel_add_byte(Register dst, Register src0, Register src1);

InstructionX86 parallel_bitwise_or(Register dst, Register src0, Register src1);

InstructionX86 parallel_bitwise_xor(Register dst, Register src0, Register src1);

InstructionX86 parallel_bitwise_and(Register dst, Register src0, Register src1);

// Reminder - a word in MIPS = 32bits = a DWORD in x86
//     MIPS   ||   x86
// -----------------------
// byte       || byte
// halfword   || word
// word       || dword
// doubleword || quadword

// -- Unpack High Data Instructions
InstructionX86 pextub_swapped(Register dst, Register src0, Register src1);

InstructionX86 pextuh_swapped(Register dst, Register src0, Register src1);

InstructionX86 pextuw_swapped(Register dst, Register src0, Register src1);

// -- Unpack Low Data Instructions
InstructionX86 pextlb_swapped(Register dst, Register src0, Register src1);

InstructionX86 pextlh_swapped(Register dst, Register src0, Register src1);

InstructionX86 pextlw_swapped(Register dst, Register src0, Register src1);

// Equal to than comparison as 16 bytes (8 bits)
InstructionX86 parallel_compare_e_b(Register dst, Register src0, Register src1);

// Equal to than comparison as 8 halfwords (16 bits)
InstructionX86 parallel_compare_e_h(Register dst, Register src0, Register src1);

// Equal to than comparison as 4 words (32 bits)
InstructionX86 parallel_compare_e_w(Register dst, Register src0, Register src1);

// Greater than comparison as 16 bytes (8 bits)
InstructionX86 parallel_compare_gt_b(Register dst, Register src0, Register src1);

// Greater than comparison as 8 halfwords (16 bits)
InstructionX86 parallel_compare_gt_h(Register dst, Register src0, Register src1);

// Greater than comparison as 4 words (32 bits)
InstructionX86 parallel_compare_gt_w(Register dst, Register src0, Register src1);

InstructionX86 vpunpcklqdq(Register dst, Register src0, Register src1);

InstructionX86 pcpyld_swapped(Register dst, Register src0, Register src1);

InstructionX86 pcpyud(Register dst, Register src0, Register src1);

InstructionX86 vpsubd(Register dst, Register src0, Register src1);

InstructionX86 vpsrldq(Register dst, Register src, u8 imm);

InstructionX86 vpslldq(Register dst, Register src, u8 imm);

InstructionX86 vpshuflw(Register dst, Register src, u8 imm);

InstructionX86 vpshufhw(Register dst, Register src, u8 imm);

InstructionX86 vpackuswb(Register dst, Register src0, Register src1);
}  // namespace X86
}  // namespace IGen
}  // namespace emitter