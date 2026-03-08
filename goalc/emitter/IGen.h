#pragma once

#include "Instruction.h"
#include "Register.h"

#include "goalc/emitter/ObjectGenerator.h"

namespace emitter {
namespace IGen {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Move data from src to dst. Moves all 64-bits of the GPR.
 */
Instruction mov_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Move a 64-bit constant into a register.
 */
Instruction mov_gpr64_u64(const ObjectGenerator& gen, Register dst, uint64_t val);

/*!
 * Move a 32-bit constant into a register. Zeros the upper 32 bits.
 */
Instruction mov_gpr64_u32(const ObjectGenerator& gen, Register dst, uint64_t val);

/*!
 * Move a signed 32-bit constant into a register. Sign extends for the upper 32 bits.
 * When possible prefer mov_gpr64_u32. (use this only for negative values...)
 * This is always bigger than mov_gpr64_u32, but smaller than a mov_gpr_u64.
 */
Instruction mov_gpr64_s32(const ObjectGenerator& gen, Register dst, int64_t val);

/*!
 * Move 32-bits of xmm to 32 bits of gpr (no sign extension).
 */
Instruction movd_gpr32_xmm32(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Move 32-bits of gpr to 32-bits of xmm (no sign extension)
 */
Instruction movd_xmm32_gpr32(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Move 64-bits of xmm to 64 bits of gpr (no sign extension).
 */
Instruction movq_gpr64_xmm64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Move 64-bits of gpr to 64-bits of xmm (no sign extension)
 */
Instruction movq_xmm64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Move 32-bits between xmm's
 */
Instruction mov_xmm32_xmm32(const ObjectGenerator& gen, Register dst, Register src);

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
Instruction load8s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2);

Instruction store8_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register addr1,
                                          Register addr2,
                                          Register value);

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset);

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register addr1,
                                                  Register addr2,
                                                  Register value,
                                                  s64 offset);

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset);

/*!
 * movzx dst, BYTE PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load8u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2);

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset);

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

/*!
 * movsx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load16s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2);

Instruction store16_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value);

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset);

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset);

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset);

/*!
 * movzx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load16u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2);

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset);

/*!
 * movsxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load32s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2);

Instruction store32_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value);

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset);

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset);

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset);

/*!
 * movzxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load32u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2);

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset);

/*!
 * mov dst, QWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load64_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2);

Instruction store64_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value);

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset);

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset);

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset);

Instruction store_goal_vf(const ObjectGenerator& gen,
                          Register addr,
                          Register value,
                          Register off,
                          s64 offset);

Instruction store_goal_gpr(const ObjectGenerator& gen,
                           Register addr,
                           Register value,
                           Register off,
                           int offset,
                           int size);

Instruction load_goal_xmm128(const ObjectGenerator& gen,
                             Register dst,
                             Register addr,
                             Register off,
                             int offset);

/*!
 * Load memory at addr + offset, where addr is a GOAL pointer and off is the offset register.
 * This will pick the appropriate fancy addressing mode instruction.
 */
Instruction load_goal_gpr(const ObjectGenerator& gen,
                          Register dst,
                          Register addr,
                          Register off,
                          int offset,
                          int size,
                          bool sign_extend);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM32
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction store32_xmm32_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register xmm_value);

Instruction load32_xmm32_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register xmm_dest,
                                          Register addr1,
                                          Register addr2);

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register xmm_value,
                                                   s64 offset);

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register xmm_dest,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset);

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register xmm_value,
                                                    s64 offset);

Instruction lea_reg_plus_off32(const ObjectGenerator& gen,
                               Register dest,
                               Register base,
                               s64 offset);

Instruction lea_reg_plus_off8(const ObjectGenerator& gen, Register dest, Register base, s64 offset);

Instruction lea_reg_plus_off(const ObjectGenerator& gen, Register dest, Register base, s64 offset);

Instruction store32_xmm32_gpr64_plus_s32(const ObjectGenerator& gen,
                                         Register base,
                                         Register xmm_value,
                                         s64 offset);

Instruction store32_xmm32_gpr64_plus_s8(const ObjectGenerator& gen,
                                        Register base,
                                        Register xmm_value,
                                        s64 offset);

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register xmm_dest,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset);

Instruction load32_xmm32_gpr64_plus_s32(const ObjectGenerator& gen,
                                        Register xmm_dest,
                                        Register base,
                                        s64 offset);

Instruction load32_xmm32_gpr64_plus_s8(const ObjectGenerator& gen,
                                       Register xmm_dest,
                                       Register base,
                                       s64 offset);

Instruction load_goal_xmm32(const ObjectGenerator& gen,
                            Register xmm_dest,
                            Register addr,
                            Register off,
                            s64 offset);

Instruction store_goal_xmm32(const ObjectGenerator& gen,
                             Register addr,
                             Register xmm_value,
                             Register off,
                             s64 offset);

Instruction store_reg_offset_xmm32(const ObjectGenerator& gen,
                                   Register base,
                                   Register xmm_value,
                                   s64 offset);

Instruction load_reg_offset_xmm32(const ObjectGenerator& gen,
                                  Register xmm_dest,
                                  Register base,
                                  s64 offset);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM128
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Store a 128-bit xmm into an address stored in a register, no offset
 */
Instruction store128_gpr64_xmm128(const ObjectGenerator& gen,
                                  Register gpr_addr,
                                  Register xmm_value);

Instruction store128_gpr64_xmm128_s32(const ObjectGenerator& gen,
                                      Register gpr_addr,
                                      Register xmm_value,
                                      s64 offset);

Instruction store128_gpr64_xmm128_s8(const ObjectGenerator& gen,
                                     Register gpr_addr,
                                     Register xmm_value,
                                     s64 offset);

Instruction load128_xmm128_gpr64(const ObjectGenerator& gen, Register xmm_dest, Register gpr_addr);

Instruction load128_xmm128_gpr64_s32(const ObjectGenerator& gen,
                                     Register xmm_dest,
                                     Register gpr_addr,
                                     s64 offset);

Instruction load128_xmm128_gpr64_s8(const ObjectGenerator& gen,
                                    Register xmm_dest,
                                    Register gpr_addr,
                                    s64 offset);

Instruction load128_xmm128_reg_offset(const ObjectGenerator& gen,
                                      Register xmm_dest,
                                      Register base,
                                      s64 offset);

Instruction store128_xmm128_reg_offset(const ObjectGenerator& gen,
                                       Register base,
                                       Register xmm_val,
                                       s64 offset);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   RIP loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction load64_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load32s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load32u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load16u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load16s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load8u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction load8s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset);

Instruction static_load(const ObjectGenerator& gen,
                        Register dest,
                        s64 offset,
                        int size,
                        bool sign_extend);

Instruction store64_rip_s32(const ObjectGenerator& gen, Register src, s64 offset);

Instruction store32_rip_s32(const ObjectGenerator& gen, Register src, s64 offset);

Instruction store16_rip_s32(const ObjectGenerator& gen, Register src, s64 offset);

Instruction store8_rip_s32(const ObjectGenerator& gen, Register src, s64 offset);

Instruction static_store(const ObjectGenerator& gen, Register value, s64 offset, int size);

Instruction static_addr(const ObjectGenerator& gen, Register dst, s64 offset);

Instruction static_load_xmm32(const ObjectGenerator& gen, Register xmm_dest, s64 offset);

Instruction static_store_xmm32(const ObjectGenerator& gen, Register xmm_value, s64 offset);

// TODO, special load/stores of 128 bit values.

// TODO, consider specialized stack loads and stores?
Instruction load64_gpr64_plus_s32(const ObjectGenerator& gen,
                                  Register dst_reg,
                                  int32_t offset,
                                  Register src_reg);

/*!
 * Store 64-bits from gpr into memory located at 64-bit reg + 32-bit signed offset.
 */
Instruction store64_gpr64_plus_s32(const ObjectGenerator& gen,
                                   Register addr,
                                   int32_t offset,
                                   Register value);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Function return. Pops the 64-bit return address (real) off the stack and jumps to it.
 */
Instruction ret(const ObjectGenerator& gen);

/*!
 * Instruction to push gpr (64-bits) onto the stack
 */
Instruction push_gpr64(const ObjectGenerator& gen, Register reg);

/*!
 * Instruction to pop 64 bit gpr from the stack
 */
Instruction pop_gpr64(const ObjectGenerator& gen, Register reg);

/*!
 * Call a function stored in a 64-bit gpr
 */
Instruction call_r64(const ObjectGenerator& gen, Register reg_);

/*!
 * Jump to an x86-64 address stored in a 64-bit gpr.
 */
Instruction jmp_r64(const ObjectGenerator& gen, Register reg_);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction sub_gpr64_imm8s(const ObjectGenerator& gen, Register reg, int64_t imm);

Instruction sub_gpr64_imm32s(const ObjectGenerator& gen, Register reg, int64_t imm);

Instruction add_gpr64_imm8s(const ObjectGenerator& gen, Register reg, int64_t v);

Instruction add_gpr64_imm32s(const ObjectGenerator& gen, Register reg, int64_t v);

Instruction add_gpr64_imm(const ObjectGenerator& gen, Register reg, int64_t imm);

Instruction sub_gpr64_imm(const ObjectGenerator& gen, Register reg, int64_t imm);

Instruction add_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

Instruction sub_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Multiply gprs (32-bit, signed).
 * (Note - probably worth doing imul on gpr64's to implement the EE's unsigned multiply)
 */
Instruction imul_gpr32_gpr32(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Multiply gprs (64-bit, signed).
 * DANGER - this treats all operands as 64-bit. This is not like the EE.
 */
Instruction imul_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Divide (idiv, 32 bit)
 */
Instruction idiv_gpr32(const ObjectGenerator& gen, Register reg);

Instruction unsigned_div_gpr32(const ObjectGenerator& gen, Register reg);

/*!
 * Convert doubleword to quadword for division.
 */
Instruction cdq(const ObjectGenerator& gen);

/*!
 * Move from gpr32 to gpr64, with sign extension.
 * Needed for multiplication/divsion madness.
 */
Instruction movsx_r64_r32(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Compare gpr64.  This sets the flags for the jumps.
 * todo UNTESTED
 */
Instruction cmp_gpr64_gpr64(const ObjectGenerator& gen, Register a, Register b);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Or of two gprs
 */
Instruction or_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * And of two gprs
 */
Instruction and_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Xor of two gprs
 */
Instruction xor_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Bitwise not a gpr
 */
Instruction not_gpr64(const ObjectGenerator& gen, Register reg);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Shift 64-bit gpr left by CL register
 */
Instruction shl_gpr64_cl(const ObjectGenerator& gen, Register reg);

/*!
 * Shift 64-bit gpr right (logical) by CL register
 */
Instruction shr_gpr64_cl(const ObjectGenerator& gen, Register reg);

/*!
 * Shift 64-bit gpr right (arithmetic) by CL register
 */
Instruction sar_gpr64_cl(const ObjectGenerator& gen, Register reg);

/*!
 * Shift 64-ptr left (logical) by the constant shift amount "sa".
 */
Instruction shl_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa);

/*!
 * Shift 64-ptr right (logical) by the constant shift amount "sa".
 */
Instruction shr_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa);

/*!
 * Shift 64-ptr right (arithmetic) by the constant shift amount "sa".
 */
Instruction sar_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Jump, 32-bit constant offset.  The offset is by default 0 and must be patched later.
 */
Instruction jmp_32(const ObjectGenerator& gen);

/*!
 * Jump if equal.
 */
Instruction je_32(const ObjectGenerator& gen);

/*!
 * Jump not equal.
 */
Instruction jne_32(const ObjectGenerator& gen);

/*!
 * Jump less than or equal.
 */
Instruction jle_32(const ObjectGenerator& gen);

/*!
 * Jump greater than or equal.
 */
Instruction jge_32(const ObjectGenerator& gen);

/*!
 * Jump less than
 */
Instruction jl_32(const ObjectGenerator& gen);

/*!
 * Jump greater than
 */
Instruction jg_32(const ObjectGenerator& gen);

/*!
 * Jump below or equal
 */
Instruction jbe_32(const ObjectGenerator& gen);

/*!
 * Jump above or equal
 */
Instruction jae_32(const ObjectGenerator& gen);

/*!
 * Jump below
 */
Instruction jb_32(const ObjectGenerator& gen);

/*!
 * Jump above
 */
Instruction ja_32(const ObjectGenerator& gen);

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Compare two floats and set flag register for jump (ucomiss)
 */
Instruction cmp_flt_flt(const ObjectGenerator& gen, Register a, Register b);

Instruction sqrts_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Multiply two floats in xmm's
 */
Instruction mulss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Divide two floats in xmm's
 */
Instruction divss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Subtract two floats in xmm's
 */
Instruction subss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Add two floats in xmm's
 */
Instruction addss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Floating point minimum.
 */
Instruction minss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Floating point maximum.
 */
Instruction maxss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Convert GPR int32 to XMM float (single precision)
 */
Instruction int32_to_float(const ObjectGenerator& gen, Register dst, Register src);

/*!
 * Convert XMM float to GPR int32(single precision) (truncate)
 */
Instruction float_to_int32(const ObjectGenerator& gen, Register dst, Register src);

Instruction nop(const ObjectGenerator& gen);

// TODO - rsqrt / abs / sqrt

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * A "null" instruction.  This instruction does not generate any bytes
 * but can be referred to by a label.  Useful to insert in place of a real instruction
 * if the real instruction has been optimized out.
 */
Instruction null(const ObjectGenerator& gen);

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

Instruction nop_vf(const ObjectGenerator& gen);

Instruction wait_vf(const ObjectGenerator& gen);

Instruction mov_vf_vf(const ObjectGenerator& gen, Register dst, Register src);

Instruction loadvf_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                    Register dst,
                                    Register addr1,
                                    Register addr2);

Instruction loadvf_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                            Register dst,
                                            Register addr1,
                                            Register addr2,
                                            s64 offset);

Instruction loadvf_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                             Register dst,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset);

Instruction storevf_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                     Register value,
                                     Register addr1,
                                     Register addr2);

Instruction storevf_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                             Register value,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset);

Instruction storevf_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                              Register value,
                                              Register addr1,
                                              Register addr2,
                                              s64 offset);

Instruction loadvf_rip_plus_s32(const ObjectGenerator& gen, Register dest, s64 offset);

// TODO - rip relative loads and stores.

Instruction blend_vf(const ObjectGenerator& gen,
                     Register dst,
                     Register src1,
                     Register src2,
                     u8 mask);

Instruction
shuffle_vf(const ObjectGenerator& gen, Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw);

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
Instruction swizzle_vf(const ObjectGenerator& gen, Register dst, Register src, u8 controlBytes);

/*
  Splats a single element in 'src' to all elements in 'dst'
  For example (pseudocode):
  xmm1 = (1.5, 2.5, 3.5, 4.5)
  xmm2 = (1, 2, 3, 4)
  splat_vf(xmm1, xmm2, XMM_ELEMENT::X);
  xmm1 = (4, 4, 4, 4)
  */
Instruction splat_vf(const ObjectGenerator& gen,
                     Register dst,
                     Register src,
                     Register::VF_ELEMENT element);

Instruction xor_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction sub_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction add_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction mul_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction max_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction min_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction div_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2);

Instruction sqrt_vf(const ObjectGenerator& gen, Register dst, Register src);

Instruction itof_vf(const ObjectGenerator& gen, Register dst, Register src);

Instruction ftoi_vf(const ObjectGenerator& gen, Register dst, Register src);

Instruction pw_sra(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction pw_srl(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction ph_srl(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction pw_sll(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction ph_sll(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction parallel_add_byte(const ObjectGenerator& gen,
                              Register dst,
                              Register src0,
                              Register src1);

Instruction parallel_bitwise_or(const ObjectGenerator& gen,
                                Register dst,
                                Register src0,
                                Register src1);

Instruction parallel_bitwise_xor(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1);

Instruction parallel_bitwise_and(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1);

// Reminder - a word in MIPS = 32bits = a DWORD in x86
//     MIPS   ||   x86
// -----------------------
// byte       || byte
// halfword   || word
// word       || dword
// doubleword || quadword

// -- Unpack High Data Instructions
Instruction pextub_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pextuh_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pextuw_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

// -- Unpack Low Data Instructions
Instruction pextlb_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pextlh_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pextlw_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

// Equal to than comparison as 16 bytes (8 bits)
Instruction parallel_compare_e_b(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1);

// Equal to than comparison as 8 halfwords (16 bits)
Instruction parallel_compare_e_h(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1);

// Equal to than comparison as 4 words (32 bits)
Instruction parallel_compare_e_w(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1);

// Greater than comparison as 16 bytes (8 bits)
Instruction parallel_compare_gt_b(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1);

// Greater than comparison as 8 halfwords (16 bits)
Instruction parallel_compare_gt_h(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1);

// Greater than comparison as 4 words (32 bits)
Instruction parallel_compare_gt_w(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1);

Instruction vpunpcklqdq(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pcpyld_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction pcpyud(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction vpsubd(const ObjectGenerator& gen, Register dst, Register src0, Register src1);

Instruction vpsrldq(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction vpslldq(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction vpshuflw(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction vpshufhw(const ObjectGenerator& gen, Register dst, Register src, u8 imm);

Instruction vpackuswb(const ObjectGenerator& gen, Register dst, Register src0, Register src1);
};  // namespace IGen
}  // namespace emitter
