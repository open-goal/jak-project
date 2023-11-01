#include <memory>
#ifdef __aarch64__

#include "IGen.h"

namespace emitter {
namespace IGen {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Move data from src to dst. Moves all 64-bits of the GPR.
 */
Instruction mov_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Move a 64-bit constant into a register.
 */
Instruction mov_gpr64_u64(Register dst, uint64_t val) {
  return Instruction(0b0);
}

/*!
 * Move a 32-bit constant into a register. Zeros the upper 32 bits.
 */
Instruction mov_gpr64_u32(Register dst, uint64_t val) {
  return Instruction(0b0);
}

/*!
 * Move a signed 32-bit constant into a register. Sign extends for the upper 32 bits.
 * When possible prefer mov_gpr64_u32. (use this only for negative values...)
 * This is always bigger than mov_gpr64_u32, but smaller than a mov_gpr_u64.
 */
Instruction mov_gpr64_s32(Register dst, int64_t val) {
  return Instruction(0b0);
}

/*!
 * Move 32-bits of xmm to 32 bits of gpr (no sign extension).
 */
Instruction movd_gpr32_xmm32(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Move 32-bits of gpr to 32-bits of xmm (no sign extension)
 */
Instruction movd_xmm32_gpr32(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Move 64-bits of xmm to 64 bits of gpr (no sign extension).
 */
Instruction movq_gpr64_xmm64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Move 64-bits of gpr to 64-bits of xmm (no sign extension)
 */
Instruction movq_xmm64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Move 32-bits between xmm's
 */
Instruction mov_xmm32_xmm32(Register dst, Register src) {
  return Instruction(0b0);
}

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
Instruction load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  return Instruction(0b0);
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  return Instruction(0b0);
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                  Register addr2,
                                                  Register value,
                                                  s64 offset) {
  return Instruction(0b0);
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  return Instruction(0b0);
}

/*!
 * movzx dst, BYTE PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  return Instruction(0b0);
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

/*!
 * movsx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction store16_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  return Instruction(0b0);
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  return Instruction(0b0);
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  return Instruction(0b0);
}

/*!
 * movzx dst, WORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  return Instruction(0b0);
}

/*!
 * movsxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction store32_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  return Instruction(0b0);
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  return Instruction(0b0);
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  return Instruction(0b0);
}

/*!
 * movzxd dst, DWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  return Instruction(0b0);
}

/*!
 * mov dst, QWORD PTR [addr1 + addr2]
 * addr1 and addr2 have to be different registers.
 * Cannot use rsp.
 */
Instruction load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction store64_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  return Instruction(0b0);
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  return Instruction(0b0);
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  return Instruction(0b0);
}

Instruction store_goal_vf(Register addr, Register value, Register off, s64 offset) {
  return Instruction(0b0);
}

Instruction store_goal_gpr(Register addr, Register value, Register off, int offset, int size) {
  return Instruction(0b0);
}

Instruction load_goal_xmm128(Register dst, Register addr, Register off, int offset) {
  return Instruction(0b0);
}

/*!
 * Load memory at addr + offset, where addr is a GOAL pointer and off is the offset register.
 * This will pick the appropriate fancy addressing mode instruction.
 */
Instruction load_goal_gpr(Register dst,
                          Register addr,
                          Register off,
                          int offset,
                          int size,
                          bool sign_extend) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM32
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction store32_xmm32_gpr64_plus_gpr64(Register addr1, Register addr2, Register xmm_value) {
  return Instruction(0b0);
}

Instruction load32_xmm32_gpr64_plus_gpr64(Register xmm_dest, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register xmm_value,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s8(Register xmm_dest,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  return Instruction(0b0);
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register xmm_value,
                                                    s64 offset) {
  return Instruction(0b0);
}

Instruction lea_reg_plus_off32(Register dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction lea_reg_plus_off8(Register dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction lea_reg_plus_off(Register dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction store32_xmm32_gpr64_plus_s32(Register base, Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

Instruction store32_xmm32_gpr64_plus_s8(Register base, Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s32(Register xmm_dest,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  return Instruction(0b0);
}

Instruction load32_xmm32_gpr64_plus_s32(Register xmm_dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction load32_xmm32_gpr64_plus_s8(Register xmm_dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction load_goal_xmm32(Register xmm_dest, Register addr, Register off, s64 offset) {
  return Instruction(0b0);
}

Instruction store_goal_xmm32(Register addr, Register xmm_value, Register off, s64 offset) {
  return Instruction(0b0);
}

Instruction store_reg_offset_xmm32(Register base, Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

Instruction load_reg_offset_xmm32(Register xmm_dest, Register base, s64 offset) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM128
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Store a 128-bit xmm into an address stored in a register, no offset
 */
Instruction store128_gpr64_xmm128(Register gpr_addr, Register xmm_value) {
  return Instruction(0b0);
}

Instruction store128_gpr64_xmm128_s32(Register gpr_addr, Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

Instruction store128_gpr64_xmm128_s8(Register gpr_addr, Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

Instruction load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr) {
  return Instruction(0b0);
}

Instruction load128_xmm128_gpr64_s32(Register xmm_dest, Register gpr_addr, s64 offset) {
  return Instruction(0b0);
}

Instruction load128_xmm128_gpr64_s8(Register xmm_dest, Register gpr_addr, s64 offset) {
  return Instruction(0b0);
}

Instruction load128_xmm128_reg_offset(Register xmm_dest, Register base, s64 offset) {
  return Instruction(0b0);
}

Instruction store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   RIP loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction load64_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load32s_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load32u_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load16u_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load16s_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load8u_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction load8s_rip_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

Instruction static_load(Register dest, s64 offset, int size, bool sign_extend) {
  return Instruction(0b0);
}

Instruction store64_rip_s32(Register src, s64 offset) {
  return Instruction(0b0);
}

Instruction store32_rip_s32(Register src, s64 offset) {
  return Instruction(0b0);
}

Instruction store16_rip_s32(Register src, s64 offset) {
  return Instruction(0b0);
}

Instruction store8_rip_s32(Register src, s64 offset) {
  return Instruction(0b0);
}

Instruction static_store(Register value, s64 offset, int size) {
  return Instruction(0b0);
}

Instruction static_addr(Register dst, s64 offset) {
  return Instruction(0b0);
}

Instruction static_load_xmm32(Register xmm_dest, s64 offset) {
  return Instruction(0b0);
}

Instruction static_store_xmm32(Register xmm_value, s64 offset) {
  return Instruction(0b0);
}

// TODO, special load/stores of 128 bit values.

// TODO, consider specialized stack loads and stores?
Instruction load64_gpr64_plus_s32(Register dst_reg, int32_t offset, Register src_reg) {
  return Instruction(0b0);
}

/*!
 * Store 64-bits from gpr into memory located at 64-bit reg + 32-bit signed offset.
 */
Instruction store64_gpr64_plus_s32(Register addr, int32_t offset, Register value) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/*!
 * Function return. Pops the 64-bit return address (real) off the stack and jumps to it.
 */
Instruction ret() {
  return InstructionARM64(0b11010110010111110000001111000000);
}

/*!
 * Instruction to push gpr (64-bits) onto the stack
 */
Instruction push_gpr64(Register reg) {
  return Instruction(0b0);
}

/*!
 * Instruction to pop 64 bit gpr from the stack
 */
Instruction pop_gpr64(Register reg) {
  return Instruction(0b0);
}

/*!
 * Call a function stored in a 64-bit gpr
 */
Instruction call_r64(Register reg_) {
  return Instruction(0b0);
}

/*!
 * Jump to an x86-64 address stored in a 64-bit gpr.
 */
Instruction jmp_r64(Register reg_) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction sub_gpr64_imm8s(Register reg, int64_t imm) {
  return Instruction(0b0);
}

Instruction sub_gpr64_imm32s(Register reg, int64_t imm) {
  return Instruction(0b0);
}

Instruction add_gpr64_imm8s(Register reg, int64_t v) {
  return Instruction(0b0);
}

Instruction add_gpr64_imm32s(Register reg, int64_t v) {
  return Instruction(0b0);
}

Instruction add_gpr64_imm(Register reg, int64_t imm) {
  return Instruction(0b0);
}

Instruction sub_gpr64_imm(Register reg, int64_t imm) {
  return Instruction(0b0);
}

Instruction add_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction sub_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Multiply gprs (32-bit, signed).
 * (Note - probably worth doing imul on gpr64's to implement the EE's unsigned multiply)
 */
Instruction imul_gpr32_gpr32(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Multiply gprs (64-bit, signed).
 * DANGER - this treats all operands as 64-bit. This is not like the EE.
 */
Instruction imul_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Divide (idiv, 32 bit)
 */
Instruction idiv_gpr32(Register reg) {
  return Instruction(0b0);
}

Instruction unsigned_div_gpr32(Register reg) {
  return Instruction(0b0);
}

/*!
 * Convert doubleword to quadword for division.
 */
Instruction cdq() {
  return Instruction(0b0);
}

/*!
 * Move from gpr32 to gpr64, with sign extension.
 * Needed for multiplication/divsion madness.
 */
Instruction movsx_r64_r32(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Compare gpr64.  This sets the flags for the jumps.
 * todo UNTESTED
 */
Instruction cmp_gpr64_gpr64(Register a, Register b) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Or of two gprs
 */
Instruction or_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * And of two gprs
 */
Instruction and_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Xor of two gprs
 */
Instruction xor_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Bitwise not a gpr
 */
Instruction not_gpr64(Register reg) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Shift 64-bit gpr left by CL register
 */
Instruction shl_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

/*!
 * Shift 64-bit gpr right (logical) by CL register
 */
Instruction shr_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

/*!
 * Shift 64-bit gpr right (arithmetic) by CL register
 */
Instruction sar_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

/*!
 * Shift 64-ptr left (logical) by the constant shift amount "sa".
 */
Instruction shl_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

/*!
 * Shift 64-ptr right (logical) by the constant shift amount "sa".
 */
Instruction shr_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

/*!
 * Shift 64-ptr right (arithmetic) by the constant shift amount "sa".
 */
Instruction sar_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Jump, 32-bit constant offset.  The offset is by default 0 and must be patched later.
 */
Instruction jmp_32() {
  return Instruction(0b0);
}

/*!
 * Jump if equal.
 */
Instruction je_32() {
  return Instruction(0b0);
}

/*!
 * Jump not equal.
 */
Instruction jne_32() {
  return Instruction(0b0);
}

/*!
 * Jump less than or equal.
 */
Instruction jle_32() {
  return Instruction(0b0);
}

/*!
 * Jump greater than or equal.
 */
Instruction jge_32() {
  return Instruction(0b0);
}

/*!
 * Jump less than
 */
Instruction jl_32() {
  return Instruction(0b0);
}

/*!
 * Jump greater than
 */
Instruction jg_32() {
  return Instruction(0b0);
}

/*!
 * Jump below or equal
 */
Instruction jbe_32() {
  return Instruction(0b0);
}

/*!
 * Jump above or equal
 */
Instruction jae_32() {
  return Instruction(0b0);
}

/*!
 * Jump below
 */
Instruction jb_32() {
  return Instruction(0b0);
}

/*!
 * Jump above
 */
Instruction ja_32() {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Compare two floats and set flag register for jump (ucomiss)
 */
Instruction cmp_flt_flt(Register a, Register b) {
  return Instruction(0b0);
}

Instruction sqrts_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Multiply two floats in xmm's
 */
Instruction mulss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Divide two floats in xmm's
 */
Instruction divss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Subtract two floats in xmm's
 */
Instruction subss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Add two floats in xmm's
 */
Instruction addss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Floating point minimum.
 */
Instruction minss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Floating point maximum.
 */
Instruction maxss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Convert GPR int32 to XMM float (single precision)
 */
Instruction int32_to_float(Register dst, Register src) {
  return Instruction(0b0);
}

/*!
 * Convert XMM float to GPR int32(single precision) (truncate)
 */
Instruction float_to_int32(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction nop() {
  return Instruction(0b0);
}

// TODO - rsqrt / abs / sqrt

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * A "null" instruction.  This Instruction does not generate any bytes
 * but can be referred to by a label.  Useful to insert in place of a real instruction
 * if the real Instruction has been optimized out.
 */
Instruction null() {
  return Instruction(0b0);
}

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

Instruction nop_vf() {
  return Instruction(0b0);
}

Instruction wait_vf() {
  return Instruction(0b0);
}

Instruction mov_vf_vf(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction loadvf_gpr64_plus_gpr64_plus_s8(Register dst,
                                            Register addr1,
                                            Register addr2,
                                            s64 offset) {
  return Instruction(0b0);
}

Instruction loadvf_gpr64_plus_gpr64_plus_s32(Register dst,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  return Instruction(0b0);
}

Instruction storevf_gpr64_plus_gpr64(Register value, Register addr1, Register addr2) {
  return Instruction(0b0);
}

Instruction storevf_gpr64_plus_gpr64_plus_s8(Register value,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  return Instruction(0b0);
}

Instruction storevf_gpr64_plus_gpr64_plus_s32(Register value,
                                              Register addr1,
                                              Register addr2,
                                              s64 offset) {
  return Instruction(0b0);
}

Instruction loadvf_rip_plus_s32(Register dest, s64 offset) {
  return Instruction(0b0);
}

// TODO - rip relative loads and stores.

Instruction blend_vf(Register dst, Register src1, Register src2, u8 mask) {
  return Instruction(0b0);
}

Instruction shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  return Instruction(0b0);
}

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
Instruction swizzle_vf(Register dst, Register src, u8 controlBytes) {
  return Instruction(0b0);
}

/*
  Splats a single element in 'src' to all elements in 'dst'
  For example (pseudocode):
  xmm1 = (1.5, 2.5, 3.5, 4.5)
  xmm2 = (1, 2, 3, 4)
  splat_vf(xmm1, xmm2, XMM_ELEMENT::X);
  xmm1 = (4, 4, 4, 4)
  */
Instruction splat_vf(Register dst, Register src, Register::VF_ELEMENT element) {
  return Instruction(0b0);
}

Instruction xor_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction sub_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction add_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction mul_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction max_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction min_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction div_vf(Register dst, Register src1, Register src2) {
  return Instruction(0b0);
}

Instruction sqrt_vf(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction itof_vf(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction ftoi_vf(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction pw_sra(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction pw_srl(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction ph_srl(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction pw_sll(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}
Instruction ph_sll(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction parallel_add_byte(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_bitwise_or(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_bitwise_xor(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_bitwise_and(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Reminder - a word in MIPS = 32bits = a DWORD in x86
//     MIPS   ||   x86
// -----------------------
// byte       || byte
// halfword   || word
// word       || dword
// doubleword || quadword

// -- Unpack High Data Instructions
Instruction pextub_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextuh_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextuw_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// -- Unpack Low Data Instructions
Instruction pextlb_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextlh_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextlw_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Equal to than comparison as 16 bytes (8 bits)
Instruction parallel_compare_e_b(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Equal to than comparison as 8 halfwords (16 bits)
Instruction parallel_compare_e_h(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Equal to than comparison as 4 words (32 bits)
Instruction parallel_compare_e_w(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Greater than comparison as 16 bytes (8 bits)
Instruction parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Greater than comparison as 8 halfwords (16 bits)
Instruction parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

// Greater than comparison as 4 words (32 bits)
Instruction parallel_compare_gt_w(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction vpunpcklqdq(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pcpyld_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pcpyud(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction vpsubd(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction vpsrldq(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction vpslldq(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction vpshuflw(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction vpshufhw(Register dst, Register src, u8 imm) {
  return Instruction(0b0);
}

Instruction vpackuswb(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}
}  // namespace IGen
}  // namespace emitter

#endif