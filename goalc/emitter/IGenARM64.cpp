
#include "goalc/emitter/Instruction.h"
#ifdef __aarch64__
#include <memory>

#include "IGen.h"

// https://armconverter.com/?code=ret
// https://developer.arm.com/documentation/ddi0487/latest

namespace emitter {
namespace IGen {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction mov_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction mov_gpr64_u64(Register dst, uint64_t val) {
  return Instruction(0b0);
}

Instruction mov_gpr64_u32(Register dst, uint64_t val) {
  return Instruction(0b0);
}

Instruction mov_gpr64_s32(Register dst, int64_t val) {
  return Instruction(0b0);
}

Instruction movd_gpr32_xmm32(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction movd_xmm32_gpr32(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction movq_gpr64_xmm64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction movq_xmm64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction mov_xmm32_xmm32(Register dst, Register src) {
  return Instruction(0b0);
}

// todo - GPR64 -> XMM64 (zext)
// todo - XMM -> GPR64

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   GOAL Loads and Stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
//   LOADS n' STORES - SIMD (128-bit, QWORDS)
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

Instruction store64_gpr64_plus_s32(Register addr, int32_t offset, Register value) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction ret() {
  // pg. 1850
  return Instruction(0b11010110010111110000001111000000);
}

Instruction push_gpr64(Register reg) {
  // pg. 1998
  ASSERT(reg.is_gpr());
  // TODO - is hw_id needed?
  return Instruction(0b11111000001);  // TODO - finish
}

Instruction pop_gpr64(Register reg) {
  // pg. 1998
  ASSERT(reg.is_gpr());
  // TODO - is hw_id needed?
  return Instruction(0b11111000011);  // TODO - finish
}

Instruction call_r64(Register reg_) {
  return Instruction(0b0);
}

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

Instruction imul_gpr32_gpr32(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction imul_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction idiv_gpr32(Register reg) {
  return Instruction(0b0);
}

Instruction unsigned_div_gpr32(Register reg) {
  return Instruction(0b0);
}

Instruction cdq() {
  return Instruction(0b0);
}

Instruction movsx_r64_r32(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction cmp_gpr64_gpr64(Register a, Register b) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction or_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction and_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction xor_gpr64_gpr64(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction not_gpr64(Register reg) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction shl_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

Instruction shr_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

Instruction sar_gpr64_cl(Register reg) {
  return Instruction(0b0);
}

Instruction shl_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

Instruction shr_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

Instruction sar_gpr64_u8(Register reg, uint8_t sa) {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction jmp_32() {
  return Instruction(0b0);
}

Instruction je_32() {
  return Instruction(0b0);
}

Instruction jne_32() {
  return Instruction(0b0);
}

Instruction jle_32() {
  return Instruction(0b0);
}

Instruction jge_32() {
  return Instruction(0b0);
}

Instruction jl_32() {
  return Instruction(0b0);
}

Instruction jg_32() {
  return Instruction(0b0);
}

Instruction jbe_32() {
  return Instruction(0b0);
}

Instruction jae_32() {
  return Instruction(0b0);
}

Instruction jb_32() {
  return Instruction(0b0);
}

Instruction ja_32() {
  return Instruction(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction cmp_flt_flt(Register a, Register b) {
  return Instruction(0b0);
}

Instruction sqrts_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction mulss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction divss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction subss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction addss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction minss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction maxss_xmm_xmm(Register dst, Register src) {
  return Instruction(0b0);
}

Instruction int32_to_float(Register dst, Register src) {
  return Instruction(0b0);
}

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

Instruction swizzle_vf(Register dst, Register src, u8 controlBytes) {
  return Instruction(0b0);
}

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

Instruction pextub_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextuh_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextuw_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextlb_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextlh_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction pextlw_swapped(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_compare_e_b(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_compare_e_h(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_compare_e_w(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

Instruction parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  return Instruction(0b0);
}

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