
#include "IGenARM64.h"

#include "goalc/emitter/Instruction.h"

// https://armconverter.com/?code=ret
// https://developer.arm.com/documentation/ddi0487/latest

namespace emitter {
namespace IGen {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 mov_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mov_gpr64_u64(Register dst, uint64_t val) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mov_gpr64_u32(Register dst, uint64_t val) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mov_gpr64_s32(Register dst, int64_t val) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 movd_gpr32_xmm32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 movd_xmm32_gpr32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 movq_gpr64_xmm64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 movq_xmm64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mov_xmm32_xmm32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

// todo - GPR64 -> XMM64 (zext)
// todo - XMM -> GPR64

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   GOAL Loads and Stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                       Register addr2,
                                                       Register value,
                                                       s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                        Register addr2,
                                                        Register value,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store16_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                        Register addr2,
                                                        Register value,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                         Register addr2,
                                                         Register value,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                         Register addr1,
                                                         Register addr2,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                         Register addr1,
                                                         Register addr2,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                        Register addr2,
                                                        Register value,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                         Register addr1,
                                                         Register addr2,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                         Register addr2,
                                                         Register value,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                         Register addr1,
                                                         Register addr2,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store64_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store64_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                        Register addr2,
                                                        Register value,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store64_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                         Register addr2,
                                                         Register value,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store_goal_vf(Register addr, Register value, Register off, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store_goal_gpr(Register addr, Register value, Register off, int offset, int size) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load_goal_xmm128(Register dst, Register addr, Register off, int offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load_goal_gpr(Register dst,
                               Register addr,
                               Register off,
                               int offset,
                               int size,
                               bool sign_extend) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM32
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InstructionARM64 store32_xmm32_gpr64_plus_gpr64(Register addr1,
                                                Register addr2,
                                                Register xmm_value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_gpr64(Register xmm_dest, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_xmm32_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                        Register addr2,
                                                        Register xmm_value,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_gpr64_plus_s8(Register xmm_dest,
                                                       Register addr1,
                                                       Register addr2,
                                                       s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_xmm32_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                         Register addr2,
                                                         Register xmm_value,
                                                         s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 lea_reg_plus_off32(Register dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 lea_reg_plus_off8(Register dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 lea_reg_plus_off(Register dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_xmm32_gpr64_plus_s32(Register base, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_xmm32_gpr64_plus_s8(Register base, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_gpr64_plus_s32(Register xmm_dest,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_s32(Register xmm_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_s8(Register xmm_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load_goal_xmm32(Register xmm_dest, Register addr, Register off, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store_goal_xmm32(Register addr, Register xmm_value, Register off, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store_reg_offset_xmm32(Register base, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load_reg_offset_xmm32(Register xmm_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - SIMD (128-bit, QWORDS)
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 store128_gpr64_xmm128(Register gpr_addr, Register xmm_value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store128_gpr64_xmm128_s32(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store128_gpr64_xmm128_s8(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_xmm128_gpr64_s32(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_xmm128_gpr64_s8(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_xmm128_reg_offset(Register xmm_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   RIP loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 load64_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32s_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32u_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16u_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16s_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8u_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8s_rip_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_load(Register dest, s64 offset, int size, bool sign_extend) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store64_rip_s32(Register src, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_rip_s32(Register src, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store16_rip_s32(Register src, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store8_rip_s32(Register src, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_store(Register value, s64 offset, int size) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_addr(Register dst, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_load_xmm32(Register xmm_dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_store_xmm32(Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

// TODO, special load/stores of 128 bit values.

// TODO, consider specialized stack loads and stores?
InstructionARM64 load64_gpr64_plus_s32(Register dst_reg, int32_t offset, Register src_reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store64_gpr64_plus_s32(Register addr, int32_t offset, Register value) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 ret() {
  // pg. 1850
  return InstructionARM64(0b11010110010111110000001111000000);
}

InstructionARM64 push_gpr64(Register reg) {
  // pg. 1998
  ASSERT(reg.is_gpr());
  // TODO - is hw_id needed?
  return InstructionARM64(0b11111000001);  // TODO - finish
}

InstructionARM64 pop_gpr64(Register reg) {
  // pg. 1998
  ASSERT(reg.is_gpr());
  // TODO - is hw_id needed?
  return InstructionARM64(0b11111000011);  // TODO - finish
}

InstructionARM64 call_r64(Register reg_) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jmp_r64(Register reg_) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InstructionARM64 sub_gpr64_imm8s(Register reg, int64_t imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sub_gpr64_imm32s(Register reg, int64_t imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 add_gpr64_imm8s(Register reg, int64_t v) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 add_gpr64_imm32s(Register reg, int64_t v) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 add_gpr64_imm(Register reg, int64_t imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sub_gpr64_imm(Register reg, int64_t imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 add_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sub_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 imul_gpr32_gpr32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 imul_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 idiv_gpr32(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 unsigned_div_gpr32(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 cdq() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 movsx_r64_r32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 cmp_gpr64_gpr64(Register a, Register b) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 or_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 and_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 xor_gpr64_gpr64(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 not_gpr64(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 shl_gpr64_cl(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 shr_gpr64_cl(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sar_gpr64_cl(Register reg) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 shl_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 shr_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sar_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 jmp_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 je_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jne_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jle_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jge_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jl_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jg_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jbe_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jae_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 jb_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 ja_32() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 cmp_flt_flt(Register a, Register b) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sqrts_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mulss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 divss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 subss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 addss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 minss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 maxss_xmm_xmm(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 int32_to_float(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 float_to_int32(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 nop() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

// TODO - rsqrt / abs / sqrt

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 null() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

InstructionARM64 nop_vf() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 wait_vf() {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mov_vf_vf(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 loadvf_gpr64_plus_gpr64_plus_s8(Register dst,
                                                 Register addr1,
                                                 Register addr2,
                                                 s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 loadvf_gpr64_plus_gpr64_plus_s32(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 storevf_gpr64_plus_gpr64(Register value, Register addr1, Register addr2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 storevf_gpr64_plus_gpr64_plus_s8(Register value,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 storevf_gpr64_plus_gpr64_plus_s32(Register value,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 loadvf_rip_plus_s32(Register dest, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

// TODO - rip relative loads and stores.

InstructionARM64 blend_vf(Register dst, Register src1, Register src2, u8 mask) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 swizzle_vf(Register dst, Register src, u8 controlBytes) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 splat_vf(Register dst, Register src, Register::VF_ELEMENT element) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 xor_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sub_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 add_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 mul_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 max_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 min_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 div_vf(Register dst, Register src1, Register src2) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 sqrt_vf(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 itof_vf(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 ftoi_vf(Register dst, Register src) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pw_sra(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pw_srl(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 ph_srl(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pw_sll(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}
InstructionARM64 ph_sll(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_add_byte(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_bitwise_or(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_bitwise_xor(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_bitwise_and(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextub_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextuh_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextuw_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextlb_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextlh_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pextlw_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_e_b(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_e_h(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_e_w(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 parallel_compare_gt_w(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpunpcklqdq(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pcpyld_swapped(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 pcpyud(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpsubd(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpsrldq(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpslldq(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpshuflw(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpshufhw(Register dst, Register src, u8 imm) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpackuswb(Register dst, Register src0, Register src1) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}
}  // namespace IGen
}  // namespace emitter

#endif