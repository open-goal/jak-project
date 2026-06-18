
#include "IGenARM64.h"

#include <tuple>

#include "goalc/emitter/Instruction.h"
#include "goalc/emitter/InstructionSet.h"
#include "goalc/emitter/Register.h"

// https://armconverter.com/?code=ret
// https://developer.arm.com/documentation/ddi0487/latest

// TODO ARM64 - just silencing errors while things are not implemented obviously
#pragma GCC diagnostic ignored "-Wunused-parameter"

namespace emitter {
namespace IGen {
namespace ARM64 {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

const auto instr_set = emitter::InstructionSet::ARM64;
using namespace emitter::ARM64;

InstructionARM64 mov_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/mov_orr_log_shift.html
  // MOV <Xd>, <Xm>
  ASSERT(dst.is_gpr(instr_set));
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b10101010000, 11), Rm(src.id()), Rn(0b11111), Rd(dst.id()),
                          Imm6(0));
}

InstructionARM64 mov_gpr64_u64(Register dst, uint64_t val) {
  // Cannot be done in a single instruction, must combine multiple MOVZ/MOVKs
  std::vector<InstructionARM64> instrs;
  bool emitted_movz = false;
  for (int i = 0; i < 4; i++) {
    u16 chunk = (val >> (i * 16)) & 0xFFFF;
    if (!emitted_movz && chunk != 0) {
      // https://www.scs.stanford.edu/~zyedidia/arm64/movz.html
      // MOVZ <Xd>, #<imm>{, LSL #<shift>/16}
      instrs.emplace_back(
          InstructionARM64(Base(0b110100101, 9), Hw(i), Imm16(chunk), Rd(dst.id())));
      emitted_movz = true;
    } else if (emitted_movz && chunk != 0) {
      // https://www.scs.stanford.edu/~zyedidia/arm64/movk.html
      // MOVK <Xd>, #<imm>{, LSL #<shift>/16}
      instrs.emplace_back(
          InstructionARM64(Base(0b111100101, 9), Hw(i), Imm16(chunk), Rd(dst.id())));
    }
  }
  if (!emitted_movz) {
    // https://www.scs.stanford.edu/~zyedidia/arm64/movz.html
    // MOVZ <Xd>, #<imm>{, LSL #0}
    instrs.emplace_back(InstructionARM64(Base(0b110100101, 9), Hw(0), Imm16(0), Rd(dst.id())));
  }
  return InstructionARM64(instrs);
}

InstructionARM64 mov_gpr64_u32(Register dst, uint64_t val) {
  return mov_gpr64_u64(dst, val);
}

InstructionARM64 mov_gpr64_s32(Register dst, int64_t val) {
  // preserve sign -- but we are are simply moving the bits
  u64 raw_val = static_cast<u64>(val);  // via int64_t → sign already there
  return mov_gpr64_u64(dst, raw_val);
}

InstructionARM64 movd_gpr32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmov_float_gen.html
  // Single-precision to 32-bit (sf == 0 && ftype == 00 && rmode == 00 && opcode == 110)
  // FMOV <Wd>, <Sn>
  ASSERT(dst.is_gpr(instr_set));
  return InstructionARM64(Base(0b0001111000100110000000, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 movd_f32_gpr32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmov_float_gen.html
  // 32-bit to single-precision (sf == 0 && ftype == 00 && rmode == 00 && opcode == 111)
  // FMOV <Sd>, <Wn>
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b0001111000100111000000, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 movq_gpr64_f64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmov_float_gen.html
  // Double-precision to 64-bit (sf == 1 && ftype == 01 && rmode == 00 && opcode == 110)
  // FMOV <Xd>, <Dn>
  ASSERT(dst.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001111001100110000000, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 movq_f64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmov_float_gen.html
  // 64-bit to double-precision (sf == 1 && ftype == 01 && rmode == 00 && opcode == 111)
  // FMOV <Xd>, <Dn>
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001111001100111000000, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 mov_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmov_float.html
  // Single-precision (ftype == 00)
  // FMOV <Sd>, <Sn>
  return InstructionARM64(Base(0b0001111000100000010000, 22), Rn(src.id()), Rd(dst.id()));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   GOAL Loads and Stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldrsb_reg.html
  // 64-bit with extended register offset (opc == 10 && option != 011)
  // LDRSB <Xt>, [<Xn|SP>, (<Wm>|<Xm>), <extend> {<amount>}]
  ASSERT(dst.is_gpr(instr_set));
  ASSERT(addr1.is_gpr(instr_set));
  ASSERT(addr2.is_gpr(instr_set));
  return InstructionARM64(Base(0b0011100010100000111010, 22), Rt(dst.id()), Rn(addr1.id()),
                          Rm(addr2.id()));
}

InstructionARM64 store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/strb_reg.html
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

InstructionARM64 load32_xmm32_gpr64_plus_gpr64(Register simd_dest, Register addr1, Register addr2) {
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

InstructionARM64 load32_xmm32_gpr64_plus_gpr64_plus_s8(Register simd_dest,
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

InstructionARM64 load32_xmm32_gpr64_plus_gpr64_plus_s32(Register simd_dest,
                                                        Register addr1,
                                                        Register addr2,
                                                        s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_s32(Register simd_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load32_xmm32_gpr64_plus_s8(Register simd_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load_goal_xmm32(Register simd_dest, Register addr, Register off, s64 offset) {
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

InstructionARM64 load_reg_offset_xmm32(Register simd_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - SIMD (128-bit, QWORDS)
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 store128_gpr64_simd128(Register gpr_addr, Register simd_reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/str_imm_fpsimd.html
  // - STR Qn, [Xn] (unsigned offset)
  ASSERT(gpr_addr.is_gpr(instr_set));
  ASSERT(
      simd_reg.is_128bit_simd(instr_set));  // TODO ARM64 - this assertion isn't as useful for ARM
                                            // since Q registers are not unique in terms of their id
  return InstructionARM64(Base(0b0011110110, 10), Rn(gpr_addr.id()), Rt(simd_reg.id()), Imm12(0));
}

InstructionARM64 store128_gpr64_simd128_s32(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store128_gpr64_simd128_s8(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_simd128_gpr64(Register simd_dest, Register gpr_addr) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_imm_fpsimd.html
  // - LDR <Qt>, [<Xn|SP>{, #<pimm>}]
  ASSERT(gpr_addr.is_gpr(instr_set));
  ASSERT(simd_dest.is_128bit_simd(
      instr_set));  // TODO ARM64 - this assertion isn't as useful for ARM
                    // since Q registers are not unique in terms of their id
  return InstructionARM64(Base(0b0011110111, 10), Rn(gpr_addr.id()), Rt(simd_dest.id()), Imm12(0));
}

InstructionARM64 load128_simd128_gpr64_s32(Register simd_dest, Register gpr_addr, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_simd128_gpr64_s8(Register simd_dest, Register gpr_addr, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load128_xmm128_reg_offset(Register simd_dest, Register base, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   PC relative loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

// Implement with LDR but that has a 1MB range limit on ARM (not 2GB like on x86)
// Hopefully this is fine, however it could potentially not be if this is loading static data, which
// may not within 1MB of the current instruction -- that all depends on the linker layout.
//
// TODO ARM - But keep it simple at first, add good assertions and we'll see what happens when we
// compile for real.

const int ARM64_LDR_MIN = -(1 << 18) * 4;
const int ARM64_LDR_MAX = ((1 << 18) - 1) * 4;

InstructionARM64 load64_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_lit_gen.html
  // LDR <Xt>, <label>
  return InstructionARM64(Base(0b01011000, 8), Imm19(offset / 4), Rt(dest.id()));
}

InstructionARM64 load32s_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldrsw_lit.html
  // LDRSW <Xt>, <label>
  return InstructionARM64(Base(0b10011000, 8), Imm19(offset / 4), Rt(dest.id()));
}

InstructionARM64 load32u_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_lit_gen.html
  // LDR <Wt>, <label>
  return InstructionARM64(Base(0b00011000, 8), Imm19(offset / 4), Rt(dest.id()));
}

// TODO ARM - 8/16 bit loads don't have a literal version, that means these
// MUST use a temporary register.

InstructionARM64 load16u_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load16s_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8u_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 load8s_pcRel_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_load(Register dest, s64 offset, int size, bool sign_extend) {
  switch (size) {
    case 1:
      if (sign_extend) {
        return load8s_pcRel_s32(dest, offset);
      } else {
        return load8u_pcRel_s32(dest, offset);
      }
      break;
    case 2:
      if (sign_extend) {
        return load16s_pcRel_s32(dest, offset);
      } else {
        return load16u_pcRel_s32(dest, offset);
      }
      break;
    case 4:
      if (sign_extend) {
        return load8s_pcRel_s32(dest, offset);
      } else {
        return load8u_pcRel_s32(dest, offset);
      }
      break;
    case 8:
      return load8s_pcRel_s32(dest, offset);
    default:
      ASSERT(false);
  }
}

// TODO ARM - no direct store instructions, gotta be two and involve a register

InstructionARM64 store64_pcRel_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store32_pcRel_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store16_pcRel_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 store8_pcRel_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 static_store(Register value, s64 offset, int size) {
  switch (size) {
    case 1:
      return store8_pcRel_s32(value, offset);
    case 2:
      return store16_pcRel_s32(value, offset);
    case 4:
      return store32_pcRel_s32(value, offset);
    case 8:
      return store64_pcRel_s32(value, offset);
    default:
      ASSERT(false);
  }
}

InstructionARM64 static_addr(Register dest, s64 offset) {
  ASSERT(dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_lit_gen.html
  // LDR <Xt>, <label>
  return InstructionARM64(Base(0b01011000, 8), Imm19(offset / 4), Rt(dest.id()));
}

InstructionARM64 static_load_f32(Register simd_dest, s64 offset) {
  ASSERT(simd_dest.is_gpr(instr_set));
  ASSERT_MSG(offset >= ARM64_LDR_MIN && offset <= ARM64_LDR_MAX,
             "PC Relative offset is too large for ARM64, fix it.");
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_lit_fpsimd.html
  // LDR <St>, <label>
  return InstructionARM64(Base(0b00011100, 8), Imm19(offset / 4), Rt(simd_dest.id()));
}

// TODO ARM - no direct store instructions, gotta be two and involve a register

InstructionARM64 static_store_f32(Register xmm_value, s64 offset) {
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
  // https://www.scs.stanford.edu/~zyedidia/arm64/ret.html
  // - defaults to using X30 if Rn is absent
  return InstructionARM64(Base(0b1101011001011111000000, 22), Rn(30));
}

InstructionARM64 push_gpr64(Register reg) {
  // ARM64 stack grows down, so we subtract 16 from SP and store the register
  // Equivalent assembly: STR reg, [SP, #-16]!
  // - https://www.scs.stanford.edu/~zyedidia/arm64/str_imm_gen.html
  // We use 16 because in ARM, the stack must be 16-byte aligned.
  // This does mean we are inefficiently using the stack, there are a few better options:
  // - Push in pairs, two registers at a time
  // - Preallocate stack-space
  // But we can't do either of these at this level, this is an optimization that has to come from
  // higher in the stack.  Here we are concerned with just satisfying the need to push a GPR
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1111100000000000000011, 22), Imm9s(-16), Rn(ARM64_REG::SP),
                          Rt(reg.id()));
}

InstructionARM64 pop_gpr64(Register reg) {
  // ldr reg, [sp], #16
  // - https://www.scs.stanford.edu/~zyedidia/arm64/ldr_imm_gen.html
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1111100001000000000001, 22), Imm9s(16), Rn(ARM64_REG::SP),
                          Rt(reg.id()));
}

InstructionARM64 call_r64(Register reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/blr.html
  // BLR <Xn>
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1101011000111111000000, 22), Rn(reg.id()));
}

InstructionARM64 jmp_r64(Register reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/br.html
  // BR <Xn>
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1101011000011111000000, 22), Rn(reg.id()));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 sub_gpr64_imm8s(Register reg, int64_t imm) {
  return sub_gpr64_imm(reg, imm);
}

InstructionARM64 add_gpr64_imm8s(Register reg, int64_t imm) {
  return add_gpr64_imm(reg, imm);
}

InstructionARM64 sub_gpr64_imm32s(Register reg, int64_t imm) {
  return sub_gpr64_imm(reg, imm);
}

InstructionARM64 add_gpr64_imm32s(Register reg, int64_t imm) {
  return add_gpr64_imm(reg, imm);
}

// Checks whether or not an immediate can be represented in 12 unsigned bits, either:
// - plain [0-4095] immediate
// - imm << 12 (some multiple of 4096)
std::tuple<bool, u16, bool> can_encode_single_imm12(u64 imm) {
  if (imm < 4096) {
    return {true, static_cast<u16>(imm), false};
  }
  if ((imm & 0xFFF) == 0) {  // divisible by 4096
    uint32_t upper = imm >> 12;
    if (upper < 4096) {
      return {true, static_cast<uint16_t>(upper), true};
    }
  }
  return {false, 0, false};
}

// Given a larger than u12 immediate, decompose it into multiple (shifted or not)
// immediates that can be used to emit multiple instructions to produce the desired outcome
std::vector<std::tuple<u16, bool>> decompose_into_imm12_chunks(u64 imm) {
  std ::vector<std::tuple<u16, bool>> result;
  u32 upper = imm >> 12;
  while (upper > 0) {
    u16 chunk = (upper > 4095) ? 4095 : static_cast<u16>(upper);
    result.emplace_back(chunk, true);
    upper -= chunk;
  }

  u16 lower = imm & 0xFFF;
  if (lower > 0) {
    result.emplace_back(lower, false);
  }
  return result;
}

InstructionARM64 add_gpr64_imm(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr(instr_set));
  if (imm < 0) {
    return sub_gpr64_imm(reg, std::abs(imm));
  }
  // Check to see if we can represent this subtraction in a single instruction
  // if not, then we need to emit multiple partial instructions
  const auto [is_single_instr, imm12, needs_shift] = can_encode_single_imm12(imm);
  if (is_single_instr) {
    // https://www.scs.stanford.edu/~zyedidia/arm64/add_addsub_imm.html
    // ADD <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}
    return InstructionARM64(Base(0b100100010, 9), Sh(needs_shift ? 1 : 0), Imm12(imm12),
                            Rd(reg.id()), Rn(reg.id()));
  } else {
    const auto chunks = decompose_into_imm12_chunks(imm);
    std::vector<InstructionARM64> instrs;
    for (const auto& [_imm12, _needs_shift] : chunks) {
      // https://www.scs.stanford.edu/~zyedidia/arm64/add_addsub_imm.html
      // ADD <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}
      instrs.emplace_back(InstructionARM64(Base(0b100100010, 9), Sh(_needs_shift ? 1 : 0),
                                           Imm12(_imm12), Rd(reg.id()), Rn(reg.id())));
    }
    return InstructionARM64(instrs);
  }
}

InstructionARM64 sub_gpr64_imm(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr(instr_set));
  if (imm < 0) {
    return add_gpr64_imm(reg, std::abs(imm));
  }
  // Check to see if we can represent this subtraction in a single instruction
  // if not, then we need to emit multiple partial instructions
  const auto [is_single_instr, imm12, needs_shift] = can_encode_single_imm12(imm);
  if (is_single_instr) {
    // https://www.scs.stanford.edu/~zyedidia/arm64/sub_addsub_imm.html
    // SUB <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}
    return InstructionARM64(Base(0b110100010, 9), Sh(needs_shift ? 1 : 0), Imm12(imm12),
                            Rd(reg.id()), Rn(reg.id()));
  } else {
    const auto chunks = decompose_into_imm12_chunks(imm);
    std::vector<InstructionARM64> instrs;
    for (const auto& [_imm12, _needs_shift] : chunks) {
      // https://www.scs.stanford.edu/~zyedidia/arm64/sub_addsub_imm.html
      // SUB <Xd|SP>, <Xn|SP>, #<imm>{, <shift>}
      instrs.emplace_back(InstructionARM64(Base(0b110100010, 9), Sh(_needs_shift ? 1 : 0),
                                           Imm12(_imm12), Rd(reg.id()), Rn(reg.id())));
    }
    return InstructionARM64(instrs);
  }
}

InstructionARM64 add_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/add_addsub_shift.html
  // ADD <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
  return InstructionARM64(Base(0b10001011000, 11), Rd(dst.id()), Imm6(0), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 sub_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/sub_addsub_shift.html
  // SUB <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
  return InstructionARM64(Base(0b11001011000, 11), Rd(dst.id()), Imm6(0), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 imul_gpr32_gpr32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/mul_madd.html
  // MUL <Wd>, <Wn>, <Wm>
  return InstructionARM64(Base(0b0001101100000000011111, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 imul_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/mul_madd.html
  // MUL <Xd>, <Xn>, <Xm>
  return InstructionARM64(Base(0b1001101100000000011111, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 idiv_gpr32(Register reg) {
  // divides on x86 are annoying, its one of many that involve hard-coded register src/destinations,
  // deal with it last
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 unsigned_div_gpr32(Register reg) {
  // divides on x86 are annoying, its one of many that involve hard-coded register src/destinations,
  // deal with it last
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 cdq() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/asr_asrv.html
  // asr x3, x0, #63
  // (using X3 = edx and X0 = eax)
  // TODO - hardcoded registers, need to check this...
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(Base(0b1001101011000000001010, 22), Rm(63), Rn(ARM64_REG::X0),
                          Rd(ARM64_REG::X3));
}

InstructionARM64 movsx_r64_r32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/sxtw_sbfm.html
  // SXTW <Xd>, <Wn>
  return InstructionARM64(Base(0b1001001101000000011111, 22), Rd(dst.id()), Rn(src.id()));
}

InstructionARM64 cmp_gpr64_gpr64(Register a, Register b) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmp_subs_addsub_ext.html
  // CMP <Xn|SP>, <R><m>{, <extend> {#<amount>}}
  return InstructionARM64(Base(0b11101011001000000000000000011111, 32), Rn(a.id()), Rn(b.id()));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 or_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/orr_log_shift.html
  // ORR <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
  ASSERT(dst.is_gpr(instr_set));
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b10101010000, 11), Rd(dst.id()), Rn(dst.id()), Rm(src.id()));
}

InstructionARM64 and_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/add_addsub_shift.html
  // ADD <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
  ASSERT(dst.is_gpr(instr_set));
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b10001011000, 11), Rd(dst.id()), Rn(dst.id()), Rm(src.id()));
}

InstructionARM64 xor_gpr64_gpr64(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/eor_log_shift.html
  // EOR <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
  ASSERT(dst.is_gpr(instr_set));
  ASSERT(src.is_gpr(instr_set));
  return InstructionARM64(Base(0b11001010000, 11), Rd(dst.id()), Rn(dst.id()), Rm(src.id()));
}

InstructionARM64 not_gpr64(Register reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/mvn_orn_log_shift.html
  // MVN <Xd>, <Xm>{, <shift> #<amount>}
  // ==
  // ORN <Xd>, XZR, <Xm>{, <shift> #<amount>}
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b101010100010000000000011111, 27), Rd(reg.id()), Rm(reg.id()));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 shl_gpr64_reg(Register reg, Register shift_reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/lsl_lslv.html
  // LSL <Xd>, <Xn>, <Xm>
  ASSERT(reg.is_gpr(instr_set));
  ASSERT(shift_reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001101011000000001000, 22), Rd(reg.id()), Rn(reg.id()),
                          Rm(shift_reg.id()));
}

InstructionARM64 shr_gpr64_reg(Register reg, Register shift_reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/lsr_lsrv.html
  // LSR <Xd>, <Xn>, <Xm>
  ASSERT(reg.is_gpr(instr_set));
  ASSERT(shift_reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001101011000000001001, 22), Rd(reg.id()), Rn(reg.id()),
                          Rm(shift_reg.id()));
}

InstructionARM64 sar_gpr64_reg(Register reg, Register shift_reg) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/asr_asrv.html
  // ASR <Xd>, <Xn>, <Xm>
  ASSERT(reg.is_gpr(instr_set));
  ASSERT(shift_reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001101011000000001010, 22), Rd(reg.id()), Rn(reg.id()),
                          Rm(shift_reg.id()));
}

InstructionARM64 shl_gpr64_u8(Register reg, uint8_t sa) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/lsl_ubfm.html
  // LSL <Xd>, <Xn>, #<shift>
  ASSERT(sa < 63);
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1101001101, 10), Rd(reg.id()), Rn(reg.id()), Immr((64 - sa) & 63),
                          Imms(63 - sa));
}

InstructionARM64 shr_gpr64_u8(Register reg, uint8_t sa) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/lsr_ubfm.html
  // LSR <Xd>, <Xn>, #<shift>
  // sf	1	0	1	0	0	1	1	0	N
  ASSERT(sa < 63);
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1101001101000000111111, 22), Rd(reg.id()), Rn(reg.id()), Immr(sa));
}

InstructionARM64 sar_gpr64_u8(Register reg, uint8_t sa) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/asr_sbfm.html
  // ASR <Xd>, <Xn>, #<shift>
  ASSERT(sa < 63);
  ASSERT(reg.is_gpr(instr_set));
  return InstructionARM64(Base(0b1001001101000000111111, 22), Rd(reg.id()), Rn(reg.id()), Immr(sa));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//
// All of these instructions jump to a target that is zero
// and then its up to the IR to patch the actual target
//
// Critically, on arm these relative targets must be within
// 128MB, which is much less than x86 (~2GB)
//
// However, these functions are only really used for jumps within
// a given function...of which the largest we've seen isn't even in the MB
// of sizes
//
// So for now, keep it simple and don't implement something more
// complicated like veneers, this should be fine.

InstructionARM64 jmp_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_uncond.html
  // B <label>
  return InstructionARM64(Base(0b000101, 6), Imm26(0));
}

// Now these instructions in ARM are even more limiting, conditional
// branches must be within 1MB relative to the instruction
//
// However once again, that is still WAY below our biggest functions of a few kb
//
// But still...be aware!  There should be some assertions in place in the patching so that
// said issues don't just fly under the radar
//
// Also, these instructions may have to be patched slightly differently since
// ARM uses a single branch instruction for all
//
// It's worth noting that these x86 instructions also have limitations, they cannot
// jump to far labels (labels in other code segments).  But that's more difficult to
// give a numeric value to like with ARM.

InstructionARM64 je_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 0000 	EQ
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b0000));
}

InstructionARM64 jne_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 0001 	NE
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b0001));
}

InstructionARM64 jle_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1101 	LE
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1101));
}

InstructionARM64 jge_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1010 	GE
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1010));
}

InstructionARM64 jl_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1011 	LT
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1011));
}

InstructionARM64 jg_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1100 	GT
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1100));
}

InstructionARM64 jbe_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1001 	LS
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1001));
}

InstructionARM64 jae_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 0010 	CS
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b0010));
}

InstructionARM64 jb_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 0011 	CC
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b0011));
}

InstructionARM64 ja_imm() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/b_cond.html
  // B.<cond> <label>
  // 1000 	HI
  return InstructionARM64(Base(0b01010100, 8), Imm19(0), Cond(0b1000));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 cmp_f32_f32(Register a, Register b) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fcmp_float.html
  // Single-precision (ftype == 00 && opc == 00)
  // FCMP <Sn>, <Sm>
  return InstructionARM64(Base(0b00011110001000000010000000000000, 32), Rn(a.id()), Rm(b.id()));
}

InstructionARM64 sqrt_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fsqrt_float.html
  // Single-precision (ftype == 00)
  // FSQRT <Sd>, <Sn>
  return InstructionARM64(Base(0b0001111000100001110000, 22), Rn(src.id()), Rm(dst.id()));
}

InstructionARM64 mul_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmul_float.html
  // Single-precision (ftype == 00)
  // FMUL <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000000010, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 div_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fdiv_float.html
  // Single-precision (ftype == 00)
  // FDIV <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000000110, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 sub_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fsub_float.html
  // Single-precision (ftype == 00)
  // FSUB <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000001110, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 add_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fadd_float.html
  // Single-precision (ftype == 00)
  // FADD <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000001010, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 min_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmin_float.html
  // Single-precision (ftype == 00)
  // FMIN <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000010110, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 max_f32_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmax_float.html
  // Single-precision (ftype == 00)
  // FMAX <Sd>, <Sn>, <Sm>
  return InstructionARM64(Base(0b0001111000100000010010, 22), Rd(dst.id()), Rn(dst.id()),
                          Rm(src.id()));
}

InstructionARM64 int32_to_f32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/scvtf_float_int.html
  // 32-bit to single-precision (sf == 0 && ftype == 00)
  // SCVTF <Sd>, <Wn>
  return InstructionARM64(Base(0b0001111000100010000000, 22), Rd(dst.id()), Rn(dst.id()));
}

InstructionARM64 f32_to_int32(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fcvtzs_float_int.html
  // 32-bit to single-precision (sf == 0 && ftype == 00)
  // FCVTZS <Wd>, <Sn>
  return InstructionARM64(Base(0b0001111000111000000000, 22), Rd(dst.id()), Rn(dst.id()));
}

InstructionARM64 nop() {
  // https://www.scs.stanford.edu/~zyedidia/arm64/nop.html
  return InstructionARM64(Base(0b11010101000000110010000000011111, 32));
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InstructionARM64 null() {
  // dummy empty byte
  return InstructionARM64(0b0);
}

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

InstructionARM64 nop_vf() {
  // Not sure if this one was even needed for x86, but it does not really exist on ARM64
  // just use a normal nop
  return nop();
}

InstructionARM64 wait_vf() {
  // Another instruction that doesnt really map to arm64 because there is no annoying
  // x87 FPU behaviour
  return nop();
}

InstructionARM64 mov_vf_vf(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/mov_orr_advsimd_reg.html
  // MOV <Vd>.<T>, <Vn>.<T>
  // Q 	<T>
  // 0 	8B
  // 1 	16B
  ASSERT(dst.is_128bit_simd(instr_set));
  ASSERT(src.is_128bit_simd(instr_set));
  return InstructionARM64(Base(0b0100111010100000000111, 22), Rd(dst.id()), Rn(src.id()));
}

InstructionARM64 loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ldr_reg_fpsimd.html
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

InstructionARM64 blend_vf(Register dst, Register src1, Register src2, u8 mask) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 swizzle_vf(Register dst, Register src, u8 controlBytes) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 splat_vf(Register dst, Register src, Register::VF_ELEMENT element) {
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 xor_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/eor_advsimd.html
  // EOR <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111000100000000111, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 sub_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fsub_advsimd.html
  // FSUB <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0100111010100000110101, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 add_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/add_advsimd.html
  // ADD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0100111010100000100001, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 mul_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fmul_advsimd_vec.html
  // FMUL <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0110111000100000110111, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 max_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/famax_advsimd.html
  // FAMAX <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0100111010100000110111, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 min_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/famin_advsimd.html
  // FAMIN <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0110111010100000110111, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 div_vf(Register dst, Register src1, Register src2) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fdiv_advsimd.html
  // FDIV <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0110111000100000111111, 22), Rn(src1.id()), Rm(src2.id()),
                          Rd(dst.id()));
}

InstructionARM64 sqrt_vf(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fsqrt_advsimd.html
  // FSQRT <Vd>.<T>, <Vn>.<T>
  // 4 single precision floats
  return InstructionARM64(Base(0b0110111010100001111110, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 itof_vf(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/scvtf_advsimd_int.html
  // SCVTF <Vd>.<T>, <Vn>.<T>
  // s32 int -> 4 single precision floats
  return InstructionARM64(Base(0b0100111000100001110110, 22), Rn(src.id()), Rd(dst.id()));
}

InstructionARM64 ftoi_vf(Register dst, Register src) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/fcvtzs_advsimd_int.html
  // FCVTZS <Vd>.<T>, <Vn>.<T>
  // 4 single precision floats -> s32 ints
  // TODO - double check rounding mode
  return InstructionARM64(Base(0b0100111010100001101110, 22), Rn(src.id()), Rd(dst.id()));
}

// TODO - rename these instructions

// - arithmetic_shift_right_32bit_vf
InstructionARM64 pw_sra(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/sshr_advsimd.html
  // - vector, 4S
  // SSHR <Vd>.<T>, <Vn>.<T>, #<shift>
  return InstructionARM64(Base(0b0100111100100000000001, 22), Rn(src.id()), Rd(dst.id()),
                          Immb(imm));
}

// - logical_shift_right_32bit_vf
InstructionARM64 pw_srl(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ushr_advsimd.html
  // - vector, 4S
  // USHR <Vd>.<T>, <Vn>.<T>, #<shift>
  return InstructionARM64(Base(0b0110111100100000000001, 22), Rn(src.id()), Rd(dst.id()),
                          Immb(imm));
}

// - logical_shift_left_32bit_vf
InstructionARM64 pw_sll(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/shl_advsimd.html
  // - vector, 4S
  // SHL <Vd>.<T>, <Vn>.<T>, #<shift>
  return InstructionARM64(Base(0b0100111100100000010101, 22), Rn(src.id()), Rd(dst.id()),
                          Immb(imm));
}

// - logical_shift_right_16bit_vf
InstructionARM64 ph_srl(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ushr_advsimd.html
  // - vector, 8H
  // USHR <Vd>.<T>, <Vn>.<T>, #<shift>
  return InstructionARM64(Base(0b0110111100010000000001, 22), Rn(src.id()), Rd(dst.id()),
                          Immb(imm));
}

// - logical_shift_left_16bit_vf
InstructionARM64 ph_sll(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/shl_advsimd.html
  // - vector, 8H
  // SHL <Vd>.<T>, <Vn>.<T>, #<shift>
  return InstructionARM64(Base(0b0100111100010000010101, 22), Rn(src.id()), Rd(dst.id()),
                          Immb(imm));
}

InstructionARM64 parallel_add_byte(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/add_advsimd.html
  // - vector, 16B
  // ADD <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111000100000100001, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_bitwise_or(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/orr_advsimd_reg.html
  // - vector, 16B
  // ORR <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111010100000000111, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_bitwise_xor(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/eor_advsimd.html
  // - vector, 16B
  // EOR <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111000100000000111, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_bitwise_and(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/and_advsimd.html
  // - vector, 16B
  // AND <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111000100000000111, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextub_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp2_advsimd.html
  // - 16B
  // UZP2 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111000000000010110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextuh_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp2_advsimd.html
  // - 8H
  // UZP2 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111001000000010110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextuw_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp2_advsimd.html
  // - 4S
  // UZP2 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111010000000010110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextlb_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp1_advsimd.html
  // - 16B
  // UZP1 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111000000000000110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextlh_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp1_advsimd.html
  // - 8H
  // UZP1 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111001000000000110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pextlw_swapped(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/uzp1_advsimd.html
  // - 4S
  // UZP1 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111010000000000110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_e_b(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmeq_advsimd_reg.html
  // - vector, 16B
  // CMEQ <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111000100000100011, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_e_h(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmeq_advsimd_reg.html
  // - vector, 8H
  // CMEQ <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111001100000100011, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_e_w(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmeq_advsimd_reg.html
  // - vector, 4S
  // CMEQ <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111010100000100011, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmgt_advsimd_reg.html
  // - vector, 16B
  // CMGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111000100000001101, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmgt_advsimd_reg.html
  // - vector, 8H
  // CMGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111001100000001101, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 parallel_compare_gt_w(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/cmgt_advsimd_reg.html
  // - vector, 4S
  // CMGT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111010100000001101, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

// TODO - rename this monstrosity from x86

InstructionARM64 vpunpcklqdq(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/zip1_advsimd.html
  // - vector, 2D
  // ZIP1 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111011000000001110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

InstructionARM64 pcpyld_swapped(Register dst, Register src0, Register src1) {
  return vpunpcklqdq(dst, src0, src1);
}

InstructionARM64 pcpyud(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/zip2_advsimd.html
  // - vector, 2D
  // ZIP2 <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0100111011000000011110, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

// TODO - more x86 rename candidates

// lane-wise-32bit-substraction
InstructionARM64 vpsubd(Register dst, Register src0, Register src1) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/sub_advsimd.html
  // - vector, 4S
  // SUB <Vd>.<T>, <Vn>.<T>, <Vm>.<T>
  return InstructionARM64(Base(0b0110111010100000100001, 22), Rn(src0.id()), Rm(src1.id()),
                          Rd(dst.id()));
}

// shift-right-logical-entire-simd-reg
InstructionARM64 vpsrldq(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ext_advsimd.html
  // - 16B
  // EXT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>, #<index>
  return InstructionARM64(Base(0b0110111000000000000000, 22), Rn(src.id()), Rm(src.id()),
                          Rd(dst.id()), Imm4(imm));
}

// shift-left-logical-entire-simd-reg
InstructionARM64 vpslldq(Register dst, Register src, u8 imm) {
  // https://www.scs.stanford.edu/~zyedidia/arm64/ext_advsimd.html
  // - 16B
  // EXT <Vd>.<T>, <Vn>.<T>, <Vm>.<T>, #<index>
  return InstructionARM64(Base(0b0110111000000000000000, 22), Rn(src.id()), Rm(src.id()),
                          Rd(dst.id()), Imm4((16 - imm) & 0xF));
}

InstructionARM64 vpshuflw(Register dst, Register src, u8 imm) {
  // TBL and a mov
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpshufhw(Register dst, Register src, u8 imm) {
  // TBL and a mov
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}

InstructionARM64 vpackuswb(Register dst, Register src0, Register src1) {
  // UQXTN then UQXTN2
  ASSERT_MSG(false, "not yet implemented");
  return InstructionARM64(0b0);
}
}  // namespace ARM64
}  // namespace IGen
}  // namespace emitter
