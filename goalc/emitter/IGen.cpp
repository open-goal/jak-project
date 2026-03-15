#include "IGen.h"

#include "IGenARM64.h"
#include "IGenX86.h"
#include "goalc/emitter/ObjectGenerator.h"

#define IGEN_DISPATCH(name, ...)       \
  switch (gen.instr_set()) {           \
    case InstructionSet::X86:          \
      return X86::name(__VA_ARGS__);   \
    case InstructionSet::ARM64:        \
      return ARM64::name(__VA_ARGS__); \
  }

namespace emitter {
namespace IGen {

Instruction mov_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(mov_gpr64_gpr64, dst, src);
}

Instruction mov_gpr64_u64(const ObjectGenerator& gen, Register dst, uint64_t val) {
  IGEN_DISPATCH(mov_gpr64_u64, dst, val);
}

Instruction mov_gpr64_u32(const ObjectGenerator& gen, Register dst, uint64_t val) {
  IGEN_DISPATCH(mov_gpr64_u32, dst, val);
}

Instruction mov_gpr64_s32(const ObjectGenerator& gen, Register dst, int64_t val) {
  IGEN_DISPATCH(mov_gpr64_s32, dst, val);
}

Instruction movd_gpr32_xmm32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(movd_gpr32_xmm32, dst, src);
}

Instruction movd_xmm32_gpr32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(movd_xmm32_gpr32, dst, src);
}

Instruction movq_gpr64_xmm64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(movq_gpr64_xmm64, dst, src);
}

Instruction movq_xmm64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(movq_xmm64_gpr64, dst, src);
}

Instruction mov_xmm32_xmm32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(mov_xmm32_xmm32, dst, src);
}

Instruction load8s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2) {
  IGEN_DISPATCH(load8s_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction store8_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register addr1,
                                          Register addr2,
                                          Register value) {
  IGEN_DISPATCH(store8_gpr64_gpr64_plus_gpr64, addr1, addr2, value);
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  IGEN_DISPATCH(load8s_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register addr1,
                                                  Register addr2,
                                                  Register value,
                                                  s64 offset) {
  IGEN_DISPATCH(store8_gpr64_gpr64_plus_gpr64_plus_s8, addr1, addr2, value, offset);
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load8s_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  IGEN_DISPATCH(store8_gpr64_gpr64_plus_gpr64_plus_s32, addr1, addr2, value, offset);
}

Instruction load8u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2) {
  IGEN_DISPATCH(load8u_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  IGEN_DISPATCH(load8u_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load8u_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction load16s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2) {
  IGEN_DISPATCH(load16s_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction store16_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value) {
  IGEN_DISPATCH(store16_gpr64_gpr64_plus_gpr64, addr1, addr2, value);
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  IGEN_DISPATCH(store16_gpr64_gpr64_plus_gpr64_plus_s8, addr1, addr2, value, offset);
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  IGEN_DISPATCH(store16_gpr64_gpr64_plus_gpr64_plus_s32, addr1, addr2, value, offset);
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load16s_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  IGEN_DISPATCH(load16s_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction load16u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2) {
  IGEN_DISPATCH(load16u_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load16u_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  IGEN_DISPATCH(load16u_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction load32s_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2) {
  IGEN_DISPATCH(load32s_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction store32_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value) {
  IGEN_DISPATCH(store32_gpr64_gpr64_plus_gpr64, addr1, addr2, value);
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load32s_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  IGEN_DISPATCH(store32_gpr64_gpr64_plus_gpr64_plus_s8, addr1, addr2, value, offset);
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  IGEN_DISPATCH(load32s_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  IGEN_DISPATCH(store32_gpr64_gpr64_plus_gpr64_plus_s32, addr1, addr2, value, offset);
}

Instruction load32u_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register dst,
                                           Register addr1,
                                           Register addr2) {
  IGEN_DISPATCH(load32u_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load32u_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  IGEN_DISPATCH(load32u_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction load64_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register dst,
                                          Register addr1,
                                          Register addr2) {
  IGEN_DISPATCH(load64_gpr64_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction store64_gpr64_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register value) {
  IGEN_DISPATCH(store64_gpr64_gpr64_plus_gpr64, addr1, addr2, value);
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  IGEN_DISPATCH(load64_gpr64_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  IGEN_DISPATCH(store64_gpr64_gpr64_plus_gpr64_plus_s8, addr1, addr2, value, offset);
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load64_gpr64_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  IGEN_DISPATCH(store64_gpr64_gpr64_plus_gpr64_plus_s32, addr1, addr2, value, offset);
}

Instruction store_goal_vf(const ObjectGenerator& gen,
                          Register addr,
                          Register value,
                          Register off,
                          s64 offset) {
  IGEN_DISPATCH(store_goal_vf, addr, value, off, offset);
}

Instruction store_goal_gpr(const ObjectGenerator& gen,
                           Register addr,
                           Register value,
                           Register off,
                           int offset,
                           int size) {
  IGEN_DISPATCH(store_goal_gpr, addr, value, off, offset, size);
}

Instruction load_goal_xmm128(const ObjectGenerator& gen,
                             Register dst,
                             Register addr,
                             Register off,
                             int offset) {
  IGEN_DISPATCH(load_goal_xmm128, dst, addr, off, offset);
}

Instruction load_goal_gpr(const ObjectGenerator& gen,
                          Register dst,
                          Register addr,
                          Register off,
                          int offset,
                          int size,
                          bool sign_extend) {
  IGEN_DISPATCH(load_goal_gpr, dst, addr, off, offset, size, sign_extend);
}

Instruction store32_xmm32_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                           Register addr1,
                                           Register addr2,
                                           Register xmm_value) {
  IGEN_DISPATCH(store32_xmm32_gpr64_plus_gpr64, addr1, addr2, xmm_value);
}

Instruction load32_xmm32_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                          Register simd_dest,
                                          Register addr1,
                                          Register addr2) {
  IGEN_DISPATCH(load32_xmm32_gpr64_plus_gpr64, simd_dest, addr1, addr2);
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                   Register addr1,
                                                   Register addr2,
                                                   Register xmm_value,
                                                   s64 offset) {
  IGEN_DISPATCH(store32_xmm32_gpr64_plus_gpr64_plus_s8, addr1, addr2, xmm_value, offset);
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                                  Register simd_dest,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  IGEN_DISPATCH(load32_xmm32_gpr64_plus_gpr64_plus_s8, simd_dest, addr1, addr2, offset);
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                    Register addr1,
                                                    Register addr2,
                                                    Register xmm_value,
                                                    s64 offset) {
  IGEN_DISPATCH(store32_xmm32_gpr64_plus_gpr64_plus_s32, addr1, addr2, xmm_value, offset);
}

Instruction lea_reg_plus_off32(const ObjectGenerator& gen,
                               Register dest,
                               Register base,
                               s64 offset) {
  IGEN_DISPATCH(lea_reg_plus_off32, dest, base, offset);
}

Instruction lea_reg_plus_off8(const ObjectGenerator& gen,
                              Register dest,
                              Register base,
                              s64 offset) {
  IGEN_DISPATCH(lea_reg_plus_off8, dest, base, offset);
}

Instruction lea_reg_plus_off(const ObjectGenerator& gen, Register dest, Register base, s64 offset) {
  IGEN_DISPATCH(lea_reg_plus_off, dest, base, offset);
}

Instruction store32_xmm32_gpr64_plus_s32(const ObjectGenerator& gen,
                                         Register base,
                                         Register xmm_value,
                                         s64 offset) {
  IGEN_DISPATCH(store32_xmm32_gpr64_plus_s32, base, xmm_value, offset);
}

Instruction store32_xmm32_gpr64_plus_s8(const ObjectGenerator& gen,
                                        Register base,
                                        Register xmm_value,
                                        s64 offset) {
  IGEN_DISPATCH(store32_xmm32_gpr64_plus_s8, base, xmm_value, offset);
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                                   Register simd_dest,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  IGEN_DISPATCH(load32_xmm32_gpr64_plus_gpr64_plus_s32, simd_dest, addr1, addr2, offset);
}

Instruction load32_xmm32_gpr64_plus_s32(const ObjectGenerator& gen,
                                        Register simd_dest,
                                        Register base,
                                        s64 offset) {
  IGEN_DISPATCH(load32_xmm32_gpr64_plus_s32, simd_dest, base, offset);
}

Instruction load32_xmm32_gpr64_plus_s8(const ObjectGenerator& gen,
                                       Register simd_dest,
                                       Register base,
                                       s64 offset) {
  IGEN_DISPATCH(load32_xmm32_gpr64_plus_s8, simd_dest, base, offset);
}

Instruction load_goal_xmm32(const ObjectGenerator& gen,
                            Register simd_dest,
                            Register addr,
                            Register off,
                            s64 offset) {
  IGEN_DISPATCH(load_goal_xmm32, simd_dest, addr, off, offset);
}

Instruction store_goal_xmm32(const ObjectGenerator& gen,
                             Register addr,
                             Register xmm_value,
                             Register off,
                             s64 offset) {
  IGEN_DISPATCH(store_goal_xmm32, addr, xmm_value, off, offset);
}

Instruction store_reg_offset_xmm32(const ObjectGenerator& gen,
                                   Register base,
                                   Register xmm_value,
                                   s64 offset) {
  IGEN_DISPATCH(store_reg_offset_xmm32, base, xmm_value, offset);
}

Instruction load_reg_offset_xmm32(const ObjectGenerator& gen,
                                  Register simd_dest,
                                  Register base,
                                  s64 offset) {
  IGEN_DISPATCH(load_reg_offset_xmm32, simd_dest, base, offset);
}

Instruction store128_gpr64_simd128(const ObjectGenerator& gen,
                                   Register gpr_addr,
                                   Register xmm_value) {
  IGEN_DISPATCH(store128_gpr64_simd128, gpr_addr, xmm_value);
}

Instruction store128_gpr64_simd128_s32(const ObjectGenerator& gen,
                                       Register gpr_addr,
                                       Register xmm_value,
                                       s64 offset) {
  IGEN_DISPATCH(store128_gpr64_simd128_s32, gpr_addr, xmm_value, offset);
}

Instruction store128_gpr64_simd128_s8(const ObjectGenerator& gen,
                                      Register gpr_addr,
                                      Register xmm_value,
                                      s64 offset) {
  IGEN_DISPATCH(store128_gpr64_simd128_s8, gpr_addr, xmm_value, offset);
}

Instruction load128_simd128_gpr64(const ObjectGenerator& gen,
                                  Register simd_dest,
                                  Register gpr_addr) {
  IGEN_DISPATCH(load128_simd128_gpr64, simd_dest, gpr_addr);
}

Instruction load128_simd128_gpr64_s32(const ObjectGenerator& gen,
                                      Register simd_dest,
                                      Register gpr_addr,
                                      s64 offset) {
  IGEN_DISPATCH(load128_simd128_gpr64_s32, simd_dest, gpr_addr, offset);
}

Instruction load128_simd128_gpr64_s8(const ObjectGenerator& gen,
                                     Register simd_dest,
                                     Register gpr_addr,
                                     s64 offset) {
  IGEN_DISPATCH(load128_simd128_gpr64_s8, simd_dest, gpr_addr, offset);
}

Instruction load128_xmm128_reg_offset(const ObjectGenerator& gen,
                                      Register simd_dest,
                                      Register base,
                                      s64 offset) {
  IGEN_DISPATCH(load128_xmm128_reg_offset, simd_dest, base, offset);
}

Instruction store128_xmm128_reg_offset(const ObjectGenerator& gen,
                                       Register base,
                                       Register xmm_val,
                                       s64 offset) {
  IGEN_DISPATCH(store128_xmm128_reg_offset, base, xmm_val, offset);
}

Instruction load64_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load64_rip_s32, dest, offset);
}

Instruction load32s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load32s_rip_s32, dest, offset);
}

Instruction load32u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load32u_rip_s32, dest, offset);
}

Instruction load16u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load16u_rip_s32, dest, offset);
}

Instruction load16s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load16s_rip_s32, dest, offset);
}

Instruction load8u_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load8u_rip_s32, dest, offset);
}

Instruction load8s_rip_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(load8s_rip_s32, dest, offset);
}

Instruction static_load(const ObjectGenerator& gen,
                        Register dest,
                        s64 offset,
                        int size,
                        bool sign_extend) {
  IGEN_DISPATCH(static_load, dest, offset, size, sign_extend);
}

Instruction store64_rip_s32(const ObjectGenerator& gen, Register src, s64 offset) {
  IGEN_DISPATCH(store64_rip_s32, src, offset);
}

Instruction store32_rip_s32(const ObjectGenerator& gen, Register src, s64 offset) {
  IGEN_DISPATCH(store32_rip_s32, src, offset);
}

Instruction store16_rip_s32(const ObjectGenerator& gen, Register src, s64 offset) {
  IGEN_DISPATCH(store16_rip_s32, src, offset);
}

Instruction store8_rip_s32(const ObjectGenerator& gen, Register src, s64 offset) {
  IGEN_DISPATCH(store8_rip_s32, src, offset);
}

Instruction static_store(const ObjectGenerator& gen, Register value, s64 offset, int size) {
  IGEN_DISPATCH(static_store, value, offset, size);
}

Instruction static_addr(const ObjectGenerator& gen, Register dst, s64 offset) {
  IGEN_DISPATCH(static_addr, dst, offset);
}

Instruction static_load_xmm32(const ObjectGenerator& gen, Register simd_dest, s64 offset) {
  IGEN_DISPATCH(static_load_xmm32, simd_dest, offset);
}

Instruction static_store_xmm32(const ObjectGenerator& gen, Register xmm_value, s64 offset) {
  IGEN_DISPATCH(static_store_xmm32, xmm_value, offset);
}

Instruction load64_gpr64_plus_s32(const ObjectGenerator& gen,
                                  Register dst_reg,
                                  int32_t offset,
                                  Register src_reg) {
  IGEN_DISPATCH(load64_gpr64_plus_s32, dst_reg, offset, src_reg);
}

Instruction store64_gpr64_plus_s32(const ObjectGenerator& gen,
                                   Register addr,
                                   int32_t offset,
                                   Register value) {
  IGEN_DISPATCH(store64_gpr64_plus_s32, addr, offset, value);
}

Instruction ret(const ObjectGenerator& gen) {
  IGEN_DISPATCH(ret);
}

Instruction push_gpr64(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(push_gpr64, reg);
}

Instruction pop_gpr64(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(pop_gpr64, reg);
}

Instruction call_r64(const ObjectGenerator& gen, Register reg_) {
  IGEN_DISPATCH(call_r64, reg_);
}

Instruction jmp_r64(const ObjectGenerator& gen, Register reg_) {
  IGEN_DISPATCH(jmp_r64, reg_);
}

Instruction sub_gpr64_imm8s(const ObjectGenerator& gen, Register reg, int64_t imm) {
  IGEN_DISPATCH(sub_gpr64_imm8s, reg, imm);
}

Instruction sub_gpr64_imm32s(const ObjectGenerator& gen, Register reg, int64_t imm) {
  IGEN_DISPATCH(sub_gpr64_imm32s, reg, imm);
}

Instruction add_gpr64_imm8s(const ObjectGenerator& gen, Register reg, int64_t v) {
  IGEN_DISPATCH(add_gpr64_imm8s, reg, v);
}

Instruction add_gpr64_imm32s(const ObjectGenerator& gen, Register reg, int64_t v) {
  IGEN_DISPATCH(add_gpr64_imm32s, reg, v);
}

Instruction add_gpr64_imm(const ObjectGenerator& gen, Register reg, int64_t imm) {
  IGEN_DISPATCH(add_gpr64_imm, reg, imm);
}

Instruction sub_gpr64_imm(const ObjectGenerator& gen, Register reg, int64_t imm) {
  IGEN_DISPATCH(sub_gpr64_imm, reg, imm);
}

Instruction add_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(add_gpr64_gpr64, dst, src);
}

Instruction sub_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(sub_gpr64_gpr64, dst, src);
}

Instruction imul_gpr32_gpr32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(imul_gpr32_gpr32, dst, src);
}

Instruction imul_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(imul_gpr64_gpr64, dst, src);
}

Instruction idiv_gpr32(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(idiv_gpr32, reg);
}

Instruction unsigned_div_gpr32(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(unsigned_div_gpr32, reg);
}

Instruction cdq(const ObjectGenerator& gen) {
  IGEN_DISPATCH(cdq);
}

Instruction movsx_r64_r32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(movsx_r64_r32, dst, src);
}

Instruction cmp_gpr64_gpr64(const ObjectGenerator& gen, Register a, Register b) {
  IGEN_DISPATCH(cmp_gpr64_gpr64, a, b);
}

Instruction or_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(or_gpr64_gpr64, dst, src);
}

Instruction and_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(and_gpr64_gpr64, dst, src);
}

Instruction xor_gpr64_gpr64(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(xor_gpr64_gpr64, dst, src);
}

Instruction not_gpr64(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(not_gpr64, reg);
}

Instruction shl_gpr64_cl(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(shl_gpr64_cl, reg);
}

Instruction shr_gpr64_cl(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(shr_gpr64_cl, reg);
}

Instruction sar_gpr64_cl(const ObjectGenerator& gen, Register reg) {
  IGEN_DISPATCH(sar_gpr64_cl, reg);
}

Instruction shl_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa) {
  IGEN_DISPATCH(shl_gpr64_u8, reg, sa);
}

Instruction shr_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa) {
  IGEN_DISPATCH(shr_gpr64_u8, reg, sa);
}

Instruction sar_gpr64_u8(const ObjectGenerator& gen, Register reg, uint8_t sa) {
  IGEN_DISPATCH(sar_gpr64_u8, reg, sa);
}

Instruction jmp_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jmp_32);
}

Instruction je_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(je_32);
}

Instruction jne_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jne_32);
}

Instruction jle_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jle_32);
}

Instruction jge_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jge_32);
}

Instruction jl_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jl_32);
}

Instruction jg_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jg_32);
}

Instruction jbe_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jbe_32);
}

Instruction jae_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jae_32);
}

Instruction jb_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(jb_32);
}

Instruction ja_32(const ObjectGenerator& gen) {
  IGEN_DISPATCH(ja_32);
}

Instruction cmp_flt_flt(const ObjectGenerator& gen, Register a, Register b) {
  IGEN_DISPATCH(cmp_flt_flt, a, b);
}

Instruction sqrts_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(sqrts_xmm, dst, src);
}

Instruction mulss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(mulss_xmm_xmm, dst, src);
}

Instruction divss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(divss_xmm_xmm, dst, src);
}

Instruction subss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(subss_xmm_xmm, dst, src);
}

Instruction addss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(addss_xmm_xmm, dst, src);
}

Instruction minss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(minss_xmm_xmm, dst, src);
}

Instruction maxss_xmm_xmm(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(maxss_xmm_xmm, dst, src);
}

Instruction int32_to_float(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(int32_to_float, dst, src);
}

Instruction float_to_int32(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(float_to_int32, dst, src);
}

Instruction nop(const ObjectGenerator& gen) {
  IGEN_DISPATCH(nop);
}

Instruction null(const ObjectGenerator& gen) {
  IGEN_DISPATCH(null);
}

Instruction nop_vf(const ObjectGenerator& gen) {
  IGEN_DISPATCH(nop_vf);
}

Instruction wait_vf(const ObjectGenerator& gen) {
  IGEN_DISPATCH(wait_vf);
}

Instruction mov_vf_vf(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(mov_vf_vf, dst, src);
}

Instruction loadvf_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                    Register dst,
                                    Register addr1,
                                    Register addr2) {
  IGEN_DISPATCH(loadvf_gpr64_plus_gpr64, dst, addr1, addr2);
}

Instruction loadvf_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                            Register dst,
                                            Register addr1,
                                            Register addr2,
                                            s64 offset) {
  IGEN_DISPATCH(loadvf_gpr64_plus_gpr64_plus_s8, dst, addr1, addr2, offset);
}

Instruction loadvf_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                             Register dst,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  IGEN_DISPATCH(loadvf_gpr64_plus_gpr64_plus_s32, dst, addr1, addr2, offset);
}

Instruction storevf_gpr64_plus_gpr64(const ObjectGenerator& gen,
                                     Register value,
                                     Register addr1,
                                     Register addr2) {
  IGEN_DISPATCH(storevf_gpr64_plus_gpr64, value, addr1, addr2);
}

Instruction storevf_gpr64_plus_gpr64_plus_s8(const ObjectGenerator& gen,
                                             Register value,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  IGEN_DISPATCH(storevf_gpr64_plus_gpr64_plus_s8, value, addr1, addr2, offset);
}

Instruction storevf_gpr64_plus_gpr64_plus_s32(const ObjectGenerator& gen,
                                              Register value,
                                              Register addr1,
                                              Register addr2,
                                              s64 offset) {
  IGEN_DISPATCH(storevf_gpr64_plus_gpr64_plus_s32, value, addr1, addr2, offset);
}

Instruction loadvf_rip_plus_s32(const ObjectGenerator& gen, Register dest, s64 offset) {
  IGEN_DISPATCH(loadvf_rip_plus_s32, dest, offset);
}

Instruction blend_vf(const ObjectGenerator& gen,
                     Register dst,
                     Register src1,
                     Register src2,
                     u8 mask) {
  IGEN_DISPATCH(blend_vf, dst, src1, src2, mask);
}

Instruction
shuffle_vf(const ObjectGenerator& gen, Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  IGEN_DISPATCH(shuffle_vf, dst, src, dx, dy, dz, dw);
}

Instruction swizzle_vf(const ObjectGenerator& gen, Register dst, Register src, u8 controlBytes) {
  IGEN_DISPATCH(swizzle_vf, dst, src, controlBytes);
}

Instruction splat_vf(const ObjectGenerator& gen,
                     Register dst,
                     Register src,
                     Register::VF_ELEMENT element) {
  IGEN_DISPATCH(splat_vf, dst, src, element);
}

Instruction xor_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(xor_vf, dst, src1, src2);
}

Instruction sub_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(sub_vf, dst, src1, src2);
}

Instruction add_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(add_vf, dst, src1, src2);
}

Instruction mul_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(mul_vf, dst, src1, src2);
}

Instruction max_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(max_vf, dst, src1, src2);
}

Instruction min_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(min_vf, dst, src1, src2);
}

Instruction div_vf(const ObjectGenerator& gen, Register dst, Register src1, Register src2) {
  IGEN_DISPATCH(div_vf, dst, src1, src2);
}

Instruction sqrt_vf(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(sqrt_vf, dst, src);
}

Instruction itof_vf(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(itof_vf, dst, src);
}

Instruction ftoi_vf(const ObjectGenerator& gen, Register dst, Register src) {
  IGEN_DISPATCH(ftoi_vf, dst, src);
}

Instruction pw_sra(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(pw_sra, dst, src, imm);
}

Instruction pw_srl(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(pw_srl, dst, src, imm);
}

Instruction ph_srl(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(ph_srl, dst, src, imm);
}

Instruction pw_sll(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(pw_sll, dst, src, imm);
}

Instruction ph_sll(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(ph_sll, dst, src, imm);
}

Instruction parallel_add_byte(const ObjectGenerator& gen,
                              Register dst,
                              Register src0,
                              Register src1) {
  IGEN_DISPATCH(parallel_add_byte, dst, src0, src1);
}

Instruction parallel_bitwise_or(const ObjectGenerator& gen,
                                Register dst,
                                Register src0,
                                Register src1) {
  IGEN_DISPATCH(parallel_bitwise_or, dst, src0, src1);
}

Instruction parallel_bitwise_xor(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1) {
  IGEN_DISPATCH(parallel_bitwise_xor, dst, src0, src1);
}

Instruction parallel_bitwise_and(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1) {
  IGEN_DISPATCH(parallel_bitwise_and, dst, src0, src1);
}

Instruction pextub_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextub_swapped, dst, src0, src1);
}

Instruction pextuh_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextuh_swapped, dst, src0, src1);
}

Instruction pextuw_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextuw_swapped, dst, src0, src1);
}

Instruction pextlb_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextlb_swapped, dst, src0, src1);
}

Instruction pextlh_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextlh_swapped, dst, src0, src1);
}

Instruction pextlw_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pextlw_swapped, dst, src0, src1);
}

Instruction parallel_compare_e_b(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1) {
  IGEN_DISPATCH(parallel_compare_e_b, dst, src0, src1);
}

Instruction parallel_compare_e_h(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1) {
  IGEN_DISPATCH(parallel_compare_e_h, dst, src0, src1);
}

Instruction parallel_compare_e_w(const ObjectGenerator& gen,
                                 Register dst,
                                 Register src0,
                                 Register src1) {
  IGEN_DISPATCH(parallel_compare_e_w, dst, src0, src1);
}

Instruction parallel_compare_gt_b(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1) {
  IGEN_DISPATCH(parallel_compare_gt_b, dst, src0, src1);
}

Instruction parallel_compare_gt_h(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1) {
  IGEN_DISPATCH(parallel_compare_gt_h, dst, src0, src1);
}

Instruction parallel_compare_gt_w(const ObjectGenerator& gen,
                                  Register dst,
                                  Register src0,
                                  Register src1) {
  IGEN_DISPATCH(parallel_compare_gt_w, dst, src0, src1);
}

Instruction vpunpcklqdq(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(vpunpcklqdq, dst, src0, src1);
}

Instruction pcpyld_swapped(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pcpyld_swapped, dst, src0, src1);
}

Instruction pcpyud(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(pcpyud, dst, src0, src1);
}

Instruction vpsubd(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(vpsubd, dst, src0, src1);
}

Instruction vpsrldq(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(vpsrldq, dst, src, imm);
}

Instruction vpslldq(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(vpslldq, dst, src, imm);
}

Instruction vpshuflw(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(vpshuflw, dst, src, imm);
}

Instruction vpshufhw(const ObjectGenerator& gen, Register dst, Register src, u8 imm) {
  IGEN_DISPATCH(vpshufhw, dst, src, imm);
}

Instruction vpackuswb(const ObjectGenerator& gen, Register dst, Register src0, Register src1) {
  IGEN_DISPATCH(vpackuswb, dst, src0, src1);
}

};  // namespace IGen
};  // namespace emitter
