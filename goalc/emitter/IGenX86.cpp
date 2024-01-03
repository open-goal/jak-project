#ifndef __aarch64__

#include "IGen.h"

namespace emitter {
namespace IGen {
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   MOVES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction mov_gpr64_gpr64(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

Instruction mov_gpr64_u64(Register dst, uint64_t val) {
  ASSERT(dst.is_gpr());
  bool rex_b = false;
  auto dst_hw_id = dst.hw_id();
  if (dst_hw_id >= 8) {
    dst_hw_id -= 8;
    rex_b = true;
  }
  InstructionX86 instr(0xb8 + dst_hw_id);
  instr.set(REX(true, false, false, rex_b));
  instr.set(Imm(8, val));
  return instr;
}

Instruction mov_gpr64_u32(Register dst, uint64_t val) {
  ASSERT(val <= UINT32_MAX);
  ASSERT(dst.is_gpr());
  auto dst_hw_id = dst.hw_id();
  bool rex_b = false;
  if (dst_hw_id >= 8) {
    dst_hw_id -= 8;
    rex_b = true;
  }

  InstructionX86 instr(0xb8 + dst_hw_id);
  if (rex_b) {
    instr.set(REX(false, false, false, rex_b));
  }
  instr.set(Imm(4, val));
  return instr;
}

Instruction mov_gpr64_s32(Register dst, int64_t val) {
  ASSERT(val >= INT32_MIN && val <= INT32_MAX);
  ASSERT(dst.is_gpr());
  InstructionX86 instr(0xc7);
  instr.set_modrm_and_rex(0, dst.hw_id(), 3, true);
  instr.set(Imm(4, val));
  return instr;
}

Instruction movd_gpr32_xmm32(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x7e);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction movd_xmm32_gpr32(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x6e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction movq_gpr64_xmm64(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x7e);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  instr.swap_op0_rex();
  return instr;
}

Instruction movq_xmm64_gpr64(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x6e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  instr.swap_op0_rex();
  return instr;
}

Instruction mov_xmm32_xmm32(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

// todo - GPR64 -> XMM64 (zext)
// todo - XMM -> GPR64

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   GOAL Loads and Stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbe);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                false);
  return instr;
}

Instruction store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x88);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
  if (value.id() > RBX) {
    instr.add_rex();
  }
  return instr;
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbe);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                  Register addr2,
                                                  Register value,
                                                  s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x88);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);
  if (value.id() > RBX) {
    instr.add_rex();
  }
  return instr;
}

Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbe);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x88);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);
  if (value.id() > RBX) {
    instr.add_rex();
  }
  return instr;
}

Instruction load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb6);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                false);
  return instr;
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb6);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb6);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbf);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                false);
  return instr;
}

Instruction store16_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
  instr.swap_op0_rex();  // why?????
  return instr;
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);
  instr.swap_op0_rex();  // why?????
  return instr;
}

Instruction store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);
  instr.swap_op0_rex();  // why?????
  return instr;
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbf);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbf);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb7);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                false);
  return instr;
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb7);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb7);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x63);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true);
  return instr;
}

Instruction store32_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
  return instr;
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x63);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);
  return instr;
}

Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x63);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction store32_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);
  return instr;
}

Instruction load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id());
  return instr;
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);
  return instr;
}

Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                    Register addr1,
                                                    Register addr2,
                                                    s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);
  return instr;
}

Instruction load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true);
  return instr;
}

Instruction store64_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id(), true);
  return instr;
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register value,
                                                   s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, true);
  return instr;
}

Instruction load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction store64_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register value,
                                                    s64 offset) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, true);
  return instr;
}

Instruction store_goal_vf(Register addr, Register value, Register off, s64 offset) {
  if (offset == 0) {
    return storevf_gpr64_plus_gpr64(value, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return storevf_gpr64_plus_gpr64_plus_s8(value, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return storevf_gpr64_plus_gpr64_plus_s32(value, addr, off, offset);
  }
  ASSERT(false);
  return InstructionX86(0);
}

Instruction store_goal_gpr(Register addr, Register value, Register off, int offset, int size) {
  switch (size) {
    case 1:
      if (offset == 0) {
        return store8_gpr64_gpr64_plus_gpr64(addr, off, value);
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        return store8_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        return store8_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
      } else {
        ASSERT(false);
      }
    case 2:
      if (offset == 0) {
        return store16_gpr64_gpr64_plus_gpr64(addr, off, value);
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        return store16_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        return store16_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
      } else {
        ASSERT(false);
      }
    case 4:
      if (offset == 0) {
        return store32_gpr64_gpr64_plus_gpr64(addr, off, value);
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        return store32_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        return store32_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
      } else {
        ASSERT(false);
      }
    case 8:
      if (offset == 0) {
        return store64_gpr64_gpr64_plus_gpr64(addr, off, value);
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        return store64_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        return store64_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
      } else {
        ASSERT(false);
      }
    default:
      ASSERT(false);
      return InstructionX86(0);
  }
}

Instruction load_goal_xmm128(Register dst, Register addr, Register off, int offset) {
  if (offset == 0) {
    return loadvf_gpr64_plus_gpr64(dst, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return loadvf_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return loadvf_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction load_goal_gpr(Register dst,
                          Register addr,
                          Register off,
                          int offset,
                          int size,
                          bool sign_extend) {
  switch (size) {
    case 1:
      if (offset == 0) {
        if (sign_extend) {
          return load8s_gpr64_gpr64_plus_gpr64(dst, addr, off);
        } else {
          return load8u_gpr64_gpr64_plus_gpr64(dst, addr, off);
        }
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        if (sign_extend) {
          return load8s_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        } else {
          return load8u_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        }
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        if (sign_extend) {
          return load8s_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        } else {
          return load8u_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        }
      } else {
        ASSERT(false);
      }
    case 2:
      if (offset == 0) {
        if (sign_extend) {
          return load16s_gpr64_gpr64_plus_gpr64(dst, addr, off);
        } else {
          return load16u_gpr64_gpr64_plus_gpr64(dst, addr, off);
        }
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        if (sign_extend) {
          return load16s_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        } else {
          return load16u_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        }
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        if (sign_extend) {
          return load16s_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        } else {
          return load16u_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        }
      } else {
        ASSERT(false);
      }
    case 4:
      if (offset == 0) {
        if (sign_extend) {
          return load32s_gpr64_gpr64_plus_gpr64(dst, addr, off);
        } else {
          return load32u_gpr64_gpr64_plus_gpr64(dst, addr, off);
        }
      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        if (sign_extend) {
          return load32s_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        } else {
          return load32u_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
        }
      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        if (sign_extend) {
          return load32s_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        } else {
          return load32u_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
        }
      } else {
        ASSERT(false);
      }
    case 8:
      if (offset == 0) {
        return load64_gpr64_gpr64_plus_gpr64(dst, addr, off);

      } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
        return load64_gpr64_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);

      } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
        return load64_gpr64_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);

      } else {
        ASSERT(false);
      }
    default:
      ASSERT(false);
      return InstructionX86(0);
  }
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM32
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction store32_xmm32_gpr64_plus_gpr64(Register addr1, Register addr2, Register xmm_value) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(xmm_value.hw_id(), addr1.hw_id(), addr2.hw_id());

  instr.swap_op0_rex();
  return instr;
}

Instruction load32_xmm32_gpr64_plus_gpr64(Register xmm_dest, Register addr1, Register addr2) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(xmm_dest.hw_id(), addr1.hw_id(), addr2.hw_id());

  instr.swap_op0_rex();
  return instr;
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                   Register addr2,
                                                   Register xmm_value,
                                                   s64 offset) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(xmm_value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);

  instr.swap_op0_rex();
  return instr;
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s8(Register xmm_dest,
                                                  Register addr1,
                                                  Register addr2,
                                                  s64 offset) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(xmm_dest.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                   offset, false);

  instr.swap_op0_rex();
  return instr;
}

Instruction store32_xmm32_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                    Register addr2,
                                                    Register xmm_value,
                                                    s64 offset) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(xmm_value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);

  instr.swap_op0_rex();
  return instr;
}

Instruction lea_reg_plus_off32(Register dest, Register base, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dest.hw_id(), 2, base.hw_id(), true);
  instr.set(Imm(4, offset));
  return instr;
}

Instruction lea_reg_plus_off8(Register dest, Register base, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dest.hw_id(), 1, base.hw_id(), true);
  instr.set(Imm(1, offset));
  return instr;
}

Instruction lea_reg_plus_off(Register dest, Register base, s64 offset) {
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return lea_reg_plus_off8(dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return lea_reg_plus_off32(dest, base, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction store32_xmm32_gpr64_plus_s32(Register base, Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 2, base.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction store32_xmm32_gpr64_plus_s8(Register base, Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 1, base.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load32_xmm32_gpr64_plus_gpr64_plus_s32(Register xmm_dest,
                                                   Register addr1,
                                                   Register addr2,
                                                   s64 offset) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(xmm_dest.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    offset, false);

  instr.swap_op0_rex();
  return instr;
}

Instruction load32_xmm32_gpr64_plus_s32(Register xmm_dest, Register base, s64 offset) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 2, base.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load32_xmm32_gpr64_plus_s8(Register xmm_dest, Register base, s64 offset) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 1, base.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load_goal_xmm32(Register xmm_dest, Register addr, Register off, s64 offset) {
  if (offset == 0) {
    return load32_xmm32_gpr64_plus_gpr64(xmm_dest, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load32_xmm32_gpr64_plus_gpr64_plus_s8(xmm_dest, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load32_xmm32_gpr64_plus_gpr64_plus_s32(xmm_dest, addr, off, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction store_goal_xmm32(Register addr, Register xmm_value, Register off, s64 offset) {
  if (offset == 0) {
    return store32_xmm32_gpr64_plus_gpr64(addr, off, xmm_value);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store32_xmm32_gpr64_plus_gpr64_plus_s8(addr, off, xmm_value, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store32_xmm32_gpr64_plus_gpr64_plus_s32(addr, off, xmm_value, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction store_reg_offset_xmm32(Register base, Register xmm_value, s64 offset) {
  ASSERT(base.is_gpr());
  ASSERT(xmm_value.is_128bit_simd());
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store32_xmm32_gpr64_plus_s8(base, xmm_value, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store32_xmm32_gpr64_plus_s32(base, xmm_value, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction load_reg_offset_xmm32(Register xmm_dest, Register base, s64 offset) {
  ASSERT(base.is_gpr());
  ASSERT(xmm_dest.is_128bit_simd());
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load32_xmm32_gpr64_plus_s8(xmm_dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load32_xmm32_gpr64_plus_s32(xmm_dest, base, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   LOADS n' STORES - XMM128
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction store128_gpr64_xmm128(Register gpr_addr, Register xmm_value) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_128bit_simd());
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_and_rex_for_reg_addr(xmm_value.hw_id(), gpr_addr.hw_id(), false);
  instr.swap_op0_rex();
  return instr;
}

Instruction store128_gpr64_xmm128_s32(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 2, gpr_addr.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction store128_gpr64_xmm128_s8(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 1, gpr_addr.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_128bit_simd());
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_and_rex_for_reg_addr(xmm_dest.hw_id(), gpr_addr.hw_id(), false);
  instr.swap_op0_rex();
  return instr;
}

Instruction load128_xmm128_gpr64_s32(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 2, gpr_addr.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load128_xmm128_gpr64_s8(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x66);
  //    InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 1, gpr_addr.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

Instruction load128_xmm128_reg_offset(Register xmm_dest, Register base, s64 offset) {
  if (offset == 0) {
    return load128_xmm128_gpr64(xmm_dest, base);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load128_xmm128_gpr64_s8(xmm_dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load128_xmm128_gpr64_s32(xmm_dest, base, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

Instruction store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset) {
  if (offset == 0) {
    return store128_gpr64_xmm128(base, xmm_val);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store128_gpr64_xmm128_s8(base, xmm_val, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store128_gpr64_xmm128_s32(base, xmm_val, offset);
  } else {
    ASSERT(false);
    return InstructionX86(0);
  }
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   RIP loads and stores
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction load64_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction load32s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x63);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction load32u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, false);
  return instr;
}

Instruction load16u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb7);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction load16s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbf);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction load8u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb6);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction load8s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbe);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

Instruction static_load(Register dest, s64 offset, int size, bool sign_extend) {
  switch (size) {
    case 1:
      if (sign_extend) {
        return load8s_rip_s32(dest, offset);
      } else {
        return load8u_rip_s32(dest, offset);
      }
      break;
    case 2:
      if (sign_extend) {
        return load16s_rip_s32(dest, offset);
      } else {
        return load16u_rip_s32(dest, offset);
      }
      break;
    case 4:
      if (sign_extend) {
        return load32s_rip_s32(dest, offset);
      } else {
        return load32u_rip_s32(dest, offset);
      }
      break;
    case 8:
      return load64_rip_s32(dest, offset);
    default:
      ASSERT(false);
  }
}

Instruction store64_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, true);
  return instr;
}

Instruction store32_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, false);
  return instr;
}

Instruction store16_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction store8_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x88);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, false);
  if (src.id() > RBX) {
    instr.add_rex();
  }
  return instr;
}

Instruction static_store(Register value, s64 offset, int size) {
  switch (size) {
    case 1:
      return store8_rip_s32(value, offset);
    case 2:
      return store16_rip_s32(value, offset);
    case 4:
      return store32_rip_s32(value, offset);
    case 8:
      return store64_rip_s32(value, offset);
    default:
      ASSERT(false);
  }
}

Instruction static_addr(Register dst, s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_and_rex_for_rip_plus_s32(dst.hw_id(), offset, true);
  return instr;
}

Instruction static_load_xmm32(Register xmm_dest, s64 offset) {
  ASSERT(xmm_dest.is_128bit_simd());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_rip_plus_s32(xmm_dest.hw_id(), offset, false);

  instr.swap_op0_rex();
  return instr;
}

Instruction static_store_xmm32(Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_128bit_simd());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_rip_plus_s32(xmm_value.hw_id(), offset, false);

  instr.swap_op0_rex();
  return instr;
}

// TODO, special load/stores of 128 bit values.

// TODO, consider specialized stack loads and stores?
Instruction load64_gpr64_plus_s32(Register dst_reg, int32_t offset, Register src_reg) {
  ASSERT(dst_reg.is_gpr());
  ASSERT(src_reg.is_gpr());
  InstructionX86 instr(0x8b);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dst_reg.hw_id(), 2, src_reg.hw_id(), true);
  instr.set_disp(Imm(4, offset));
  return instr;
}

Instruction store64_gpr64_plus_s32(Register addr, int32_t offset, Register value) {
  ASSERT(addr.is_gpr());
  ASSERT(value.is_gpr());
  InstructionX86 instr(0x89);
  instr.set_modrm_rex_sib_for_reg_reg_disp(value.hw_id(), 2, addr.hw_id(), true);
  instr.set_disp(Imm(4, offset));
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FUNCTION STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction ret() {
  return InstructionX86(0xc3);
}

Instruction push_gpr64(Register reg) {
  ASSERT(reg.is_gpr());
  if (reg.hw_id() >= 8) {
    auto i = InstructionX86(0x50 + reg.hw_id() - 8);
    i.set(REX(false, false, false, true));
    return i;
  }
  return InstructionX86(0x50 + reg.hw_id());
}

Instruction pop_gpr64(Register reg) {
  ASSERT(reg.is_gpr());
  if (reg.hw_id() >= 8) {
    auto i = InstructionX86(0x58 + reg.hw_id() - 8);
    i.set(REX(false, false, false, true));
    return i;
  }
  return InstructionX86(0x58 + reg.hw_id());
}

Instruction call_r64(Register reg_) {
  ASSERT(reg_.is_gpr());
  auto reg = reg_.hw_id();
  InstructionX86 instr(0xff);
  if (reg >= 8) {
    instr.set(REX(false, false, false, true));
    reg -= 8;
  }
  ASSERT(reg < 8);
  ModRM mrm;
  mrm.rm = reg;
  mrm.reg_op = 2;
  mrm.mod = 3;
  instr.set(mrm);
  return instr;
}

Instruction jmp_r64(Register reg_) {
  ASSERT(reg_.is_gpr());
  auto reg = reg_.hw_id();
  InstructionX86 instr(0xff);
  if (reg >= 8) {
    instr.set(REX(false, false, false, true));
    reg -= 8;
  }
  ASSERT(reg < 8);
  ModRM mrm;
  mrm.rm = reg;
  mrm.reg_op = 4;
  mrm.mod = 3;
  instr.set(mrm);
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   INTEGER MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Instruction sub_gpr64_imm8s(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr());
  ASSERT(imm >= INT8_MIN && imm <= INT8_MAX);
  // SUB r/m64, imm8 : REX.W + 83 /5 ib
  InstructionX86 instr(0x83);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction sub_gpr64_imm32s(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr());
  ASSERT(imm >= INT32_MIN && imm <= INT32_MAX);
  InstructionX86 instr(0x81);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(4, imm));
  return instr;
}

Instruction add_gpr64_imm8s(Register reg, int64_t v) {
  ASSERT(v >= INT8_MIN && v <= INT8_MAX);
  InstructionX86 instr(0x83);
  instr.set_modrm_and_rex(0, reg.hw_id(), 3, true);
  instr.set(Imm(1, v));
  return instr;
}

Instruction add_gpr64_imm32s(Register reg, int64_t v) {
  ASSERT(v >= INT32_MIN && v <= INT32_MAX);
  InstructionX86 instr(0x81);
  instr.set_modrm_and_rex(0, reg.hw_id(), 3, true);
  instr.set(Imm(4, v));
  return instr;
}

Instruction add_gpr64_imm(Register reg, int64_t imm) {
  if (imm >= INT8_MIN && imm <= INT8_MAX) {
    return add_gpr64_imm8s(reg, imm);
  } else if (imm >= INT32_MIN && imm <= INT32_MAX) {
    return add_gpr64_imm32s(reg, imm);
  } else {
    throw std::runtime_error("Invalid `add` with reg[" + reg.print() + "]/imm[" +
                             std::to_string(imm) + "]");
  }
}

Instruction sub_gpr64_imm(Register reg, int64_t imm) {
  if (imm >= INT8_MIN && imm <= INT8_MAX) {
    return sub_gpr64_imm8s(reg, imm);
  } else if (imm >= INT32_MIN && imm <= INT32_MAX) {
    return sub_gpr64_imm32s(reg, imm);
  } else {
    throw std::runtime_error("Invalid `sub` with reg[" + reg.print() + "]/imm[" +
                             std::to_string(imm) + "]");
  }
}

Instruction add_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x01);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

Instruction sub_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x29);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

Instruction imul_gpr32_gpr32(Register dst, Register src) {
  InstructionX86 instr(0xf);
  instr.set_op2(0xaf);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  return instr;
}

Instruction imul_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0xf);
  instr.set_op2(0xaf);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

Instruction idiv_gpr32(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, false);
  return instr;
}

Instruction unsigned_div_gpr32(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(6, reg.hw_id(), 3, false);
  return instr;
}

Instruction cdq() {
  InstructionX86 instr(0x99);
  return instr;
}

Instruction movsx_r64_r32(Register dst, Register src) {
  InstructionX86 instr(0x63);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

Instruction cmp_gpr64_gpr64(Register a, Register b) {
  InstructionX86 instr(0x3b);
  ASSERT(a.is_gpr());
  ASSERT(b.is_gpr());
  instr.set_modrm_and_rex(a.hw_id(), b.hw_id(), 3, true);
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   BIT STUFF
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction or_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x0b);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

Instruction and_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x23);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

Instruction xor_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x33);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

Instruction not_gpr64(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(2, reg.hw_id(), 3, true);
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   SHIFTS
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction shl_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(4, reg.hw_id(), 3, true);
  return instr;
}

Instruction shr_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  return instr;
}

Instruction sar_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, true);
  return instr;
}

Instruction shl_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(4, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

Instruction shr_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

Instruction sar_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   CONTROL FLOW
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction jmp_32() {
  InstructionX86 instr(0xe9);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction je_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x84);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jne_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x85);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jle_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8e);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jge_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8d);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jl_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8c);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jg_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8f);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jbe_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x86);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jae_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x83);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction jb_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x82);
  instr.set(Imm(4, 0));
  return instr;
}

Instruction ja_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x87);
  instr.set(Imm(4, 0));
  return instr;
}

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   FLOAT MATH
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction cmp_flt_flt(Register a, Register b) {
  ASSERT(a.is_128bit_simd());
  ASSERT(b.is_128bit_simd());
  InstructionX86 instr(0x0f);
  instr.set_op2(0x2e);
  instr.set_modrm_and_rex(a.hw_id(), b.hw_id(), 3, false);
  return instr;
}

Instruction sqrts_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x51);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction mulss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x59);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction divss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction subss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5c);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction addss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x58);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction minss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5d);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction maxss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5f);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction int32_to_float(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x2a);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction float_to_int32(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x2c);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

Instruction nop() {
  // NOP
  InstructionX86 instr(0x90);
  return instr;
}

// TODO - rsqrt / abs / sqrt

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//   UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Instruction null() {
  InstructionX86 i(0);
  i.m_flags |= InstructionX86::kIsNull;
  return i;
}

/////////////////////////////
// AVX (VF - Vector Float) //
/////////////////////////////

Instruction nop_vf() {
  InstructionX86 instr(0xd9);  // FNOP
  instr.set_op2(0xd0);
  return instr;
}

Instruction wait_vf() {
  InstructionX86 instr(0x9B);  // FWAIT / WAIT
  return instr;
}

Instruction mov_vf_vf(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());

  if (src.hw_id() >= 8 && dst.hw_id() < 8) {
    // in this case, we can use the 0x29 encoding, which swaps src and dst, in order to use the
    // 2 byte VEX prefix, where the 0x28 encoding would require an extra byte.
    // compilers/assemblers seem to prefer 0x28, unless 0x29 would save you a byte.
    InstructionX86 instr(0x29);
    instr.set_vex_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, VEX3::LeadingBytes::P_0F, false);
    return instr;
  } else {
    InstructionX86 instr(0x28);
    instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, VEX3::LeadingBytes::P_0F, false);
    return instr;
  }
}

Instruction loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x28);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction loadvf_gpr64_plus_gpr64_plus_s8(Register dst,
                                            Register addr1,
                                            Register addr2,
                                            s64 offset) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x28);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                       offset, VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction loadvf_gpr64_plus_gpr64_plus_s32(Register dst,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x28);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                        offset, VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction storevf_gpr64_plus_gpr64(Register value, Register addr1, Register addr2) {
  ASSERT(value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x29);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                    VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction storevf_gpr64_plus_gpr64_plus_s8(Register value,
                                             Register addr1,
                                             Register addr2,
                                             s64 offset) {
  ASSERT(value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x29);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                       offset, VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction storevf_gpr64_plus_gpr64_plus_s32(Register value,
                                              Register addr1,
                                              Register addr2,
                                              s64 offset) {
  ASSERT(value.is_128bit_simd());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x29);
  instr.set_vex_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                        offset, VEX3::LeadingBytes::P_0F, false);
  return instr;
}

Instruction loadvf_rip_plus_s32(Register dest, s64 offset) {
  ASSERT(dest.is_128bit_simd());
  ASSERT(offset >= INT32_MIN);
  ASSERT(offset <= INT32_MAX);
  InstructionX86 instr(0x28);
  instr.set_vex_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset);
  return instr;
}

// TODO - rip relative loads and stores.

Instruction blend_vf(Register dst, Register src1, Register src2, u8 mask) {
  ASSERT(!(mask & 0b11110000));
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x0c);  // VBLENDPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F_3A, src1.hw_id(),
                              false, VexPrefix::P_66);
  instr.set(Imm(1, mask));
  return instr;
}

Instruction shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  ASSERT(dx < 4);
  ASSERT(dy < 4);
  ASSERT(dz < 4);
  ASSERT(dw < 4);
  u8 imm = dx + (dy << 2) + (dz << 4) + (dw << 6);
  return swizzle_vf(dst, src, imm);

  // SSE encoding version:
  //    InstructionX86 instr(0x0f);
  //    instr.set_op2(0xc6);
  //    instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  //    instr.set(Imm(1, imm));
  //    return instr;
}

Instruction swizzle_vf(Register dst, Register src, u8 controlBytes) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0xC6);  // VSHUFPS

  // we use the AVX "VEX" encoding here. This is a three-operand form,
  // but we just set both source
  // to the same register. It seems like this is one byte longer but is faster maybe?
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, src.hw_id());
  instr.set(Imm(1, controlBytes));
  return instr;
}

Instruction splat_vf(Register dst, Register src, Register::VF_ELEMENT element) {
  switch (element) {
    case Register::VF_ELEMENT::X:  // Least significant element
      return swizzle_vf(dst, src, 0b00000000);
      break;
    case Register::VF_ELEMENT::Y:
      return swizzle_vf(dst, src, 0b01010101);
      break;
    case Register::VF_ELEMENT::Z:
      return swizzle_vf(dst, src, 0b10101010);
      break;
    case Register::VF_ELEMENT::W:  // Most significant element
      return swizzle_vf(dst, src, 0b11111111);
      break;
    default:
      ASSERT(false);
      return InstructionX86(0);
  }
}

Instruction xor_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x57);  // VXORPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction sub_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x5c);  // VSUBPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction add_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x58);  // VADDPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction mul_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x59);  // VMULPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction max_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x5F);  // VMAXPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction min_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x5D);  // VMINPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction div_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  ASSERT(src2.is_128bit_simd());
  InstructionX86 instr(0x5E);  // VDIVPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

Instruction sqrt_vf(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0x51);  // VSQRTPS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0b0);
  return instr;
}

Instruction itof_vf(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  InstructionX86 instr(0x5b);  // VCVTDQ2PS
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0);
  return instr;
}

Instruction ftoi_vf(Register dst, Register src) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.F3.0F.WIG 5B /r VCVTTPS2DQ xmm1, xmm2/m128
  InstructionX86 instr(0x5b);  // VCVTTPS2DQ
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F3);
  return instr;
}

Instruction pw_sra(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 72 /4 ib VPSRAD xmm1, xmm2, imm8
  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(4, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction pw_srl(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 72 /2 ib VPSRLD xmm1, xmm2, imm8
  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(2, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction ph_srl(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 71 /2 ib VPSRLW
  InstructionX86 instr(0x71);
  instr.set_vex_modrm_and_rex(2, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction pw_sll(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 72 /6 ib VPSLLD xmm1, xmm2, imm8
  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(6, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}
Instruction ph_sll(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 71 /6 ib VPSLLW xmm1, xmm2, imm8
  InstructionX86 instr(0x71);
  instr.set_vex_modrm_and_rex(6, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction parallel_add_byte(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG FC /r VPADDB xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0xFC);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_bitwise_or(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG EB /r VPOR xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0xEB);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_bitwise_xor(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG EF /r VPXOR xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0xEF);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_bitwise_and(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG DB /r VPAND xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0xDB);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextub_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 68/r VPUNPCKHBW xmm1,xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x68);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextuh_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 69/r VPUNPCKHWD xmm1,xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x69);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextuw_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 6A/r VPUNPCKHDQ xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x6a);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextlb_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 60/r VPUNPCKLBW xmm1,xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x60);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextlh_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 61/r VPUNPCKLWD xmm1,xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x61);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pextlw_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 62/r VPUNPCKLDQ xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x62);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_e_b(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 74 /r VPCMPEQB xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x74);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_e_h(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 75 /r VPCMPEQW xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x75);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_e_w(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 76 /r VPCMPEQD xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x76);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 64 /r VPCMPGTB xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x64);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 65 /r VPCMPGTW xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x65);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction parallel_compare_gt_w(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 66 /r VPCMPGTD xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x66);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction vpunpcklqdq(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 6C/r VPUNPCKLQDQ xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x6c);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction pcpyld_swapped(Register dst, Register src0, Register src1) {
  return vpunpcklqdq(dst, src0, src1);
}

Instruction pcpyud(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 6D/r VPUNPCKHQDQ xmm1, xmm2, xmm3/m128
  // reg, vex, r/m
  InstructionX86 instr(0x6d);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction vpsubd(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG FA /r VPSUBD xmm1, xmm2, xmm3/m128
  // reg, vec, r/m
  InstructionX86 instr(0xfa);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

Instruction vpsrldq(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 73 /3 ib VPSRLDQ xmm1, xmm2, imm8
  InstructionX86 instr(0x73);
  instr.set_vex_modrm_and_rex(3, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction vpslldq(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.66.0F.WIG 73 /7 ib VPSLLDQ xmm1, xmm2, imm8
  InstructionX86 instr(0x73);
  instr.set_vex_modrm_and_rex(7, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction vpshuflw(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.F2.0F.WIG 70 /r ib VPSHUFLW xmm1, xmm2/m128, imm8
  InstructionX86 instr(0x70);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F2);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction vpshufhw(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src.is_128bit_simd());
  // VEX.128.F3.0F.WIG 70 /r ib VPSHUFHW xmm1, xmm2/m128, imm8
  InstructionX86 instr(0x70);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F3);
  instr.set(Imm(1, imm));
  return instr;
}

Instruction vpackuswb(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_128bit_simd());
  ASSERT(src0.is_128bit_simd());
  ASSERT(src1.is_128bit_simd());
  // VEX.128.66.0F.WIG 67 /r VPACKUSWB xmm1, xmm2, xmm3/m128
  // reg, vex, r/m

  InstructionX86 instr(0x67);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}
}  // namespace IGen
}  // namespace emitter

#endif