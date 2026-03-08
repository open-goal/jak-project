#include "IGenX86.h"

namespace emitter {
namespace IGen {
namespace X86 {

InstructionX86 mov_gpr64_gpr64(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

InstructionX86 mov_gpr64_u64(Register dst, uint64_t val) {
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

InstructionX86 mov_gpr64_u32(Register dst, uint64_t val) {
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

InstructionX86 mov_gpr64_s32(Register dst, int64_t val) {
  ASSERT(val >= INT32_MIN && val <= INT32_MAX);
  ASSERT(dst.is_gpr());
  InstructionX86 instr(0xc7);
  instr.set_modrm_and_rex(0, dst.hw_id(), 3, true);
  instr.set(Imm(4, val));
  return instr;
}

InstructionX86 movd_gpr32_xmm32(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x7e);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 movd_xmm32_gpr32(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x6e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 movq_gpr64_xmm64(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x7e);
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 movq_xmm64_gpr64(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0x66);
  instr.set_op2(0x0f);
  instr.set_op3(0x6e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 mov_xmm32_xmm32(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
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

InstructionX86 load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
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

InstructionX86 load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
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

InstructionX86 load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 store16_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
  ASSERT(value.is_gpr());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());
  ASSERT(addr1 != addr2);
  ASSERT(addr1 != RSP);
  ASSERT(addr2 != RSP);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
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
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
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
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 store32_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
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

InstructionX86 load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 store32_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
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

InstructionX86 load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 store32_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
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

InstructionX86 load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
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

InstructionX86 store64_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
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

InstructionX86 load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
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

InstructionX86 store64_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
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

InstructionX86 load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
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

InstructionX86 store64_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
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

InstructionX86 store_goal_gpr(Register addr, Register value, Register off, int offset, int size) {
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
      return {0};
  }
}

InstructionX86 load_goal_gpr(Register dst,
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
      return {0};
  }
}

InstructionX86 store32_xmm32_gpr64_plus_gpr64(Register addr1, Register addr2, Register xmm_value) {
  ASSERT(xmm_value.is_xmm());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(xmm_value.hw_id(), addr1.hw_id(), addr2.hw_id());

  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load32_xmm32_gpr64_plus_gpr64(Register xmm_dest, Register addr1, Register addr2) {
  ASSERT(xmm_dest.is_xmm());
  ASSERT(addr1.is_gpr());
  ASSERT(addr2.is_gpr());

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_reg_plus_reg_addr(xmm_dest.hw_id(), addr1.hw_id(), addr2.hw_id());

  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store32_xmm32_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                      Register addr2,
                                                      Register xmm_value,
                                                      s64 offset) {
  ASSERT(xmm_value.is_xmm());
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

InstructionX86 load32_xmm32_gpr64_plus_gpr64_plus_s8(Register xmm_dest,
                                                     Register addr1,
                                                     Register addr2,
                                                     s64 offset) {
  ASSERT(xmm_dest.is_xmm());
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

InstructionX86 store32_xmm32_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                       Register addr2,
                                                       Register xmm_value,
                                                       s64 offset) {
  ASSERT(xmm_value.is_xmm());
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

InstructionX86 lea_reg_plus_off32(Register dest, Register base, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dest.hw_id(), 2, base.hw_id(), true);
  instr.set(Imm(4, offset));
  return instr;
}

InstructionX86 lea_reg_plus_off8(Register dest, Register base, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(base.is_gpr());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dest.hw_id(), 1, base.hw_id(), true);
  instr.set(Imm(1, offset));
  return instr;
}

InstructionX86 lea_reg_plus_off(Register dest, Register base, s64 offset) {
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return lea_reg_plus_off8(dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return lea_reg_plus_off32(dest, base, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 store32_xmm32_gpr64_plus_s32(Register base, Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_xmm());
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

InstructionX86 store32_xmm32_gpr64_plus_s8(Register base, Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_xmm());
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

InstructionX86 load32_xmm32_gpr64_plus_gpr64_plus_s32(Register xmm_dest,
                                                      Register addr1,
                                                      Register addr2,
                                                      s64 offset) {
  ASSERT(xmm_dest.is_xmm());
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

InstructionX86 load32_xmm32_gpr64_plus_s32(Register xmm_dest, Register base, s64 offset) {
  ASSERT(xmm_dest.is_xmm());
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

InstructionX86 load32_xmm32_gpr64_plus_s8(Register xmm_dest, Register base, s64 offset) {
  ASSERT(xmm_dest.is_xmm());
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

InstructionX86 load_goal_xmm32(Register xmm_dest, Register addr, Register off, s64 offset) {
  if (offset == 0) {
    return load32_xmm32_gpr64_plus_gpr64(xmm_dest, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load32_xmm32_gpr64_plus_gpr64_plus_s8(xmm_dest, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load32_xmm32_gpr64_plus_gpr64_plus_s32(xmm_dest, addr, off, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 store_goal_xmm32(Register addr, Register xmm_value, Register off, s64 offset) {
  if (offset == 0) {
    return store32_xmm32_gpr64_plus_gpr64(addr, off, xmm_value);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store32_xmm32_gpr64_plus_gpr64_plus_s8(addr, off, xmm_value, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store32_xmm32_gpr64_plus_gpr64_plus_s32(addr, off, xmm_value, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 store_reg_offset_xmm32(Register base, Register xmm_value, s64 offset) {
  ASSERT(base.is_gpr());
  ASSERT(xmm_value.is_xmm());
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store32_xmm32_gpr64_plus_s8(base, xmm_value, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store32_xmm32_gpr64_plus_s32(base, xmm_value, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 load_reg_offset_xmm32(Register xmm_dest, Register base, s64 offset) {
  ASSERT(base.is_gpr());
  ASSERT(xmm_dest.is_xmm());
  if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load32_xmm32_gpr64_plus_s8(xmm_dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load32_xmm32_gpr64_plus_s32(xmm_dest, base, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 store128_gpr64_xmm128(Register gpr_addr, Register xmm_value) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_xmm());
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_and_rex_for_reg_addr(xmm_value.hw_id(), gpr_addr.hw_id(), false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store128_gpr64_xmm128_s32(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_xmm());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 2, gpr_addr.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store128_gpr64_xmm128_s8(Register gpr_addr, Register xmm_value, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_value.is_xmm());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x7f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_value.hw_id(), 1, gpr_addr.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_xmm());
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_and_rex_for_reg_addr(xmm_dest.hw_id(), gpr_addr.hw_id(), false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load128_xmm128_gpr64_s32(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_xmm());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 2, gpr_addr.hw_id(), false);
  instr.set(Imm(4, offset));
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load128_xmm128_gpr64_s8(Register xmm_dest, Register gpr_addr, s64 offset) {
  ASSERT(gpr_addr.is_gpr());
  ASSERT(xmm_dest.is_xmm());
  ASSERT(offset >= INT8_MIN && offset <= INT8_MAX);
  InstructionX86 instr(0x66);

  instr.set_op2(0x0f);
  instr.set_op3(0x6f);
  instr.set_modrm_rex_sib_for_reg_reg_disp(xmm_dest.hw_id(), 1, gpr_addr.hw_id(), false);
  instr.set(Imm(1, offset));
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load128_xmm128_reg_offset(Register xmm_dest, Register base, s64 offset) {
  if (offset == 0) {
    return load128_xmm128_gpr64(xmm_dest, base);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return load128_xmm128_gpr64_s8(xmm_dest, base, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return load128_xmm128_gpr64_s32(xmm_dest, base, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 store128_xmm128_reg_offset(Register base, Register xmm_val, s64 offset) {
  if (offset == 0) {
    return store128_gpr64_xmm128(base, xmm_val);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return store128_gpr64_xmm128_s8(base, xmm_val, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return store128_gpr64_xmm128_s32(base, xmm_val, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 load64_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 load32s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x63);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 load32u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8b);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, false);
  return instr;
}

InstructionX86 load16u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb7);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 load16s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbf);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 load8u_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xb6);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 load8s_rip_s32(Register dest, s64 offset) {
  ASSERT(dest.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0xf);
  instr.set_op2(0xbe);
  instr.set_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset, true);
  return instr;
}

InstructionX86 static_load(Register dest, s64 offset, int size, bool sign_extend) {
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

InstructionX86 store64_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, true);
  return instr;
}

InstructionX86 store32_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, false);
  return instr;
}

InstructionX86 store16_rip_s32(Register src, s64 offset) {
  ASSERT(src.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x66);
  instr.set_op2(0x89);
  instr.set_modrm_and_rex_for_rip_plus_s32(src.hw_id(), offset, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 store8_rip_s32(Register src, s64 offset) {
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

InstructionX86 static_store(Register value, s64 offset, int size) {
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

InstructionX86 static_addr(Register dst, s64 offset) {
  ASSERT(dst.is_gpr());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);
  InstructionX86 instr(0x8d);
  instr.set_modrm_and_rex_for_rip_plus_s32(dst.hw_id(), offset, true);
  return instr;
}

InstructionX86 static_load_xmm32(Register xmm_dest, s64 offset) {
  ASSERT(xmm_dest.is_xmm());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x10);
  instr.set_modrm_and_rex_for_rip_plus_s32(xmm_dest.hw_id(), offset, false);

  instr.swap_op0_rex();
  return instr;
}

InstructionX86 static_store_xmm32(Register xmm_value, s64 offset) {
  ASSERT(xmm_value.is_xmm());
  ASSERT(offset >= INT32_MIN && offset <= INT32_MAX);

  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x11);
  instr.set_modrm_and_rex_for_rip_plus_s32(xmm_value.hw_id(), offset, false);

  instr.swap_op0_rex();
  return instr;
}

InstructionX86 load64_gpr64_plus_s32(Register dst_reg, int32_t offset, Register src_reg) {
  ASSERT(dst_reg.is_gpr());
  ASSERT(src_reg.is_gpr());
  InstructionX86 instr(0x8b);
  instr.set_modrm_rex_sib_for_reg_reg_disp(dst_reg.hw_id(), 2, src_reg.hw_id(), true);
  instr.set_disp(Imm(4, offset));
  return instr;
}

InstructionX86 store64_gpr64_plus_s32(Register addr, int32_t offset, Register value) {
  ASSERT(addr.is_gpr());
  ASSERT(value.is_gpr());
  InstructionX86 instr(0x89);
  instr.set_modrm_rex_sib_for_reg_reg_disp(value.hw_id(), 2, addr.hw_id(), true);
  instr.set_disp(Imm(4, offset));
  return instr;
}

InstructionX86 ret() {
  return InstructionX86(0xc3);
}

InstructionX86 push_gpr64(Register reg) {
  ASSERT(reg.is_gpr());
  if (reg.hw_id() >= 8) {
    auto i = InstructionX86(0x50 + reg.hw_id() - 8);
    i.set(REX(false, false, false, true));
    return i;
  }
  return InstructionX86(0x50 + reg.hw_id());
}

InstructionX86 pop_gpr64(Register reg) {
  ASSERT(reg.is_gpr());
  if (reg.hw_id() >= 8) {
    auto i = InstructionX86(0x58 + reg.hw_id() - 8);
    i.set(REX(false, false, false, true));
    return i;
  }
  return InstructionX86(0x58 + reg.hw_id());
}

InstructionX86 call_r64(Register reg_) {
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

InstructionX86 jmp_r64(Register reg_) {
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

InstructionX86 sub_gpr64_imm8s(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr());
  ASSERT(imm >= INT8_MIN && imm <= INT8_MAX);

  InstructionX86 instr(0x83);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 sub_gpr64_imm32s(Register reg, int64_t imm) {
  ASSERT(reg.is_gpr());
  ASSERT(imm >= INT32_MIN && imm <= INT32_MAX);
  InstructionX86 instr(0x81);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(4, imm));
  return instr;
}

InstructionX86 add_gpr64_imm8s(Register reg, int64_t v) {
  ASSERT(v >= INT8_MIN && v <= INT8_MAX);
  InstructionX86 instr(0x83);
  instr.set_modrm_and_rex(0, reg.hw_id(), 3, true);
  instr.set(Imm(1, v));
  return instr;
}

InstructionX86 add_gpr64_imm32s(Register reg, int64_t v) {
  ASSERT(v >= INT32_MIN && v <= INT32_MAX);
  InstructionX86 instr(0x81);
  instr.set_modrm_and_rex(0, reg.hw_id(), 3, true);
  instr.set(Imm(4, v));
  return instr;
}

InstructionX86 add_gpr64_imm(Register reg, int64_t imm) {
  if (imm >= INT8_MIN && imm <= INT8_MAX) {
    return add_gpr64_imm8s(reg, imm);
  } else if (imm >= INT32_MIN && imm <= INT32_MAX) {
    return add_gpr64_imm32s(reg, imm);
  } else {
    throw std::runtime_error("Invalid `add` with reg[" + reg.print() + "]/imm[" +
                             std::to_string(imm) + "]");
  }
}

InstructionX86 sub_gpr64_imm(Register reg, int64_t imm) {
  if (imm >= INT8_MIN && imm <= INT8_MAX) {
    return sub_gpr64_imm8s(reg, imm);
  } else if (imm >= INT32_MIN && imm <= INT32_MAX) {
    return sub_gpr64_imm32s(reg, imm);
  } else {
    throw std::runtime_error("Invalid `sub` with reg[" + reg.print() + "]/imm[" +
                             std::to_string(imm) + "]");
  }
}

InstructionX86 add_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x01);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

InstructionX86 sub_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x29);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
  return instr;
}

InstructionX86 imul_gpr32_gpr32(Register dst, Register src) {
  InstructionX86 instr(0xf);
  instr.set_op2(0xaf);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  return instr;
}

InstructionX86 imul_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0xf);
  instr.set_op2(0xaf);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

InstructionX86 idiv_gpr32(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, false);
  return instr;
}

InstructionX86 unsigned_div_gpr32(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(6, reg.hw_id(), 3, false);
  return instr;
}

InstructionX86 cdq() {
  InstructionX86 instr(0x99);
  return instr;
}

InstructionX86 movsx_r64_r32(Register dst, Register src) {
  InstructionX86 instr(0x63);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

InstructionX86 cmp_gpr64_gpr64(Register a, Register b) {
  InstructionX86 instr(0x3b);
  ASSERT(a.is_gpr());
  ASSERT(b.is_gpr());
  instr.set_modrm_and_rex(a.hw_id(), b.hw_id(), 3, true);
  return instr;
}

InstructionX86 or_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x0b);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

InstructionX86 and_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x23);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

InstructionX86 xor_gpr64_gpr64(Register dst, Register src) {
  InstructionX86 instr(0x33);
  ASSERT(dst.is_gpr());
  ASSERT(src.is_gpr());
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, true);
  return instr;
}

InstructionX86 not_gpr64(Register reg) {
  InstructionX86 instr(0xf7);
  ASSERT(reg.is_gpr());
  instr.set_modrm_and_rex(2, reg.hw_id(), 3, true);
  return instr;
}

InstructionX86 shl_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(4, reg.hw_id(), 3, true);
  return instr;
}

InstructionX86 shr_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  return instr;
}

InstructionX86 sar_gpr64_cl(Register reg) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xd3);
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, true);
  return instr;
}

InstructionX86 shl_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(4, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

InstructionX86 shr_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

InstructionX86 sar_gpr64_u8(Register reg, uint8_t sa) {
  ASSERT(reg.is_gpr());
  InstructionX86 instr(0xc1);
  instr.set_modrm_and_rex(7, reg.hw_id(), 3, true);
  instr.set(Imm(1, sa));
  return instr;
}

InstructionX86 jmp_32() {
  InstructionX86 instr(0xe9);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 je_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x84);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jne_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x85);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jle_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8e);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jge_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8d);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jl_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8c);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jg_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x8f);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jbe_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x86);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jae_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x83);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 jb_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x82);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 ja_32() {
  InstructionX86 instr(0x0f);
  instr.set_op2(0x87);
  instr.set(Imm(4, 0));
  return instr;
}

InstructionX86 cmp_flt_flt(Register a, Register b) {
  ASSERT(a.is_xmm());
  ASSERT(b.is_xmm());
  InstructionX86 instr(0x0f);
  instr.set_op2(0x2e);
  instr.set_modrm_and_rex(a.hw_id(), b.hw_id(), 3, false);
  return instr;
}

InstructionX86 sqrts_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x51);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 mulss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x59);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 divss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5e);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 subss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5c);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 addss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x58);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 minss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5d);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 maxss_xmm_xmm(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x5f);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 int32_to_float(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_gpr());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x2a);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 float_to_int32(Register dst, Register src) {
  ASSERT(dst.is_gpr());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xf3);
  instr.set_op2(0x0f);
  instr.set_op3(0x2c);
  instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
  instr.swap_op0_rex();
  return instr;
}

InstructionX86 nop() {
  InstructionX86 instr(0x90);
  return instr;
}

InstructionX86 null() {
  InstructionX86 i(0);
  i.m_flags |= InstructionX86::kIsNull;
  return i;
}

InstructionX86 nop_vf() {
  InstructionX86 instr(0xd9);
  instr.set_op2(0xd0);
  return instr;
}

InstructionX86 wait_vf() {
  InstructionX86 instr(0x9B);
  return instr;
}

InstructionX86 mov_vf_vf(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  if (src.hw_id() >= 8 && dst.hw_id() < 8) {
    InstructionX86 instr(0x29);
    instr.set_vex_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, VEX3::LeadingBytes::P_0F, false);
    return instr;
  } else {
    InstructionX86 instr(0x28);
    instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, VEX3::LeadingBytes::P_0F, false);
    return instr;
  }
}

InstructionX86 loadvf_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
  ASSERT(dst.is_xmm());
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

InstructionX86 loadvf_gpr64_plus_gpr64_plus_s8(Register dst,
                                               Register addr1,
                                               Register addr2,
                                               s64 offset) {
  ASSERT(dst.is_xmm());
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

InstructionX86 loadvf_gpr64_plus_gpr64_plus_s32(Register dst,
                                                Register addr1,
                                                Register addr2,
                                                s64 offset) {
  ASSERT(dst.is_xmm());
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

InstructionX86 load_goal_xmm128(Register dst, Register addr, Register off, int offset) {
  if (offset == 0) {
    return loadvf_gpr64_plus_gpr64(dst, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return loadvf_gpr64_plus_gpr64_plus_s8(dst, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return loadvf_gpr64_plus_gpr64_plus_s32(dst, addr, off, offset);
  } else {
    ASSERT(false);
    return {0};
  }
}

InstructionX86 storevf_gpr64_plus_gpr64(Register value, Register addr1, Register addr2) {
  ASSERT(value.is_xmm());
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

InstructionX86 storevf_gpr64_plus_gpr64_plus_s8(Register value,
                                                Register addr1,
                                                Register addr2,
                                                s64 offset) {
  ASSERT(value.is_xmm());
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

InstructionX86 storevf_gpr64_plus_gpr64_plus_s32(Register value,
                                                 Register addr1,
                                                 Register addr2,
                                                 s64 offset) {
  ASSERT(value.is_xmm());
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

InstructionX86 store_goal_vf(Register addr, Register value, Register off, s64 offset) {
  if (offset == 0) {
    return storevf_gpr64_plus_gpr64(value, addr, off);
  } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
    return storevf_gpr64_plus_gpr64_plus_s8(value, addr, off, offset);
  } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
    return storevf_gpr64_plus_gpr64_plus_s32(value, addr, off, offset);
  }
  ASSERT(false);
  return {0};
}

InstructionX86 loadvf_rip_plus_s32(Register dest, s64 offset) {
  ASSERT(dest.is_xmm());
  ASSERT(offset >= INT32_MIN);
  ASSERT(offset <= INT32_MAX);
  InstructionX86 instr(0x28);
  instr.set_vex_modrm_and_rex_for_rip_plus_s32(dest.hw_id(), offset);
  return instr;
}

InstructionX86 blend_vf(Register dst, Register src1, Register src2, u8 mask) {
  ASSERT(!(mask & 0b11110000));
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x0c);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F_3A, src1.hw_id(),
                              false, VexPrefix::P_66);
  instr.set(Imm(1, mask));
  return instr;
}

InstructionX86 swizzle_vf(Register dst, Register src, u8 controlBytes) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0xC6);

  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, src.hw_id());
  instr.set(Imm(1, controlBytes));
  return instr;
}

InstructionX86 shuffle_vf(Register dst, Register src, u8 dx, u8 dy, u8 dz, u8 dw) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  ASSERT(dx < 4);
  ASSERT(dy < 4);
  ASSERT(dz < 4);
  ASSERT(dw < 4);
  u8 imm = dx + (dy << 2) + (dz << 4) + (dw << 6);
  return swizzle_vf(dst, src, imm);
}

InstructionX86 splat_vf(Register dst, Register src, Register::VF_ELEMENT element) {
  switch (element) {
    case Register::VF_ELEMENT::X:
      return swizzle_vf(dst, src, 0b00000000);
      break;
    case Register::VF_ELEMENT::Y:
      return swizzle_vf(dst, src, 0b01010101);
      break;
    case Register::VF_ELEMENT::Z:
      return swizzle_vf(dst, src, 0b10101010);
      break;
    case Register::VF_ELEMENT::W:
      return swizzle_vf(dst, src, 0b11111111);
      break;
    default:
      ASSERT(false);
      return {0};
  }
}

InstructionX86 xor_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x57);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 sub_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x5c);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 add_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x58);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 mul_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x59);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 max_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x5F);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 min_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x5D);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 div_vf(Register dst, Register src1, Register src2) {
  ASSERT(dst.is_xmm());
  ASSERT(src1.is_xmm());
  ASSERT(src2.is_xmm());
  InstructionX86 instr(0x5E);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src2.hw_id(), VEX3::LeadingBytes::P_0F, src1.hw_id());
  return instr;
}

InstructionX86 sqrt_vf(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0x51);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0b0);
  return instr;
}

InstructionX86 itof_vf(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());
  InstructionX86 instr(0x5b);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0);
  return instr;
}

InstructionX86 ftoi_vf(Register dst, Register src) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x5b);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F3);
  return instr;
}

InstructionX86 pw_sra(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(4, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 pw_srl(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(2, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 ph_srl(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x71);
  instr.set_vex_modrm_and_rex(2, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 pw_sll(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x72);
  instr.set_vex_modrm_and_rex(6, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}
InstructionX86 ph_sll(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x71);
  instr.set_vex_modrm_and_rex(6, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 parallel_add_byte(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0xFC);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_bitwise_or(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0xEB);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_bitwise_xor(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0xEF);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_bitwise_and(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0xDB);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextub_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x68);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextuh_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x69);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextuw_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x6a);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextlb_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x60);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextlh_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x61);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pextlw_swapped(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x62);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_e_b(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x74);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_e_h(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x75);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_e_w(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x76);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_gt_b(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x64);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_gt_h(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x65);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 parallel_compare_gt_w(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x66);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 vpunpcklqdq(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x6c);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 pcpyld_swapped(Register dst, Register src0, Register src1) {
  return vpunpcklqdq(dst, src0, src1);
}

InstructionX86 pcpyud(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x6d);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 vpsubd(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0xfa);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}

InstructionX86 vpsrldq(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x73);
  instr.set_vex_modrm_and_rex(3, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 vpslldq(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x73);
  instr.set_vex_modrm_and_rex(7, src.hw_id(), VEX3::LeadingBytes::P_0F, dst.hw_id(), false,
                              VexPrefix::P_66);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 vpshuflw(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x70);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F2);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 vpshufhw(Register dst, Register src, u8 imm) {
  ASSERT(dst.is_xmm());
  ASSERT(src.is_xmm());

  InstructionX86 instr(0x70);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src.hw_id(), VEX3::LeadingBytes::P_0F, 0, false,
                              VexPrefix::P_F3);
  instr.set(Imm(1, imm));
  return instr;
}

InstructionX86 vpackuswb(Register dst, Register src0, Register src1) {
  ASSERT(dst.is_xmm());
  ASSERT(src0.is_xmm());
  ASSERT(src1.is_xmm());

  InstructionX86 instr(0x67);
  instr.set_vex_modrm_and_rex(dst.hw_id(), src1.hw_id(), VEX3::LeadingBytes::P_0F, src0.hw_id(),
                              false, VexPrefix::P_66);
  return instr;
}
}  // namespace X86
}  // namespace IGen
}  // namespace emitter