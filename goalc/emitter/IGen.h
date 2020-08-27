#ifndef JAK1_IGEN_H
#define JAK1_IGEN_H

#include <stdexcept>
#include "Instruction.h"
#include "registers.h"

namespace goal {
class IGen {
 public:
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   MOVES
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  /*!
   * mov gpr, gpr, 64 bits
   */
  static Instruction mov_gpr64_gpr64(uint8_t dst, uint8_t src) {
    assert(is_gpr(dst));
    assert(is_gpr(src));
    Instruction instr(0x89);
    instr.set_modrm_and_rex(src, dst, 3, true);
    return instr;
  }

  /*!
   * Move a 64-bit constant into a register.
   */
  static Instruction mov_gpr64_u64(uint8_t dst, uint64_t val) {
    assert(is_gpr(dst));
    bool rex_b = false;
    if (dst >= 8) {
      dst -= 8;
      rex_b = true;
    }
    Instruction instr(0xb8 + dst);
    instr.set(REX(true, false, false, rex_b));
    instr.set(Imm(8, val));
    return instr;
  }

  /*!
   * Move a 32-bit constant into a register.
   */
  static Instruction mov_gpr64_u32(uint8_t dst, uint64_t val) {
    assert(val <= UINT32_MAX);
    assert(is_gpr(dst));
    bool rex_b = false;
    if (dst >= 8) {
      dst -= 8;
      rex_b = true;
    }

    Instruction instr(0xb8 + dst);
    if (rex_b) {
      instr.set(REX(false, false, false, rex_b));
    }
    instr.set(Imm(4, val));
    return instr;
  }

  /*!
   * Move a signed 32-bit constant into a register.
   * When possible prefer mov_gpr64_u32. (use this only for negative values...)
   * This is always bigger than mov_gpr64_u32, but smaller than a mov_gpr_u64.
   */
  static Instruction mov_gpr64_s32(uint8_t dst, int64_t val) {
    assert(val >= INT32_MIN && val <= INT32_MAX);
    assert(is_gpr(dst));
    Instruction instr(0xc7);
    instr.set_modrm_and_rex(0, dst, 3, true);
    instr.set(Imm(4, val));
    return instr;
  }

  /*!
   * Move 32-bits of xmm to 32 bits of gpr (no sign extension).
   */
  static Instruction movd_gpr32_xmm32(uint8_t dst, uint8_t src) {
    assert(is_gpr(dst));
    assert(is_xmm(src));
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x7e);
    instr.set_modrm_and_rex(xmm_to_id(src), dst, 3, false);
    instr.swap_op0_rex();
    return instr;
  }

  /*!
   * Move 32-bits of gpr to 32-bits of xmm (no sign extenion)
   */
  static Instruction movd_xmm32_gpr32(uint8_t dst, uint8_t src) {
    assert(is_xmm(dst));
    assert(is_gpr(src));
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x6e);
    instr.set_modrm_and_rex(dst, xmm_to_id(src), 3, false);
    instr.swap_op0_rex();
    return instr;
  }

  /*!
   * Move 32-bits between xmm's
   */
  static Instruction mov_xmm32_xmm32(uint8_t dst, uint8_t src) {
    assert(is_xmm(dst));
    assert(is_xmm(src));
    Instruction instr(0xf3);
    instr.set_op2(0x0f);
    instr.set_op3(0x10);
    instr.set_modrm_and_rex(xmm_to_id(dst), xmm_to_id(src), 3, false);
    return instr;
  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   LOADS n' STORES
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * Store 8-bits from register into a memory location that is the sum of a 64-bit register
  //   * and signed 32-bit offset.
  //   */
  //  static Instruction store8_r64off32s_gpr8(uint8_t dst_reg, int32_t offset, uint8_t src_reg) {
  //    Instruction instr(0x88);
  //    instr.set_modrm_and_rex_for_addr(src_reg, dst_reg, 2, false);
  //    instr.set_disp(Imm(4, offset));
  //    if (src_reg > int(X86R::RBX)) {
  //      instr.add_rex();
  //    }
  //    return instr;
  //  }
  //
  //  /*!
  //   * Store 16-bits from register into a memory location that is the sum of a 64-bit register
  //   * and signed 32-bit offset.
  //   */
  //  static Instruction store16_r64off32s_gpr16(uint8_t dst_reg, int32_t offset, uint8_t src_reg) {
  //    Instruction instr(0x66);
  //    instr.set_op2(0x89);
  //    instr.set_modrm_and_rex_for_addr(src_reg, dst_reg, 2, false);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Store 32-bits from register into a memory location that is the sum of a 64-bit register
  //   * and signed 32-bit offset.
  //   */
  //  static Instruction store32_r64off32s_gpr32(uint8_t dst_reg, int32_t offset, uint8_t src_reg) {
  //    Instruction instr(0x89);
  //    instr.set_modrm_and_rex_for_addr(src_reg, dst_reg, 2, false);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Store 64-bits from gpr into memory located at 64-bit reg + 32-bit signed offset.
  //   */
  //  static Instruction store64_r64off32s_gpr64(uint8_t dst_reg, int32_t offset, uint8_t src_reg) {
  //    Instruction instr(0x89);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(src_reg, 2, dst_reg, true);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 8-bits from memory (at address of 64-bit reg + 32-bit signed offset) into gpr (zero
  //   * extended)
  //   */
  //  static Instruction load16_gpr8z_r64off32s(uint8_t dst, uint8_t src, int32_t offset) {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0xb6);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst, 2, src, true);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 16-bits from memory (at address of 64-bit reg + 32-bit signed offset) into gpr (zero
  //   * extended)
  //   */
  //  static Instruction load16_gpr16z_r64off32s(uint8_t dst, uint8_t src, int32_t offset) {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0xb7);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst, 2, src, true);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 16-bits from memory (at address of 64-bit reg + 32-bit signed offset) into gpr (sign
  //   * extended)
  //   */
  //  static Instruction load16_gpr16s_r64off32s(uint8_t dst, uint8_t src, int32_t offset) {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0xbf);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst, 2, src, true);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 32-bits from memory (at address of 64-bit reg + 32-bit signed offset) into gpr.
  //   * Use the sext flag to enable sign extension.
  //   */
  //  static Instruction load32_gpr32sz_r64off32s(uint8_t dst_reg,
  //                                              int32_t offset,
  //                                              uint8_t src_reg,
  //                                              bool sext = false) {
  //    Instruction instr(0x8b);
  //    if (sext) {
  //      instr.op = 0x63;
  //    }
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst_reg, 2, src_reg, sext);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 64-bits from memory located at 64-bit reg + 32-bit signed offset into gpr
  //   */
  //  static Instruction load64_gpr64_r64off32s(uint8_t dst_reg, int32_t offset, uint8_t src_reg) {
  //    Instruction instr(0x8b);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst_reg, 2, src_reg, true);
  //    instr.set_disp(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Load 32-bits form memory located at 64-bit reg + 32-bit signed offset into xmm (32-bits)
  //   * movss
  //   */
  //  static Instruction load32_xmm32_r64off32s(uint8_t dst, uint8_t src, int32_t offset) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x10);
  //    instr.set_modrm_rex_sib_for_reg_reg_disp32(dst, 2, src, false);
  //    instr.set_disp(Imm(4, offset));
  //    instr.swap_op0_rex();
  //    return instr;
  //  }

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   FUNCTION STUFF
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  /*!
   * Return instruction
   */
  static Instruction ret() { return Instruction(0xc3); }

  /*!
   * Instruction to push gpr (64-bits) onto the stack
   */
  static Instruction push_gpr64(uint8_t reg) {
    if (reg >= 8) {
      auto i = Instruction(0x50 + reg - 8);
      i.set(REX(false, false, false, true));
      return i;
    }
    return Instruction(0x50 + reg);
  }

  /*!
   * Instruction to pop 64 bit gpr from the stack
   */
  static Instruction pop_gpr64(uint8_t reg) {
    if (reg >= 8) {
      auto i = Instruction(0x58 + reg - 8);
      i.set(REX(false, false, false, true));
      return i;
    }
    return Instruction(0x58 + reg);
  }

  //  /*!
  //   * Call a function stored in a 64-bit gpr
  //   */
  //  static Instruction call_r64(uint8_t reg) {
  //    Instruction instr(0xff);
  //    if (reg >= 8) {
  //      instr.set(REX(false, false, false, true));
  //      reg -= 8;
  //    }
  //    assert(reg < 8);
  //    ModRM mrm;
  //    mrm.rm = reg;
  //    mrm.reg_op = 2;
  //    mrm.mod = 3;
  //    instr.set(mrm);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Call a function stored in a 64-bit gpr
  //   */
  //  static Instruction jmp_r64(uint8_t reg) {
  //    Instruction instr(0xff);
  //    if (reg >= 8) {
  //      instr.set(REX(false, false, false, true));
  //      reg -= 8;
  //    }
  //    assert(reg < 8);
  //    ModRM mrm;
  //    mrm.rm = reg;
  //    mrm.reg_op = 4;
  //    mrm.mod = 3;
  //    instr.set(mrm);
  //    return instr;
  //  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   INTEGER MATH
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * Add 64-bit registers.
  //   */
  //  static Instruction add_gpr64_gpr64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x01);
  //    instr.set_modrm_and_rex(src, dst, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Add a signed 32 bit immediate to a 64 bit register
  //   * TODO: determine if we can decrease to imm16?
  //   */
  //  static Instruction add_gpr64_imm32s(uint8_t dst, int32_t offset) {
  //    Instruction instr(0x81);
  //    instr.set_modrm_and_rex(0, dst, 3, true);
  //    instr.set(Imm(4, offset));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Add a signed 32 bit immediate to a 64 bit register
  //   * TODO: determine if we can decrease to imm16?
  //   */
  //  static Instruction add_gpr64_imm8s(uint8_t dst, int8_t v) {
  //    Instruction instr(0x83);
  //    instr.set_modrm_and_rex(0, dst, 3, true);
  //    instr.set(Imm(1, v));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Subtract 64-bit registers
  //   */
  //  static Instruction sub_gpr64_gpr64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x29);
  //    instr.set_modrm_and_rex(src, dst, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Multiply gprs (32-bit, signed).
  //   */
  //  static Instruction imul_gpr32_gpr32(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf);
  //    instr.set_op2(0xaf);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Divide (idiv, 32 bit)
  //   */
  //  static Instruction idiv_gpr32(uint8_t reg) {
  //    Instruction instr(0xf7);
  //    instr.set_modrm_and_rex(7, reg, 3, false);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Convert doubleword to quadword for division.
  //   * Blame Intel for this disaster.
  //   */
  //  static Instruction cdq() {
  //    Instruction instr(0x99);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Move from gpr32 to gpr64, with sign extension.
  //   * Needed for division madness.
  //   */
  //  static Instruction movsx_r64_r32(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x63);
  //    instr.set_modrm_and_rex(dst, src, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Compare gpr64.  This sets the flags for the jumps.
  //   */
  //  static Instruction cmp_gpr64_gpr64(uint8_t a, uint8_t b) {
  //    Instruction instr(0x3b);
  //    instr.set_modrm_and_rex(a, b, 3, true);
  //    return instr;
  //  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   BIT STUFF
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * Or of two gprs
  //   */
  //  static Instruction or_gpr64_gpr64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x0b);
  //    instr.set_modrm_and_rex(dst, src, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * And of two gprs
  //   */
  //  static Instruction and_gpr64_gpr64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x23);
  //    instr.set_modrm_and_rex(dst, src, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Xor of two gprs
  //   */
  //  static Instruction xor_gpr64_gpr64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0x33);
  //    instr.set_modrm_and_rex(dst, src, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * This is the way "real" compilers zero registers, so we should do it too.
  //   */
  //  static Instruction xor_zero_gpr(uint8_t reg) {
  //    Instruction instr(0x31);
  //    instr.set_modrm_and_rex(reg, reg, 3, false);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Bitwise not a gpr
  //   */
  //  static Instruction not_gpr64(uint8_t reg) {
  //    Instruction instr(0xf7);
  //    instr.set_modrm_and_rex(2, reg, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-bit gpr left by CL register
  //   */
  //  static Instruction shl_gpr64_cl(uint8_t reg) {
  //    Instruction instr(0xd3);
  //    instr.set_modrm_and_rex(4, reg, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-bit gpr right (logical) by CL register
  //   */
  //  static Instruction shr_gpr64_cl(uint8_t reg) {
  //    Instruction instr(0xd3);
  //    instr.set_modrm_and_rex(5, reg, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-bit gpr right (arithmetic) by CL register
  //   */
  //  static Instruction sar_gpr64_cl(uint8_t reg) {
  //    Instruction instr(0xd3);
  //    instr.set_modrm_and_rex(7, reg, 3, true);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-ptr left (logical) by the constant shift amount "sa".
  //   */
  //  static Instruction shl_gpr64_u8(uint8_t reg, uint8_t sa) {
  //    Instruction instr(0xc1);
  //    instr.set_modrm_and_rex(4, reg, 3, true);
  //    instr.set(Imm(1, sa));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-ptr right (logical) by the constant shift amount "sa".
  //   */
  //  static Instruction shr_gpr64_u8(uint8_t reg, uint8_t sa) {
  //    Instruction instr(0xc1);
  //    instr.set_modrm_and_rex(5, reg, 3, true);
  //    instr.set(Imm(1, sa));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Shift 64-ptr right (arithmetic) by the constant shift amount "sa".
  //   */
  //  static Instruction sar_gpr64_u8(uint8_t reg, uint8_t sa) {
  //    Instruction instr(0xc1);
  //    instr.set_modrm_and_rex(7, reg, 3, true);
  //    instr.set(Imm(1, sa));
  //    return instr;
  //  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   CONTROL FLOW
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * Jump, 32-bit constant offset.  The offset is by default 0 and must be patched later.
  //   */
  //  static Instruction jmp_32() {
  //    Instruction instr(0xe9);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump if equal.
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction je_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x84);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump not equal.
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jne_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x85);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump less than or equal.
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jle_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x8e);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump greater than or equal.
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jge_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x8d);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump less than
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jl_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x8c);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump greater than
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jg_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x8f);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump below or equal
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jbe_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x86);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump above or equal
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jae_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x83);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump below
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction jb_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x82);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  /*!
  //   * Jump above
  //   * TODO - can we get away with 16 bits?
  //   */
  //  static Instruction ja_32() {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x87);
  //    instr.set(Imm(4, 0));
  //    return instr;
  //  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   FLOAT MATH
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * Compare two floats and set flag register for jump
  //   */
  //  static Instruction cmp_flt_flt(uint8_t a, uint8_t b) {
  //    Instruction instr(0x0f);
  //    instr.set_op2(0x2e);
  //    instr.set_modrm_and_rex(a, b, 3, false);
  //    return instr;
  //  }
  //
  //  /*!
  //   * Multiply two floats in xmm's
  //   */
  //  static Instruction mulss_xmm_xmm(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x59);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  /*!
  //   * Divide two floats in xmm's
  //   */
  //  static Instruction divss_xmm_xmm(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x5e);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  /*!
  //   * Subtract two floats in xmm's
  //   */
  //  static Instruction subss_xmm_xmm(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x5c);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  /*!
  //   * Add two floats in xmm's
  //   */
  //  static Instruction addss_xmm_xmm(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x58);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  /*!
  //   * Convert GPR int32 to XMM float (single precision)
  //   */
  //  static Instruction int32_to_float(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x2a);
  //    instr.set_modrm_and_rex(dst, src, 3, false);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  /*!
  //   * Convert XMM float to GPR int32(single precision) (truncate)
  //   */
  //  static Instruction float_to_int64(uint8_t dst, uint8_t src) {
  //    Instruction instr(0xf3);
  //    instr.set_op2(0x0f);
  //    instr.set_op3(0x2c);
  //    instr.set_modrm_and_rex(dst, src, 3, true);
  //    instr.swap_op0_rex();
  //    return instr;
  //  }
  //
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //  //   UTILITIES
  //  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //
  //  /*!
  //   * A "null" instruction.  This instruction does not generate any bytes
  //   * but can be referred to by a label.  Useful to insert in place of a real instruction
  //   * if the real instruction has been optimized out.
  //   */
  //  static Instruction null() {
  //    Instruction i(0);
  //    i.is_null = true;
  //    return i;
  //  }
  //
  //  /*!
  //   * A "function start" instruction.  This emits no opcodes, but is used
  //   * to determine where to insert the function type tag and how to align a function.
  //   */
  //  static Instruction function_start() {
  //    Instruction i(0);
  //    i.is_null = true;
  //    i.is_function_start = true;
  //    return i;
  //  }
};
}  // namespace goal

#endif  // JAK1_IGEN_H
