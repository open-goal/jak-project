#ifndef JAK_IGEN_H
#define JAK_IGEN_H

#include <cassert>
#include "Register.h"
#include "Instruction.h"

namespace emitter {
class IGen {
 public:
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   MOVES
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  /*!
   * Move data from src to dst. Moves all 64-bits of the GPR.
   */
  static Instruction mov_gpr64_gpr64(Register dst, Register src) {
    assert(dst.is_gpr());
    assert(src.is_gpr());
    Instruction instr(0x89);
    instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, true);
    return instr;
  }

  /*!
   * Move a 64-bit constant into a register.
   */
  static Instruction mov_gpr64_u64(Register dst, uint64_t val) {
    assert(dst.is_gpr());
    bool rex_b = false;
    auto dst_hw_id = dst.hw_id();
    if (dst_hw_id >= 8) {
      dst_hw_id -= 8;
      rex_b = true;
    }
    Instruction instr(0xb8 + dst_hw_id);
    instr.set(REX(true, false, false, rex_b));
    instr.set(Imm(8, val));
    return instr;
  }

  /*!
   * Move a 32-bit constant into a register. Zeros the upper 32 bits.
   */
  static Instruction mov_gpr64_u32(Register dst, uint64_t val) {
    assert(val <= UINT32_MAX);
    assert(dst.is_gpr());
    auto dst_hw_id = dst.hw_id();
    bool rex_b = false;
    if (dst_hw_id >= 8) {
      dst_hw_id -= 8;
      rex_b = true;
    }

    Instruction instr(0xb8 + dst_hw_id);
    if (rex_b) {
      instr.set(REX(false, false, false, rex_b));
    }
    instr.set(Imm(4, val));
    return instr;
  }

  /*!
   * Move a signed 32-bit constant into a register. Sign extends for the upper 32 bits.
   * When possible prefer mov_gpr64_u32. (use this only for negative values...)
   * This is always bigger than mov_gpr64_u32, but smaller than a mov_gpr_u64.
   */
  static Instruction mov_gpr64_s32(Register dst, int64_t val) {
    assert(val >= INT32_MIN && val <= INT32_MAX);
    assert(dst.is_gpr());
    Instruction instr(0xc7);
    instr.set_modrm_and_rex(0, dst.hw_id(), 3, true);
    instr.set(Imm(4, val));
    return instr;
  }

  /*!
   * Move 32-bits of xmm to 32 bits of gpr (no sign extension).
   */
  static Instruction movd_gpr32_xmm32(Register dst, Register src) {
    assert(dst.is_gpr());
    assert(src.is_xmm());
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x7e);
    instr.set_modrm_and_rex(src.hw_id(), dst.hw_id(), 3, false);
    instr.swap_op0_rex();
    return instr;
  }

  /*!
   * Move 32-bits of gpr to 32-bits of xmm (no sign extension)
   */
  static Instruction movd_xmm32_gpr32(Register dst, Register src) {
    assert(dst.is_xmm());
    assert(src.is_gpr());
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x6e);
    instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
    instr.swap_op0_rex();
    return instr;
  }

  /*!
   * Move 32-bits between xmm's
   */
  static Instruction mov_xmm32_xmm32(Register dst, Register src) {
    assert(dst.is_xmm());
    assert(src.is_xmm());
    Instruction instr(0xf3);
    instr.set_op2(0x0f);
    instr.set_op3(0x10);
    instr.set_modrm_and_rex(dst.hw_id(), src.hw_id(), 3, false);
    return instr;
  }

  // todo - GPR64 -> XMM64 (zext)
  // todo - XMM -> GPR64
  // todo - XMM128 - XMM128

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   GOAL Loads
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  /*!
   * movsx dst, BYTE PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load8s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0xf);
    instr.set_op2(0xbe);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                  false);
    return instr;
  }

  static Instruction store8_gpr64_gpr64_plus_gpr64(Register addr1, Register addr2, Register value) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0x88);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
    if (value.id() > RBX) {
      instr.add_rex();
    }
    return instr;
  }

  static Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                           Register addr1,
                                                           Register addr2,
                                                           s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xbe);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction store8_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                           Register addr2,
                                                           Register value,
                                                           s64 offset) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0x88);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, false);
    if (value.id() > RBX) {
      instr.add_rex();
    }
    return instr;
  }

  static Instruction load8s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xbe);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  static Instruction store8_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                            Register addr2,
                                                            Register value,
                                                            s64 offset) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0x88);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, false);
    if (value.id() > RBX) {
      instr.add_rex();
    }
    return instr;
  }

  /*!
   * movzx dst, BYTE PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load8u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0xf);
    instr.set_op2(0xb6);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                  false);
    return instr;
  }

  static Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                           Register addr1,
                                                           Register addr2,
                                                           s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xb6);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction load8u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xb6);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  /*!
   * movsx dst, WORD PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load16s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0xf);
    instr.set_op2(0xbf);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                  false);
    return instr;
  }

  static Instruction store16_gpr64_gpr64_plus_gpr64(Register addr1,
                                                    Register addr2,
                                                    Register value) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0x66);
    instr.set_op2(0x89);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(value.hw_id(), addr1.hw_id(), addr2.hw_id());
    instr.swap_op0_rex();  // why?????
    return instr;
  }

  static Instruction store16_gpr64_gpr64_plus_gpr64_plus_s8(Register addr1,
                                                            Register addr2,
                                                            Register value,
                                                            s64 offset) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0x66);
    instr.set_op2(0x89);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, false);
    instr.swap_op0_rex();  // why?????
    return instr;
  }

  static Instruction store16_gpr64_gpr64_plus_gpr64_plus_s32(Register addr1,
                                                             Register addr2,
                                                             Register value,
                                                             s64 offset) {
    assert(value.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0x66);
    instr.set_op2(0x89);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(value.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, false);
    instr.swap_op0_rex();  // why?????
    return instr;
  }

  static Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xbf);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction load16s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                             Register addr1,
                                                             Register addr2,
                                                             s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xbf);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  /*!
   * movzx dst, WORD PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load16u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0xf);
    instr.set_op2(0xb7);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true,
                                                  false);
    return instr;
  }

  static Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xb7);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction load16u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                             Register addr1,
                                                             Register addr2,
                                                             s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0xf);
    instr.set_op2(0xb7);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  /*!
   * movsxd dst, DWORD PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load32s_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0x63);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true);
    return instr;
  }

  static Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0x63);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction load32s_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                             Register addr1,
                                                             Register addr2,
                                                             s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0x63);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  /*!
   * movzxd dst, DWORD PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load32u_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id());
    return instr;
  }

  static Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, false);
    return instr;
  }

  static Instruction load32u_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                             Register addr1,
                                                             Register addr2,
                                                             s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, false);
    return instr;
  }

  /*!
   * mov dst, QWORD PTR [addr1 + addr2]
   * addr1 and addr2 have to be different registers.
   * Cannot use rsp.
   */
  static Instruction load64_gpr64_gpr64_plus_gpr64(Register dst, Register addr1, Register addr2) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_addr(dst.hw_id(), addr1.hw_id(), addr2.hw_id(), true);
    return instr;
  }

  static Instruction load64_gpr64_gpr64_plus_gpr64_plus_s8(Register dst,
                                                           Register addr1,
                                                           Register addr2,
                                                           s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT8_MIN && offset <= INT8_MAX);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s8(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                     offset, true);
    return instr;
  }

  static Instruction load64_gpr64_gpr64_plus_gpr64_plus_s32(Register dst,
                                                            Register addr1,
                                                            Register addr2,
                                                            s64 offset) {
    assert(dst.is_gpr());
    assert(addr1.is_gpr());
    assert(addr2.is_gpr());
    assert(addr1 != addr2);
    assert(addr1 != RSP);
    assert(addr2 != RSP);
    assert(offset >= INT32_MIN && offset <= INT32_MAX);
    Instruction instr(0x8b);
    instr.set_modrm_and_rex_for_reg_plus_reg_plus_s32(dst.hw_id(), addr1.hw_id(), addr2.hw_id(),
                                                      offset, true);
    return instr;
  }

  static Instruction store_goal_gpr(Register addr,
                                    Register value,
                                    Register off,
                                    int offset,
                                    int size) {
    switch (size) {
      case 1:
        if (offset == 0) {
          return store8_gpr64_gpr64_plus_gpr64(addr, off, value);
        } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
          return store8_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
        } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
          return store8_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
        } else {
          assert(false);
        }
      case 2:
        if (offset == 0) {
          return store16_gpr64_gpr64_plus_gpr64(addr, off, value);
        } else if (offset >= INT8_MIN && offset <= INT8_MAX) {
          return store16_gpr64_gpr64_plus_gpr64_plus_s8(addr, off, value, offset);
        } else if (offset >= INT32_MIN && offset <= INT32_MAX) {
          return store16_gpr64_gpr64_plus_gpr64_plus_s32(addr, off, value, offset);
        } else {
          assert(false);
        }
      default:
        assert(false);
    }
  }

  /*!
   * Load memory at addr + offset, where addr is a GOAL pointer and off is the offset register.
   * This will pick the appropriate fancy addressing mode instruction.
   */
  static Instruction load_goal_gpr(Register dst,
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
          assert(false);
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
          assert(false);
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
          assert(false);
        }
      case 8:
      default:
        assert(false);
    }
  }

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   LOADS n' STORES - XMM128
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  /*!
   * Store a 128-bit xmm into an address stored in a register, no offset
   */
  static Instruction store128_gpr64_xmm128(Register gpr_addr, Register xmm_value) {
    assert(gpr_addr.is_gpr());
    assert(xmm_value.is_xmm());
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x7f);
    instr.set_modrm_and_rex_for_reg_addr(xmm_value.hw_id(), gpr_addr.hw_id(), false);
    instr.swap_op0_rex();
    return instr;
  }

  static Instruction load128_xmm128_gpr64(Register xmm_dest, Register gpr_addr) {
    assert(gpr_addr.is_gpr());
    assert(xmm_dest.is_xmm());
    Instruction instr(0x66);
    instr.set_op2(0x0f);
    instr.set_op3(0x6f);
    instr.set_modrm_and_rex_for_reg_addr(xmm_dest.hw_id(), gpr_addr.hw_id(), false);
    instr.swap_op0_rex();
    return instr;
  }

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   FUNCTION STUFF
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  /*!
   * Function return. Pops the 64-bit return address (real) off the stack and jumps to it.
   */
  static Instruction ret() { return Instruction(0xc3); }

  /*!
   * Instruction to push gpr (64-bits) onto the stack
   */
  static Instruction push_gpr64(Register reg) {
    assert(reg.is_gpr());
    if (reg.hw_id() >= 8) {
      auto i = Instruction(0x50 + reg.hw_id() - 8);
      i.set(REX(false, false, false, true));
      return i;
    }
    return Instruction(0x50 + reg.hw_id());
  }

  /*!
   * Instruction to pop 64 bit gpr from the stack
   */
  static Instruction pop_gpr64(Register reg) {
    if (reg.hw_id() >= 8) {
      auto i = Instruction(0x58 + reg.hw_id() - 8);
      i.set(REX(false, false, false, true));
      return i;
    }
    return Instruction(0x58 + reg.hw_id());
  }

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   INTEGER MATH
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  static Instruction sub_gpr64_imm8s(Register reg, int64_t imm) {
    assert(reg.is_gpr());
    assert(imm >= INT8_MIN && imm <= INT8_MAX);
    // SUB r/m64, imm8 : REX.W + 83 /5 ib
    Instruction instr(0x83);
    instr.set_modrm_and_rex(5, reg.hw_id(), 3, true);
    instr.set(Imm(1, imm));
    return instr;
  }

  static Instruction add_gpr64_imm8s(Register reg, int8_t v) {
    Instruction instr(0x83);
    instr.set_modrm_and_rex(0, reg.hw_id(), 3, true);
    instr.set(Imm(1, v));
    return instr;
  }

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   BIT STUFF
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   CONTROL FLOW
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   FLOAT MATH
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  //   UTILITIES
  //;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
};
}  // namespace emitter

#endif  // JAK_IGEN_H
