#pragma once

/*!
 * @file Register.h
 * Representation of an EE register.
 */

#include <cstdint>
#include "common/util/assert.h"
#include <string>

namespace decompiler {
// Namespace for register name constants

// Note on registers:
// Registers are assigned a unique Register ID as an integer from 0 to 164 (not including 164).
// Don't change these enums without updating the indexing scheme.
// It is important that each register is a unique register ID, and that we don't have gaps.

namespace Reg {
enum RegisterKind {
  GPR = 0,      // EE General purpose registers, these have nicknames (32 regs)
  FPR = 1,      // EE Floating point registers, just called f0 - f31 (32 regs)
  VF = 2,       // VU0 Floating point vector registers from EE, just called vf0 - vf31 (32 regs)
  VI = 3,       // VU0 Integer registers from EE, the first 16 are vi00 - vi15, the rest are control
                // regs.  (32 regs)
  COP0 = 4,     // EE COP0 Control Registers: full of fancy names (there are 32 of them) (32 regs)
  SPECIAL = 5,  // COP2 Q, ACC accessed from macro mode instructions and PCR
  MAX_KIND = 6
};

// nicknames for GPRs
enum Gpr {
  R0 = 0,   // hardcoded to zero
  AT = 1,   // temp, not used by GOAL compiler, but used by GOAL's kernel inline assembly (an other
            // places?)
  V0 = 2,   // return, temp
  V1 = 3,   // temp
  A0 = 4,   // arg0, temp
  A1 = 5,   // arg1, temp
  A2 = 6,   // arg2, temp
  A3 = 7,   // arg3, temp
  T0 = 8,   // arg4, temp
  T1 = 9,   // arg5, temp
  T2 = 10,  // arg6, temp
  T3 = 11,  // arg7, temp
  T4 = 12,  // temp
  T5 = 13,  // temp
  T6 = 14,  // temp
  T7 = 15,  // temp
  S0 = 16,  // saved
  S1 = 17,  // saved
  S2 = 18,  // saved
  S3 = 19,  // saved
  S4 = 20,  // saved
  S5 = 21,  // saved
  S6 = 22,  // process pointer
  S7 = 23,  // symbol table
  T8 = 24,  // temp
  T9 = 25,  // function pointer
  K0 = 26,  // reserved
  K1 = 27,  // reserved
  GP = 28,  // saved (C code uses this a global pointer)
  SP = 29,  // stack pointer
  FP = 30,  // global pointer (address of current function)
  RA = 31,  // return address
  MAX_GPR = 32
};

// nicknames for COP0 registers
enum Cop0 {
  INDEX = 0,
  RANDOM = 1,
  ENTRYLO0 = 2,
  ENTRYLO1 = 3,
  CONTEXT = 4,
  PAGEMASK = 5,
  WIRED = 6,
  INVALID7 = 7,
  BADVADDR = 8,
  COUNT = 9,
  ENTRYHI = 10,
  COMPARE = 11,
  COP0_STATUS = 12,
  CAUSE = 13,
  EPC = 14,
  PRID = 15,
  CONFIG = 16,
  INVALID17 = 17,
  INVALID18 = 18,
  INVALID19 = 19,
  INVALID20 = 20,
  INVALID21 = 21,
  INVALID22 = 22,
  BADPADDR = 23,
  DEBUG = 24,
  PERF = 25,
  INVALID26 = 26,
  INVALID27 = 27,
  TAGLO = 28,
  TAGHI = 29,
  ERROREPC = 30,
  INVALID31 = 31,
  MAX_COP0 = 32
};

// nicknames for COP2 Integer (VI) registers
// the first 16 are vi0 - vi15, so they don't have nicknames
enum Vi {
  COP2_STATUS = 16,
  MAC = 17,
  CLIPPING = 18,
  COP2_INVALID3 = 19,
  R = 20,
  I = 21,
  Q = 22,
  COP2_INVALID7 = 23,
  COP2_INVALID8 = 24,
  COP2_INVALID9 = 25,
  TPC = 26,
  CMSAR0 = 27,
  FBRST = 28,
  VPUSTAT = 29,
  COP2_INVALID14 = 30,
  CMSAR1 = 31,
  MAX_COP2 = 32
};

enum SpecialRegisters {
  PCR0 = 0,
  PCR1 = 1,
  MACRO_Q = 2,
  MACRO_ACC = 3,
  MAX_SPECIAL = 4,
};

const extern bool allowed_local_gprs[Reg::MAX_GPR];

constexpr int MAX_REG_ID = 32 * 5 + MAX_SPECIAL;
constexpr int MAX_VAR_REG_ID = 32 * 2;  // gprs/fprs.

}  // namespace Reg

/////////////////////////////
// Register Class
/////////////////////////////
// A register is stored as a 16-bit integer, with the top 8 bits indicating the "kind" and the lower
// 8 bits representing the register id within that kind.  If the integer is -1, it is a special
// "invalid" register used to represent an uninitialized Register.

// Note: VI / COP2 are separate "kinds" of registers, each with 16 registers.
// It might make sense to make this a single "kind" instead?
class Register {
 private:
  static constexpr int REG_CATEGORY_SHIFT = 5;
  static constexpr int REG_IDX_MASK = 0b11111;

 public:
  Register() = default;
  Register(Reg::RegisterKind kind, uint32_t num) {
    // 32 regs/category at most.
    id = (kind << REG_CATEGORY_SHIFT) | num;

    // check range:
    switch (kind) {
      case Reg::GPR:
      case Reg::FPR:
      case Reg::VF:
      case Reg::COP0:
      case Reg::VI:
        assert(num < 32);
        break;
      case Reg::SPECIAL:
        assert(num < Reg::MAX_SPECIAL);
        break;
      default:
        assert(false);
    }
  }
  explicit Register(int reg_id) {
    assert(reg_id < Reg::MAX_REG_ID);
    id = reg_id;
  }

  Register(const std::string& name);
  static Register get_arg_reg(int idx) {
    assert(idx >= 0 && idx < 8);
    return Register(Reg::GPR, Reg::A0 + idx);
  }

  uint16_t reg_id() const { return id; }
  const char* to_charp() const;
  std::string to_string() const;
  /*!
   * Get the register kind.
   */
  Reg::RegisterKind get_kind() const {
    uint16_t kind = id >> REG_CATEGORY_SHIFT;
    assert(kind < Reg::MAX_KIND);
    return (Reg::RegisterKind)kind;
  }
  bool is_vu_float() const {
    return get_kind() == Reg::VF ||
           (get_kind() == Reg::SPECIAL &&
            (get_special() == Reg::MACRO_Q || get_special() == Reg::MACRO_ACC));
  }
  bool is_gpr() const { return get_kind() == Reg::GPR; }
  Reg::Gpr get_gpr() const {
    assert(get_kind() == Reg::GPR);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < Reg::MAX_GPR);
    return (Reg::Gpr)(kind);
  }

  uint32_t get_fpr() const {
    assert(get_kind() == Reg::FPR);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < 32);
    return kind;
  }
  uint32_t get_vf() const {
    assert(get_kind() == Reg::VF);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < 32);
    return kind;
  }
  uint32_t get_vi() const {
    assert(get_kind() == Reg::VI);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < 32);
    return kind;
  }
  Reg::Cop0 get_cop0() const {
    assert(get_kind() == Reg::COP0);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < Reg::MAX_COP0);
    return (Reg::Cop0)(kind);
  }
  uint32_t get_special() const {
    assert(get_kind() == Reg::SPECIAL);
    uint16_t kind = id & REG_IDX_MASK;
    assert(kind < Reg::MAX_SPECIAL);
    return kind;
  }
  bool allowed_local_gpr() const {
    if (get_kind() != Reg::GPR) {
      return false;
    }
    return Reg::allowed_local_gprs[get_gpr()];
  }

  bool operator==(const Register& other) const { return id == other.id; }
  bool operator!=(const Register& other) const { return id != other.id; }
  bool operator<(const Register& other) const { return id < other.id; }

  struct hash {
    auto operator()(const Register& x) const { return std::hash<uint16_t>()(x.id); }
  };

 private:
  uint16_t id = -1;
};
}  // namespace decompiler
