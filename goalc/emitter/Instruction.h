#pragma once

#ifndef JAK_INSTRUCTION_H
#define JAK_INSTRUCTION_H

#include "common/common_types.h"
#include "common/util/Assert.h"

namespace emitter {
/*!
 * The ModRM byte
 */
struct ModRM {
  uint8_t mod;
  uint8_t reg_op;
  uint8_t rm;

  uint8_t operator()() const { return (mod << 6) | (reg_op << 3) | (rm << 0); }
};

/*!
 * The SIB Byte
 */
struct SIB {
  uint8_t scale, index, base;

  uint8_t operator()() const { return (scale << 6) | (index << 3) | (base << 0); }
};

/*!
 * An Immediate (either imm or disp)
 */
struct Imm {
  Imm() = default;
  Imm(uint8_t sz, uint64_t v) : size(sz), value(v) {}
  uint8_t size;
  union {
    uint64_t value;
    uint8_t v_arr[8];
  };
};

/*!
 * The REX prefix byte
 */
struct REX {
  explicit REX(bool w = false, bool r = false, bool x = false, bool b = false)
      : W(w), R(r), X(x), B(b) {}
  // W - 64-bit operands
  // R - reg extension
  // X - SIB i extnsion
  // B - other extension
  bool W, R, X, B;
  uint8_t operator()() const { return (1 << 6) | (W << 3) | (R << 2) | (X << 1) | (B << 0); }
};

enum class VexPrefix : u8 { P_NONE = 0, P_66 = 1, P_F3 = 2, P_F2 = 3 };

/*!
 * The "VEX" 3-byte format for AVX instructions
 */
struct VEX3 {
  bool W, R, X, B;
  enum class LeadingBytes : u8 { P_INVALID = 0, P_0F = 1, P_0F_38 = 2, P_0F_3A = 3 } leading_bytes;
  u8 reg_id;
  VexPrefix prefix;
  bool L;

  u8 emit(u8 byte) const {
    if (byte == 0) {
      return 0b11000100;
    } else if (byte == 1) {
      u8 result = 0;
      result |= ((!R) << 7);
      result |= ((!X) << 6);
      result |= ((!B) << 5);
      result |= (0b11111 & u8(leading_bytes));
      return result;
    } else if (byte == 2) {
      u8 result = 0;
      result |= (W << 7);  // this may be inverted?
      result |= ((~reg_id) & 0b1111) << 3;
      result |= (L << 2);
      result |= (u8(prefix) & 0b11);
      return result;
    } else {
      ASSERT(false);
      return -1;
    }
  }

  VEX3(bool w,
       bool r,
       bool x,
       bool b,
       LeadingBytes _leading_bytes,
       u8 _reg_id = 0,
       VexPrefix _prefix = VexPrefix::P_NONE,
       bool l = false)
      : W(w),
        R(r),
        X(x),
        B(b),
        leading_bytes(_leading_bytes),
        reg_id(_reg_id),
        prefix(_prefix),
        L(l) {}
};

struct VEX2 {
  bool R;
  u8 reg_id;
  VexPrefix prefix;
  bool L;

  u8 emit(u8 byte) const {
    if (byte == 0) {
      return 0b11000101;
    } else if (byte == 1) {
      u8 result = 0;
      result |= ((!R) << 7);
      result |= ((~reg_id) & 0b1111) << 3;
      result |= (L << 2);
      result |= (u8(prefix) & 0b11);
      return result;
    } else {
      ASSERT(false);
      return -1;
    }
  }

  VEX2(bool r, u8 _reg_id = 0, VexPrefix _prefix = VexPrefix::P_NONE, bool l = false)
      : R(r), reg_id(_reg_id), prefix(_prefix), L(l) {}
};

/*!
 * A high-level description of an x86-64 opcode.  It can emit itself.
 */
struct Instruction {
  Instruction(uint8_t opcode) : op(opcode) {}
  uint8_t op;

  enum Flags {
    kOp2Set = (1 << 0),
    kOp3Set = (1 << 1),
    kIsNull = (1 << 2),
    kSetRex = (1 << 3),
    kSetModrm = (1 << 4),
    kSetSib = (1 << 5),
    kSetDispImm = (1 << 6),
    kSetImm = (1 << 7),
  };

  u8 m_flags = 0;

  uint8_t op2;

  uint8_t op3;

  u8 n_vex = 0;
  uint8_t vex[3] = {0, 0, 0};

  // the rex byte
  uint8_t m_rex = 0;

  // the modrm byte
  uint8_t m_modrm = 0;

  // the sib byte
  uint8_t m_sib = 0;

  // the displacement
  Imm disp;

  // the immediate
  Imm imm;

  /*!
   * Move opcode byte 0 to before the rex prefix.
   */
  void swap_op0_rex() {
    if (!(m_flags & kSetRex))
      return;
    auto temp = op;
    op = m_rex;
    m_rex = temp;
  }

  void set(REX r) {
    m_rex = r();
    m_flags |= kSetRex;
  }

  void set(ModRM modrm) {
    m_modrm = modrm();
    m_flags |= kSetModrm;
  }

  void set(SIB sib) {
    m_sib = sib();
    m_flags |= kSetSib;
  }

  void set(VEX3 vex3) {
    n_vex = 3;
    for (int i = 0; i < n_vex; i++) {
      vex[i] = vex3.emit(i);
    }
  }

  void set(VEX2 vex2) {
    n_vex = 2;
    for (int i = 0; i < n_vex; i++) {
      vex[i] = vex2.emit(i);
    }
  }

  void set_disp(Imm i) {
    disp = i;
    m_flags |= kSetDispImm;
  }

  void set(Imm i) {
    imm = i;
    m_flags |= kSetImm;
  }

  void set_op2(uint8_t b) {
    m_flags |= kOp2Set;
    op2 = b;
  }

  void set_op3(uint8_t b) {
    m_flags |= kOp3Set;
    op3 = b;
  }

  int get_imm_size() const {
    if (m_flags & kSetImm) {
      return imm.size;
    } else {
      return 0;
    }
  }

  int get_disp_size() const {
    if (m_flags & kSetDispImm) {
      return disp.size;
    } else {
      return 0;
    }
  }

  /*!
   * Set modrm and rex as needed for two regs.
   */
  void set_modrm_and_rex(uint8_t reg, uint8_t rm, uint8_t mod, bool rex_w = false) {
    bool rex_b = false, rex_r = false;

    if (rm >= 8) {
      rm -= 8;
      rex_b = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = mod;
    modrm.reg_op = reg;
    modrm.rm = rm;

    set(modrm);

    if (rex_b || rex_w || rex_r) {
      set(REX(rex_w, rex_r, false, rex_b));
    }
  }

  void set_vex_modrm_and_rex(uint8_t reg,
                             uint8_t rm,
                             VEX3::LeadingBytes lb,
                             uint8_t vex_reg = 0,
                             bool rex_w = false,
                             VexPrefix prefix = VexPrefix::P_NONE) {
    bool rex_b = false, rex_r = false;

    if (rm >= 8) {
      rm -= 8;
      rex_b = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 3;
    modrm.reg_op = reg;
    modrm.rm = rm;

    set(modrm);
    if (rex_b || rex_w || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, false, rex_b, lb, vex_reg, prefix));
    } else {
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_b);
      ASSERT(!rex_w);
      set(VEX2(rex_r, vex_reg, prefix));
    }
  }

  /*!
   * Set VEX prefix for REX as needed for two registers.
   */
  void set_vex_modrm_and_rex(uint8_t reg,
                             uint8_t rm,
                             uint8_t mod,
                             VEX3::LeadingBytes lb,
                             bool rex_w = false) {
    bool rex_b = false;
    bool rex_r = false;
    if (rm >= 8) {
      rm -= 8;
      rex_b = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = mod;
    modrm.reg_op = reg;
    modrm.rm = rm;
    set(modrm);
    if (rex_b || rex_w || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, false, rex_b, lb));
    } else {
      // can get away with two byte version
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_b);
      ASSERT(!rex_w);
      set(VEX2(rex_r));
    }
  }

  void set_modrm_and_rex_for_reg_plus_reg_plus_s8(uint8_t reg,
                                                  uint8_t addr1,
                                                  uint8_t addr2,
                                                  s8 offset,
                                                  bool rex_w) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 1;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    Imm imm2(1, offset);

    // default  addr1 in index
    if (addr1 == 4) {
      sib.index = addr2;
      sib.base = addr1;
      rex_x = addr2_ext;
      rex_b = addr1_ext;
    } else {
      // addr1 in index
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
    }
    ASSERT(sib.index != 4);

    if (rex_b || rex_w || rex_r || rex_x) {
      set(REX(rex_w, rex_r, rex_x, rex_b));
    }

    set(modrm);
    set(sib);
    set_disp(imm2);
  }

  void set_vex_modrm_and_rex_for_reg_plus_reg_plus_s8(uint8_t reg,
                                                      uint8_t addr1,
                                                      uint8_t addr2,
                                                      s8 offset,
                                                      VEX3::LeadingBytes lb,
                                                      bool rex_w) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 1;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    Imm imm2(1, offset);

    // default  addr1 in index
    if (addr1 == 4) {
      sib.index = addr2;
      sib.base = addr1;
      rex_x = addr2_ext;
      rex_b = addr1_ext;
    } else {
      // addr1 in index
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
    }
    ASSERT(sib.index != 4);

    if (rex_b || rex_w || rex_x || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, rex_x, rex_b, lb));
    } else {
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_b);
      ASSERT(!rex_w);
      ASSERT(!rex_x);
      set(VEX2(rex_r));
    }

    set(modrm);
    set(sib);
    set_disp(imm2);
  }

  void set_modrm_and_rex_for_reg_plus_reg_plus_s32(uint8_t reg,
                                                   uint8_t addr1,
                                                   uint8_t addr2,
                                                   s32 offset,
                                                   bool rex_w) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 2;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    Imm imm2(4, offset);

    // default  addr1 in index
    if (addr1 == 4) {
      sib.index = addr2;
      sib.base = addr1;
      rex_x = addr2_ext;
      rex_b = addr1_ext;
    } else {
      // addr1 in index
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
    }
    ASSERT(sib.index != 4);

    if (rex_b || rex_w || rex_r || rex_x) {
      set(REX(rex_w, rex_r, rex_x, rex_b));
    }

    set(modrm);
    set(sib);
    set_disp(imm2);
  }

  void set_vex_modrm_and_rex_for_reg_plus_reg_plus_s32(uint8_t reg,
                                                       uint8_t addr1,
                                                       uint8_t addr2,
                                                       s32 offset,
                                                       VEX3::LeadingBytes lb,
                                                       bool rex_w) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 2;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    Imm imm2(4, offset);

    // default  addr1 in index
    if (addr1 == 4) {
      sib.index = addr2;
      sib.base = addr1;
      rex_x = addr2_ext;
      rex_b = addr1_ext;
    } else {
      // addr1 in index
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
    }
    ASSERT(sib.index != 4);

    if (rex_b || rex_w || rex_x || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, rex_x, rex_b, lb));
    } else {
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_b);
      ASSERT(!rex_w);
      ASSERT(!rex_x);
      set(VEX2(rex_r));
    }

    set(modrm);
    set(sib);
    set_disp(imm2);
  }

  void set_modrm_and_rex_for_reg_plus_reg_addr(uint8_t reg,
                                               uint8_t addr1,
                                               uint8_t addr2,
                                               bool rex_w = false,
                                               bool rex_always = false) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 0;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    if (addr1 == 5 && addr2 == 5) {
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
      modrm.mod = 1;
      set_disp(Imm(1, 0));

    } else {
      // default  addr1 in index
      bool flipped = (addr1 == 4) || (addr2 == 5);

      if (flipped) {
        sib.index = addr2;
        sib.base = addr1;
        rex_x = addr2_ext;
        rex_b = addr1_ext;
      } else {
        // addr1 in index
        sib.index = addr1;
        sib.base = addr2;
        rex_x = addr1_ext;
        rex_b = addr2_ext;
      }
      ASSERT(sib.base != 5);
      ASSERT(sib.index != 4);
    }

    if (rex_b || rex_w || rex_r || rex_x || rex_always) {
      set(REX(rex_w, rex_r, rex_x, rex_b));
    }

    set(modrm);
    set(sib);
  }

  void set_vex_modrm_and_rex_for_reg_plus_reg_addr(uint8_t reg,
                                                   uint8_t addr1,
                                                   uint8_t addr2,
                                                   VEX3::LeadingBytes lb,
                                                   bool rex_w = false) {
    bool rex_b = false, rex_r = false, rex_x = false;
    bool addr1_ext = false;
    bool addr2_ext = false;

    if (addr1 >= 8) {
      addr1 -= 8;
      addr1_ext = true;
    }

    if (addr2 >= 8) {
      addr2 -= 8;
      addr2_ext = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 0;  // no disp
    modrm.rm = 4;   // sib!
    modrm.reg_op = reg;

    SIB sib;
    sib.scale = 0;

    if (addr1 == 5 && addr2 == 5) {
      sib.index = addr1;
      sib.base = addr2;
      rex_x = addr1_ext;
      rex_b = addr2_ext;
      modrm.mod = 1;
      set_disp(Imm(1, 0));

    } else {
      // default  addr1 in index
      bool flipped = (addr1 == 4) || (addr2 == 5);

      if (flipped) {
        sib.index = addr2;
        sib.base = addr1;
        rex_x = addr2_ext;
        rex_b = addr1_ext;
      } else {
        // addr1 in index
        sib.index = addr1;
        sib.base = addr2;
        rex_x = addr1_ext;
        rex_b = addr2_ext;
      }
      ASSERT(sib.base != 5);
      ASSERT(sib.index != 4);
    }

    if (rex_b || rex_w || rex_x || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, rex_x, rex_b, lb));
    } else {
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_b);
      ASSERT(!rex_w);
      ASSERT(!rex_x);
      set(VEX2(rex_r));
    }

    set(modrm);
    set(sib);
  }

  /*!
   * Set modrm and rex as needed for two regs for an addressing mode.
   * Will set SIB if R12 or RSP indexing is used.
   */
  void set_modrm_and_rex_for_reg_addr(uint8_t reg, uint8_t rm, bool rex_w = false) {
    bool rex_b = false, rex_r = false;

    if (rm >= 8) {
      rm -= 8;
      rex_b = true;
    }

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 0;
    modrm.reg_op = reg;
    modrm.rm = rm;

    if (rm == 4) {
      SIB sib;
      sib.scale = 0;
      sib.base = 4;
      sib.index = 4;

      set(sib);
    }

    if (rm == 5) {
      modrm.mod = 1;  // 1 byte imm
      set_disp(Imm(1, 0));
    }

    set(modrm);
    if (rex_b || rex_w || rex_r) {
      set(REX(rex_w, rex_r, false, rex_b));
    }
  }

  void set_modrm_and_rex_for_rip_plus_s32(uint8_t reg, s32 offset, bool rex_w = false) {
    bool rex_r = false;

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 0;
    modrm.reg_op = reg;
    modrm.rm = 5;  // use the RIP addressing mode
    set(modrm);

    if (rex_r || rex_w) {
      set(REX(rex_w, rex_r, false, false));
    }

    set_disp(Imm(4, offset));
  }

  void add_rex() {
    if (!(m_flags & kSetRex)) {
      set(REX());
    }
  }

  void set_vex_modrm_and_rex_for_rip_plus_s32(uint8_t reg,
                                              s32 offset,
                                              VEX3::LeadingBytes lb = VEX3::LeadingBytes::P_0F,
                                              bool rex_w = false) {
    bool rex_r = false;

    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }

    ModRM modrm;
    modrm.mod = 0;
    modrm.reg_op = reg;
    modrm.rm = 5;  // use the RIP addressing mode
    set(modrm);

    if (rex_w || lb != VEX3::LeadingBytes::P_0F) {
      // need three byte version
      set(VEX3(rex_w, rex_r, false, false, lb));
    } else {
      ASSERT(lb == VEX3::LeadingBytes::P_0F);  // vex2 implies 0x0f
      ASSERT(!rex_w);
      set(VEX2(rex_r));
    }

    set_disp(Imm(4, offset));
  }

  /*!
   * Set up modrm and rex for the commonly used immediate displacement indexing mode.
   */
  void set_modrm_rex_sib_for_reg_reg_disp(uint8_t reg, uint8_t mod, uint8_t rm, bool rex_w) {
    ModRM modrm;

    bool rex_r = false;
    if (reg >= 8) {
      reg -= 8;
      rex_r = true;
    }
    modrm.reg_op = reg;

    modrm.mod = mod;

    modrm.rm = 4;  // use sib

    SIB sib;
    sib.scale = 0;
    sib.index = 4;
    bool rex_b = false;
    if (rm >= 8) {
      rex_b = true;
      rm -= 8;
    }

    sib.base = rm;

    set(modrm);
    set(sib);

    if (rex_r || rex_w || rex_b) {
      set(REX(rex_w, rex_r, false, rex_b));
    }
  }

  /*!
   * Get the position of the disp immediate relative to the start of the instruction
   */
  int offset_of_disp() const {
    if (m_flags & kIsNull)
      return 0;
    ASSERT(m_flags & kSetDispImm);
    int offset = 0;
    offset += n_vex;
    if (m_flags & kSetRex)
      offset++;
    offset++;  // opcode
    if (m_flags & kOp2Set)
      offset++;
    if (m_flags & kOp3Set)
      offset++;
    if (m_flags & kSetModrm)
      offset++;
    if (m_flags & kSetSib)
      offset++;
    return offset;
  }

  /*!
   * Get the position of the imm immediate relative to the start of the instruction
   */
  int offset_of_imm() const {
    if (m_flags & kIsNull)
      return 0;
    ASSERT(m_flags & kSetImm);
    int offset = 0;
    offset += n_vex;
    if (m_flags & kSetRex)
      offset++;
    offset++;  // opcode
    if (m_flags & kOp2Set)
      offset++;
    if (m_flags & kOp3Set)
      offset++;
    if (m_flags & kSetModrm)
      offset++;
    if (m_flags & kSetSib)
      offset++;
    if (m_flags & kSetDispImm)
      offset += disp.size;
    return offset;
  }

  /*!
   * Emit into a buffer and return how many bytes written (can be zero)
   */
  uint8_t emit(uint8_t* buffer) const {
    if (m_flags & kIsNull)
      return 0;
    uint8_t count = 0;

    for (int i = 0; i < n_vex; i++) {
      buffer[count++] = vex[i];
    }

    if (m_flags & kSetRex) {
      buffer[count++] = m_rex;
    }

    buffer[count++] = op;

    if (m_flags & kOp2Set) {
      buffer[count++] = op2;
    }

    if (m_flags & kOp3Set) {
      buffer[count++] = op3;
    }

    if (m_flags & kSetModrm) {
      buffer[count++] = m_modrm;
    }

    if (m_flags & kSetSib) {
      buffer[count++] = m_sib;
    }

    if (m_flags & kSetDispImm) {
      for (int i = 0; i < disp.size; i++) {
        buffer[count++] = disp.v_arr[i];
      }
    }

    if (m_flags & kSetImm) {
      for (int i = 0; i < imm.size; i++) {
        buffer[count++] = imm.v_arr[i];
      }
    }
    return count;
  }

  uint8_t length() const {
    if (m_flags & kIsNull)
      return 0;
    uint8_t count = 0;

    count += n_vex;

    if (m_flags & kSetRex) {
      count++;
    }

    count++;

    if (m_flags & kOp2Set) {
      count++;
    }

    if (m_flags & kOp3Set) {
      count++;
    }

    if (m_flags & kSetModrm) {
      count++;
    }

    if (m_flags & kSetSib) {
      count++;
    }

    if (m_flags & kSetDispImm) {
      for (int i = 0; i < disp.size; i++) {
        count++;
      }
    }

    if (m_flags & kSetImm) {
      for (int i = 0; i < imm.size; i++) {
        count++;
      }
    }
    return count;
  }
};
}  // namespace emitter

#endif  // JAK_INSTRUCTION_H
