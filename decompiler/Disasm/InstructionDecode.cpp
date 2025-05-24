/*!
 * @file InstructionDecode.cpp
 * The Instruction Decoder - converts a LinkedWord into a Instruction.
 * This is the part of the disassembler that decodes MIPS instructions.
 */

#include "InstructionDecode.h"

#include "common/util/Assert.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "fmt/core.h"

namespace decompiler {
// utility class to extract fields of an opcode.
struct OpcodeFields {
  OpcodeFields(uint32_t _data) : data(_data) {}

  // 26 - 31
  uint32_t op() { return (data >> 26); }

  //////////////
  // R - Type //
  //////////////

  // 21 - 25
  uint32_t rs() { return (data >> 21) & 0x1f; }

  // 16 - 20
  uint32_t rt() { return (data >> 16) & 0x1f; }

  // 11 - 15
  uint32_t rd() { return (data >> 11) & 0x1f; }

  //  6 - 10
  uint32_t sa() { return (data >> 6) & 0x1f; }

  // 0 - 5
  // TODO - remove once we update clang-format
  // clang-format off
  uint32_t function() { return (data)&0x3f; }
  // clang-format on

  ////////////////
  // Immediates //
  ////////////////

  int32_t simm16() { return (int16_t)(data); }
  int32_t zimm16() { return (uint16_t)(data); }
  uint32_t imm5() { return (data >> 6) & 0x1f; }
  uint32_t imm15() { return (data >> 6) & 0b111111111111111; }

  ////////////////////
  // Floating Point //
  ////////////////////

  uint32_t cop_func() { return (data >> 21) & 0x1f; }
  uint32_t ft() { return (data >> 16) & 0x1f; }
  uint32_t fs() { return (data >> 11) & 0x1f; }
  uint32_t fd() { return (data >> 6) & 0x1f; }

  ////////////
  // Others //
  ////////////

  uint32_t pcreg() { return (data >> 1) & 0x1f; }
  uint32_t syscall() { return (data >> 6) & 0xfffff; }
  uint32_t MMI_func() { return (data >> 6) & 0x1f; }
  uint32_t lower11() { return (uint32_t)(data & 0x7ff); }
  uint32_t lower6() { return (uint32_t)(data & 0b111111); }
  uint32_t dest() { return (data >> 21) & 0b1111; }
  uint32_t fs_f() { return (data >> 21) & 0b11; }
  uint32_t ft_f() { return (data >> 23) & 0b11; }

  uint32_t data;
};

//////////////////
// OPCODE DECODE
//////////////////

static InstructionKind decode_cop2(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.lower11()) {
    case 0b0:
    case 0b1:
      switch (fields.cop_func()) {
        case 0b00001:
          return IK::QMFC2;

        case 0b00101:
          ASSERT(((fields.data >> 1) & (0b1111111111)) == 0);
          return IK::QMTC2;

        case 0b00010:
          return IK::CFC2;

        case 0b00110:
          return IK::CTC2;

        default:
          ASSERT(false);
          return IK::UNKNOWN;
      }
      break;

    case 0b00010111100:
    case 0b00010111101:
    case 0b00010111110:
    case 0b00010111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VMADDA_BC;

    case 0b00000111100:
    case 0b00000111101:
    case 0b00000111110:
    case 0b00000111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VADDA_BC;

    case 0b00110111100:
    case 0b00110111101:
    case 0b00110111110:
    case 0b00110111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VMULA_BC;

    case 0b01010111110:
      ASSERT(fields.data & (1 << 25));
      return IK::VMULA;

    case 0b01010111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VADDA;

    case 0b01010111101:
      ASSERT(fields.data & (1 << 25));
      return IK::VMADDA;

    case 0b00011111100:
    case 0b00011111101:
    case 0b00011111110:
    case 0b00011111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VMSUBA_BC;

    case 0b00101111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VFTOI0;

    case 0b00101111101:
      ASSERT(fields.data & (1 << 25));
      return IK::VFTOI4;

    case 0b00101111110:
      ASSERT(fields.data & (1 << 25));
      return IK::VFTOI12;

    case 0b00101111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VFTOI15;

    case 0b00100111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VITOF0;

    case 0b00100111110:
      ASSERT(fields.data & (1 << 25));
      return IK::VITOF12;

    case 0b00100111111:
      ASSERT(fields.data & (1 << 25));
      return IK::VITOF15;

    case 0b00111111100:
      ASSERT(fields.data & (1 << 25));
      ASSERT(fields.ft() == 0);
      return IK::VMULAQ;

    case 0b00111111101:
      ASSERT(fields.data & (1 << 25));
      return IK::VABS;

    case 0b00111111111:
      ASSERT(fields.data & (1 << 25));
      ASSERT(fields.dest() == 0b1110);
      return IK::VCLIP;

    case 0b01011111111:
      ASSERT(fields.dest() == 0);
      ASSERT(fields.ft() == 0);
      ASSERT(fields.fs() == 0);
      return IK::VNOP;
    case 0b01101111101:
      ASSERT(fields.data & (1 << 25));
      return IK::VSQI;
    case 0b01101111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VLQI;
    case 0b01110111111:
      ASSERT(fields.dest() == 0);
      ASSERT(fields.ft() == 0);
      ASSERT(fields.fs() == 0);
      return IK::VWAITQ;

    case 0b01011111110:
      ASSERT(fields.dest() == 0b1110);
      ASSERT(fields.data & (1 << 25));
      return IK::VOPMULA;

    case 0b01011111101:
      return IK::VMSUBA;

    case 0b01100111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VMOVE;

    case 0b01100111101:
      return IK::VMR32;

    case 0b01110111100:
      ASSERT(fields.data & (1 << 25));
      return IK::VDIV;

    case 0b01110111101:
      ASSERT(fields.fs() == 0);
      ASSERT(((fields.data >> 21) & 3) == 0);
      ASSERT(fields.data & (1 << 25));
      return IK::VSQRT;

    case 0b01111111100:
      ASSERT(((fields.data >> 23) & 3) == 0);
      ASSERT(fields.data & (1 << 25));
      return IK::VMTIR;

    case 0b01110111110:
      ASSERT(fields.data & (1 << 25));
      return IK::VRSQRT;

    case 0b10000111100:
      ASSERT(fields.fs() == 0);
      ASSERT(fields.data & (1 << 25));
      return IK::VRNEXT;

    case 0b10000111101:
      ASSERT(fields.fs() == 0);
      ASSERT(fields.data & (1 << 25));
      return IK::VRGET;

    case 0b10000111111:
      ASSERT(fields.ft() == 0);
      ASSERT(fields.data & (1 << 25));
      ASSERT(((fields.data >> 23) & 3) == 0);
      return IK::VRXOR;
    default:

      switch (fields.lower6()) {
        case 0b000000:
        case 0b000001:
        case 0b000010:
        case 0b000011:
          ASSERT(fields.data & (1 << 25));
          return IK::VADD_BC;

        case 0b000100:
        case 0b000101:
        case 0b000110:
        case 0b000111:
          ASSERT(fields.data & (1 << 25));
          return IK::VSUB_BC;

        case 0b001000:
        case 0b001001:
        case 0b001010:
        case 0b001011:
          ASSERT(fields.data & (1 << 25));
          return IK::VMADD_BC;

        case 0b001100:
        case 0b001101:
        case 0b001110:
        case 0b001111:
          ASSERT(fields.data & (1 << 25));
          return IK::VMSUB_BC;

        case 0b010000:
        case 0b010001:
        case 0b010010:
        case 0b010011:
          ASSERT(fields.data & (1 << 25));
          return IK::VMAX_BC;

        case 0b010100:
        case 0b010101:
        case 0b010110:
        case 0b010111:
          ASSERT(fields.data & (1 << 25));
          return IK::VMINI_BC;

        case 0b011000:
        case 0b011001:
        case 0b011010:
        case 0b011011:
          ASSERT(fields.data & (1 << 25));
          return IK::VMUL_BC;

        case 0b011100:
          ASSERT(fields.ft() == 0);
          ASSERT(fields.data & (1 << 25));
          return IK::VMULQ;

        case 0b100000:
          ASSERT(fields.data & (1 << 25));
          return IK::VADDQ;

        case 0b100100:
          ASSERT(fields.data & (1 << 25));
          return IK::VSUBQ;

        case 0b100101:
          ASSERT(fields.ft() == 0);
          ASSERT(fields.data & (1 << 25));
          return IK::VMSUBQ;

        case 0b101000:
          ASSERT(fields.data & (1 << 25));
          return IK::VADD;
        case 0b101001:
          ASSERT(fields.data & (1 << 25));
          return IK::VMADD;
        case 0b101010:
          ASSERT(fields.data & (1 << 25));
          return IK::VMUL;
        case 0b101011:
          ASSERT(fields.data & (1 << 25));
          return IK::VMAX;
        case 0b101100:
          ASSERT(fields.data & (1 << 25));
          return IK::VSUB;
        case 0b101101:
          ASSERT(fields.data & (1 << 25));
          return IK::VMSUB;
        case 0b101110:
          ASSERT(fields.data & (1 << 25));
          ASSERT(fields.dest() == 0b1110);
          return IK::VOPMSUB;
        case 0b101111:
          ASSERT(fields.data & (1 << 25));
          return IK::VMINI;
        case 0b110010:
          ASSERT(fields.data & (1 << 25));
          ASSERT(fields.dest() == 0b0);
          return IK::VIADDI;
        case 0b110100:
          ASSERT(fields.data & (1 << 25));
          ASSERT(fields.dest() == 0b0);
          return IK::VIAND;
        case 0b111000:
          ASSERT(fields.data & (1 << 25));
          ASSERT(fields.dest() == 0b0);
          return IK::VCALLMS;
        default:
          ASSERT_MSG(false, fmt::format("unknown cop2 lower11 case 0b{:b}\n", fields.lower11()));
          return IK::UNKNOWN;
      }
  }
}

static InstructionKind decode_W(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.function()) {
    case 0b100000:
      ASSERT(fields.ft() == 0);
      return IK::CVTSW;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_S(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.function()) {
    case 0b000000:
      return IK::ADDS;
    case 0b000001:
      return IK::SUBS;
    case 0b000010:
      return IK::MULS;
    case 0b000011:
      return IK::DIVS;
    case 0b000101:
      return IK::ABSS;
    case 0b000110:
      ASSERT(fields.ft() == 0);
      return IK::MOVS;
    case 0b000111:
      ASSERT(fields.ft() == 0);
      return IK::NEGS;
    case 0b000100:
      ASSERT(fields.fs() == 0);
      return IK::SQRTS;
    case 0b010110:
      return IK::RSQRTS;
    case 0b011000:
      ASSERT(fields.fd() == 0);
      return IK::ADDAS;
    case 0b011010:
      ASSERT(fields.fd() == 0);
      return IK::MULAS;
    case 0b011100:
      return IK::MADDS;
    case 0b011101:
      return IK::MSUBS;
    case 0b011110:
      ASSERT(fields.fd() == 0);
      return IK::MADDAS;
    case 0b011111:
      ASSERT(fields.fd() == 0);
      return IK::MSUBAS;
    case 0b100100:
      ASSERT(fields.ft() == 0);
      return IK::CVTWS;
    case 0b101000:
      return IK::MAXS;
    case 0b101001:
      return IK::MINS;
    case 0b110010:
      ASSERT(fields.fd() == 0);
      return IK::CEQS;
    case 0b110100:
      ASSERT(fields.fd() == 0);
      return IK::CLTS;
    case 0b110110:
      ASSERT(fields.fd() == 0);
      return IK::CLES;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_BC1(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.ft()) {
    case 0b00000:
      return IK::BC1F;
    case 0b00001:
      return IK::BC1T;
    case 0b00010:
      return IK::BC1FL;
    case 0b00011:
      return IK::BC1TL;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_cop1(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.cop_func()) {
    case 0b00000:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.function() == 0);
      return IK::MFC1;
    case 0b00100:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.function() == 0);
      return IK::MTC1;
    case 0b01000:
      return decode_BC1(fields);
    case 0b10000:
      return decode_S(fields);
    case 0b10100:
      return decode_W(fields);
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_c0(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.function()) {
    case 0b011000:
      return IK::ERET;
    case 0b111000:
      ASSERT(fields.sa() == 0 && fields.rd() == 0 && fields.rt() == 0);
      return IK::EI;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_mt0(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.lower11()) {
    case 0b00000000000:
      return IK::MTC0;
    case 0b00000000100:
      ASSERT(fields.rd() == 0b11000);
      return IK::MTDAB;
    case 0b00000000101:
      ASSERT(fields.rd() == 0b11000);
      return IK::MTDABM;
    default:
      if (fields.rd() == 0b11001 && fields.sa() == 0 && (fields.data & 1) == 1) {
        return IK::MTPC;
      } else {
        ASSERT(false);
        return IK::UNKNOWN;
      }
  }
}

static InstructionKind decode_mf0(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.lower11()) {
    case 0b0:
      return IK::MFC0;
    default:
      if (fields.rd() == 0b11001 && fields.sa() == 0 && (fields.data & 1) == 1) {
        return IK::MFPC;
      } else {
        ASSERT(false);
        return IK::UNKNOWN;
      }
  }
}

static InstructionKind decode_cop0(OpcodeFields fields) {
  switch (fields.cop_func()) {
    case 0b00000:
      return decode_mf0(fields);
    case 0b00100:
      return decode_mt0(fields);
    case 0b10000:
      return decode_c0(fields);
    default:
      ASSERT(false);
      return InstructionKind::UNKNOWN;
  }
}

static InstructionKind decode_mmi3(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.MMI_func()) {
    case 0b01010:
      return IK::PINTEH;
    case 0b01110:
      return IK::PCPYUD;
    case 0b10010:
      return IK::POR;
    case 0b10011:
      return IK::PNOR;
    case 0b11011:
      ASSERT(fields.rs() == 0);
      return IK::PCPYH;
    case 0b11110:
      return IK::PEXCW;
    default:
      ASSERT_MSG(false, fmt::format("unknown mmi3: 0b{:b}\n", fields.MMI_func()));
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_mmi2(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.MMI_func()) {
    case 0b01110:
      return IK::PCPYLD;
    case 0b10000:
      return IK::PMADDH;
    case 0b10010:
      return IK::PAND;
    case 0b10011:
      return IK::PXOR;
    case 0b11100:
      return IK::PMULTH;
    case 0b11110:
      return IK::PEXEW;
    case 0b11111:
      return IK::PROT3W;
    default:
      ASSERT_MSG(false, fmt::format("unknown mmi2: 0b{:b}\n", fields.MMI_func()));
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_mmi1(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.MMI_func()) {
    case 0b00001:
      return IK::PABSW;
    case 0b00010:
      return IK::PCEQW;
    case 0b00011:
      return IK::PMINW;
    case 0b00111:
      return IK::PMINH;
    case 0b01010:
      return IK::PCEQB;
    case 0b10010:
      return IK::PEXTUW;
    case 0b10110:
      return IK::PEXTUH;
    case 0b11010:
      return IK::PEXTUB;
    case 0b11011:
      return IK::QFSRV;
    default:
      ASSERT_MSG(false, fmt::format("unknown mmi1: 0b{:b}\n", fields.MMI_func()));
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_mmi0(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.MMI_func()) {
    case 0b00000:
      return IK::PADDW;
    case 0b00001:
      return IK::PSUBW;
    case 0b00010:
      return IK::PCGTW;
    case 0b00011:
      return IK::PMAXW;
    case 0b00100:
      return IK::PADDH;
    case 0b00101:
      return IK::PSUBH;
    case 0b00111:
      return IK::PMAXH;
    case 0b01000:
      return IK::PADDB;
    case 0b01010:
      return IK::PCGTB;
    case 0b10010:
      return IK::PEXTLW;
    case 0b10011:
      return IK::PPACW;
    case 0b10111:
      return IK::PPACH;
    case 0b10110:
      return IK::PEXTLH;
    case 0b11010:
      return IK::PEXTLB;
    case 0b11011:
      return IK::PPACB;
    default:
      ASSERT_MSG(false, fmt::format("unknown mmi0: 0b{:b}\n", fields.MMI_func()));
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_pmfhl(OpcodeFields fields) {
  // the PMFHL instruction is split into several types, and we create different instructions for
  // each.
  typedef InstructionKind IK;
  switch (fields.sa()) {
    case 0b00001:
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      return IK::PMFHL_UW;
    case 0b00000:
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      return IK::PMFHL_LW;
    case 0b00011:
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      return IK::PMFHL_LH;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_mmi(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.function()) {
    case 0b000100:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rt() == 0);
      return IK::PLZCW;
    case 0b001000:
      return decode_mmi0(fields);
    case 0b001001:
      return decode_mmi2(fields);

    case 0b010011:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rd() == 0);
      ASSERT(fields.rt() == 0);
      return IK::MTLO1;
    case 0b010010:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      return IK::MFLO1;

    case 0b101000:
      return decode_mmi1(fields);
    case 0b101001:
      return decode_mmi3(fields);
    case 0b110000:
      return decode_pmfhl(fields);
    case 0b110100:
      return IK::PSLLH;
    case 0b110110:
      return IK::PSRLH;
    case 0b110111:
      return IK::PSRAH;
    case 0b111100:
      return IK::PSLLW;
    case 0b111111:
      return IK::PSRAW;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_regimm(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.rt()) {
    case 0b00000:
      return IK::BLTZ;
    case 0b00001:
      return IK::BGEZ;
    case 0b00010:
      return IK::BLTZL;
    case 0b00011:
      return IK::BGEZL;
    case 0b10001:
      return IK::BGEZAL;
    case 0b11000:
      return IK::MTSAB;
    default:
      ASSERT_MSG(false, fmt::format("unknown regimm: 0b{:b}\n", fields.rt()));
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_sync(OpcodeFields fields) {
  // the "sync" opcode has a "stype" field which picks between P and L type syncs.
  // to avoid implementing this, we just split SYNC into two separate instructions.
  typedef InstructionKind IK;
  auto stype = fields.sa();
  ASSERT(fields.rt() == 0);
  ASSERT(fields.rs() == 0);
  ASSERT(fields.rd() == 0);
  if (stype == 0b00000) {
    return IK::SYNCL;
  } else if (stype == 0b10000) {
    return IK::SYNCP;
  } else {
    ASSERT(false);
    return IK::UNKNOWN;
  }
}

static InstructionKind decode_special(OpcodeFields fields) {
  typedef InstructionKind IK;
  switch (fields.function()) {
    case 0b000000:
      ASSERT(fields.rs() == 0);
      return IK::SLL;
    // RESERVED
    case 0b000010:
      ASSERT(fields.rs() == 0);
      return IK::SRL;
    case 0b000011:
      ASSERT(fields.rs() == 0);
      return IK::SRA;
    case 0b000100:
      ASSERT(fields.sa() == 0);
      return IK::SLLV;
    // RESERVED
    // SRLV
    // SRAV
    case 0b001000:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rd() == 0);
      ASSERT(fields.rt() == 0);
      return IK::JR;
    case 0b001001:
      ASSERT(fields.rt() == 0);
      ASSERT(fields.sa() == 0);
      return IK::JALR;
    case 0b001010:
      ASSERT(fields.sa() == 0);
      return IK::MOVZ;
    case 0b001011:
      ASSERT(fields.sa() == 0);
      return IK::MOVN;
    case 0b001100:
      return IK::SYSCALL;
    // BREAK
    // RESERVED
    case 0b001111:
      return decode_sync(fields);

    case 0b010000:
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      ASSERT(fields.sa() == 0);
      return IK::MFHI;
    // MTHI
    case 0b010010:
      ASSERT(fields.rs() == 0);
      ASSERT(fields.rt() == 0);
      ASSERT(fields.sa() == 0);
      return IK::MFLO;
    // MTLO
    case 0b010100:
      ASSERT(fields.sa() == 0);
      return IK::DSLLV;
    // RESERVED
    case 0b010110:
      ASSERT(fields.sa() == 0);
      return IK::DSRLV;
    case 0b010111:
      ASSERT(fields.sa() == 0);
      return IK::DSRAV;
    case 0b011000:
      ASSERT(fields.sa() == 0);
      return IK::MULT3;
    case 0b011001:
      ASSERT(fields.sa() == 0);
      return IK::MULTU3;
    case 0b011010:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rd() == 0);
      return IK::DIV;
    case 0b011011:
      ASSERT(fields.sa() == 0);
      ASSERT(fields.rd() == 0);
      return IK::DIVU;
    // 4x UNSUPPORTED
    // ADD
    case 0b100001:
      ASSERT(fields.sa() == 0);
      return IK::ADDU;
    // SUB
    case 0b100011:
      ASSERT(fields.sa() == 0);
      return IK::SUBU;
    case 0b100100:
      ASSERT(fields.sa() == 0);
      return IK::AND;
    case 0b100101:
      ASSERT(fields.sa() == 0);
      return IK::OR;
    case 0b100110:
      ASSERT(fields.sa() == 0);
      return IK::XOR;
    case 0b100111:
      ASSERT(fields.sa() == 0);
      return IK::NOR;
    // MFSA
    // MTSA
    case 0b101010:
      ASSERT(fields.sa() == 0);
      return IK::SLT;
    case 0b101011:
      ASSERT(fields.sa() == 0);
      return IK::SLTU;
    // DADD
    case 0b101101:
      return IK::DADDU;
    // DSUB
    case 0b101111:
      return IK::DSUBU;
    // TGE
    // TGEU
    // TLT
    // TLTU
    // TEQ
    // RESERVED
    // TNE
    // RESERVED
    case 0b111000:
      ASSERT(fields.rs() == 0);
      return IK::DSLL;
    // RESERVED
    case 0b111010:
      ASSERT(fields.rs() == 0);
      return IK::DSRL;
    case 0b111011:
      ASSERT(fields.rs() == 0);
      return IK::DSRA;
    case 0b111100:
      ASSERT(fields.rs() == 0);
      return IK::DSLL32;
    // RESERVED
    case 0b111110:
      ASSERT(fields.rs() == 0);
      return IK::DSRL32;
    case 0b111111:
      ASSERT(fields.rs() == 0);
      return IK::DSRA32;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

static InstructionKind decode_cache(OpcodeFields fields) {
  typedef InstructionKind IK;
  // there's only one cache instruction used (DXWBIN), so we just use a CACHE DXWBIN instruction
  // to avoid having to implement the full cache instruction decoding.
  switch (fields.rt()) {
    case 0b10100:
      return IK::CACHE_DXWBIN;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
  }
}

/*!
 * Top level opcode decode
 */
static InstructionKind decode_opcode(uint32_t code) {
  OpcodeFields fields(code);
  typedef InstructionKind IK;
  switch (fields.op()) {
    case 0b000000:
      return decode_special(fields);
    case 0b000001:
      return decode_regimm(fields);
    // J      010
    // JAL    011
    case 0b000100:
      return IK::BEQ;
    case 0b000101:
      return IK::BNE;
    case 0b000110:
      return IK::BLEZ;
    case 0b000111:
      return IK::BGTZ;
    // ADDI  1000
    case 0b001001:
      return IK::ADDIU;
    case 0b001010:
      return IK::SLTI;
    case 0b001011:
      return IK::SLTIU;
    case 0b001100:
      return IK::ANDI;
    case 0b001101:
      return IK::ORI;
    case 0b001110:
      return IK::XORI;
    case 0b001111:
      ASSERT(fields.rs() == 0);
      return IK::LUI;
    case 0b010000:
      return decode_cop0(fields);
    case 0b010001:
      return decode_cop1(fields);
    case 0b010010:
      return decode_cop2(fields);
    //     010011:
    //  reserved
    case 0b010100:
      return IK::BEQL;
    case 0b010101:
      return IK::BNEL;
    //     010110
    //  blezl
    case 0b010111:
      ASSERT(fields.rt() == 0);
      return IK::BGTZL;
    //   0b011000:
    //  daddi
    case 0b011001:
      return IK::DADDIU;
    case 0b011010:
      return IK::LDL;
    case 0b011011:
      return IK::LDR;
    case 0b011100:
      return decode_mmi(fields);
    //   0b011101:
    // reserved
    case 0b011110:
      return IK::LQ;
    case 0b011111:
      return IK::SQ;
    case 0b100000:
      return IK::LB;
    case 0b100001:
      return IK::LH;
    case 0b100010:
      return IK::LWL;
    case 0b100011:
      return IK::LW;
    case 0b100100:
      return IK::LBU;
    case 0b100101:
      return IK::LHU;
    case 0b100110:
      return IK::LWR;
    case 0b100111:
      return IK::LWU;
    case 0b101000:
      return IK::SB;
    case 0b101001:
      return IK::SH;
    case 0b101011:
      return IK::SW;
    // SDL
    // SDR
    // SWR
    case 0b101111:
      return decode_cache(fields);

    // unsupported
    case 0b110001:
      return IK::LWC1;
    // unsupported
    case 0b110011:
      return IK::PREF;
    // unsupported
    // unsupported
    case 0b110110:
      return IK::LQC2;
    case 0b110111:
      return IK::LD;
    case 0b111001:
      return IK::SWC1;
    case 0b111110:
      return IK::SQC2;
    case 0b111111:
      return IK::SD;
    default:
      ASSERT(false);
      return IK::UNKNOWN;
      break;
  }
}

/*!
 * Top level decode function.
 */
Instruction decode_instruction(LinkedWord& word, LinkedObjectFile& file, int seg_id, int word_id) {
  // determine the opcode, and get info for it
  Instruction i;
  auto op = decode_opcode(word.data);
  auto& info = gOpcodeInfo[(int)op];
  if (!info.defined) {
    return i;
  }
  i.kind = op;
  OpcodeFields fields(word.data);

  // loop through decoding steps to extract a value
  for (int j = 0; j < info.step_count; j++) {
    auto& step = info.steps[j];
    int32_t value;
    switch (step.field) {
      case FieldType::RS:
        value = fields.rs();
        break;
      case FieldType::RT:
        value = fields.rt();
        break;
      case FieldType::RD:
        value = fields.rd();
        break;
      case FieldType::FT:
        value = fields.ft();
        break;
      case FieldType::FS:
        value = fields.fs();
        break;
      case FieldType::FD:
        value = fields.fd();
        break;
      case FieldType::SIMM16:
        value = fields.simm16();
        break;
      case FieldType::ZIMM16:
        value = fields.zimm16();
        break;
      case FieldType::SA:
        value = fields.sa();
        break;
      case FieldType::SYSCALL:
        value = fields.syscall();
        break;
      case FieldType::PCR:
        value = fields.pcreg();
        break;
      case FieldType::DEST:
        value = fields.dest();
        break;
      case FieldType::BC:
        value = fields.data & 0b11;
        break;
      case FieldType::IMM5:
        value = fields.imm5();
        break;
      case FieldType::IL:
        value = fields.data & 1;
        break;
      case FieldType::IMM15:
        value = fields.imm15();
        break;
      case FieldType::ZERO:
        value = 0;
        break;
      case FieldType::FT_F:
        value = fields.ft_f();
        break;
      case FieldType::FS_F:
        value = fields.fs_f();
        break;
      default:
        ASSERT(false);
    }

    // use the value, to possibly add an atom
    InstructionAtom atom;
    switch (step.decode) {
      case DecodeType::GPR:
        atom.set_reg(Register(Reg::GPR, value));
        break;
      case DecodeType::FPR:
        atom.set_reg(Register(Reg::FPR, value));
        break;
      case DecodeType::COP0:
        atom.set_reg(Register(Reg::COP0, value));
        break;
      case DecodeType::PCR:
        atom.set_reg(Register(Reg::SPECIAL, Reg::PCR0 + value));
        break;
      case DecodeType::IMM:
        atom.set_imm(value);
        break;
      case DecodeType::VF:
        atom.set_reg(Register(Reg::VF, value));
        break;
      case DecodeType::VI:
        atom.set_reg(Register(Reg::VI, value));
        break;
      case DecodeType::VU_ACC:
        atom.set_vu_acc();
        break;
      case DecodeType::VU_Q:
        atom.set_vu_q();
        break;
      case DecodeType::VCALLMS_TARGET:
        atom.set_imm(value);
        break;
      case DecodeType::BRANCH_TARGET:
        atom.set_label(file.get_label_id_for(seg_id, (word_id + value + 1) * 4));
        break;
      case DecodeType::VF_F:
        atom.set_vf_field(value);
        break;

        // NOTE - these change a property of the instruction and don't add an atom.
      case DecodeType::IL:
        i.il = value;
        continue;
      case DecodeType::DEST:
        i.cop2_dest = value;
        continue;
      case DecodeType::BC:
        i.cop2_bc = value;
        continue;

      default:
        ASSERT(false);
    }

    if (step.is_src) {
      i.add_src(atom);
    } else {
      i.add_dst(atom);
    }
  }

  if (word.kind() == LinkedWord::SYM_OFFSET) {
    bool fixed = false;
    for (int j = 0; j < i.n_src; j++) {
      if (i.src[j].kind == InstructionAtom::IMM) {
        fixed = true;
        i.src[j].set_sym(word.symbol_name());
      }
    }
    ASSERT(fixed);
  }

  if (word.kind() == LinkedWord::SYM_VAL_OFFSET) {
    bool fixed = false;
    for (int j = 0; j < i.n_src; j++) {
      if (i.src[j].kind == InstructionAtom::IMM) {
        fixed = true;
        i.src[j].set_sym_val_ptr(word.symbol_name());
      }
    }
    ASSERT(fixed);
  }

  if (word.kind() == LinkedWord::HI_PTR) {
    ASSERT(i.kind == InstructionKind::LUI);
    bool fixed = false;
    for (int j = 0; j < i.n_src; j++) {
      if (i.src[j].kind == InstructionAtom::IMM) {
        fixed = true;
        i.src[j].set_label(word.label_id());
      }
    }
    ASSERT(fixed);
  }

  if (word.kind() == LinkedWord::LO_PTR) {
    ASSERT(i.kind == InstructionKind::ORI);
    bool fixed = false;
    for (int j = 0; j < i.n_src; j++) {
      if (i.src[j].kind == InstructionAtom::IMM) {
        fixed = true;
        i.src[j].set_label(word.label_id());
      }
    }
    ASSERT(fixed);
  }

  return i;
}
}  // namespace decompiler
