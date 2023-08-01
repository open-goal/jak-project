#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/common_types.h"

namespace decompiler {

enum class VuInstrK {
  // upper
  // ABS,
  ADD,
  ADDi,
  ADDq,
  ADDbc,
  ADDA,
  //  ADDAi,
  //  ADDAq,
  ADDAbc,
  SUB,
  //  SUBi,  // 10
  //  SUBq,
  SUBbc,
  //  SUBA,
  //  SUBAi,
  //  SUBAq,
  //  SUBAbc,
  MUL,
  MULi,
  MULq,
  MULbc,  // 20
  MULA,
  //  MULAi,
  MULAq,
  MULAbc,
  MADD,
  //  MADDi,
  MADDq,
  MADDbc,
  MADDA,
  //  MADDAi,  // 30
  //  MADDAq,
  MADDAbc,
  //  MSUB,
  //  MSUBi,
  //  MSUBq,
  MSUBbc,
  //  MSUBA,
  //  MSUBAi,
  //  MSUBAq,
  MSUBAbc,  // 40
  MAX,
  MAXi,
  MAXbc,
  MINI,
  MINIi,
  MINIbc,
  OPMULA,
  OPMSUB,
  NOP,    // 49
  FTOI0,  // 50
  FTOI4,
  FTOI12,
  //  FTOI15,
  ITOF0,
  //  ITOF4,
  ITOF12,
  ITOF15,
  CLIP,

  // lower
  DIV,
  SQRT,  // 60
  RSQRT,
  IADD,
  IADDI,
  IADDIU,
  IAND,
  IOR,
  ISUB,
  ISUBIU,
  MOVE,
  MFIR,  // 70
  MTIR,
  MR32,
  LQ,
  LQD,
  LQI,
  SQ,
  SQD,
  SQI,
  ILW,
  ISW,  // 80
  ILWR,
  ISWR,
  //  LOI,
  //  RINIT,
  //  RGET,
  //  RNEXT,
  //  RXOR,
  WAITQ,
  FSAND,
  //  FSEQ, // 90
  //  FSOR,
  //  FSSET,
  FMAND,
  //  FMEQ,
  //  FMOR,
  FCAND,
  //  FCEQ,
  FCOR,
  FCSET,
  FCGET,  // 100
  IBEQ,
  //  IBEZ,
  IBGEZ,
  IBGTZ,
  IBLEZ,
  IBLTZ,
  IBNE,
  B,
  BAL,
  JR,  // 110
  JALR,
  MFP,
  WAITP,
  ESADD,
  //  ERSADD,
  ELENG,
  ERLENG,
  //  EATANxy,
  //  EATANxz,
  ESUM,
  //  ERCPR,
  //  ESQRT,
  //  ERSQRT,
  //  ESIN,
  //  IATAN,
  //  EEXP,
  XGKICK,
  XTOP,
  //  XITOP,
  LOWER_NOP,  // really iadd vi0 vi0 vi0

  FP_CONSTANT,

  INVALID
};

/*!
 * Represents an operand in a VU instruction.
 */
class VuInstructionAtom {
 public:
  enum class Kind {
    VF,
    VI,
    Q,
    ACC,
    IMM,
    LOAD_STORE_IMM,
    I,
    P,
    R,
    LABEL,
    INVALID,
  };

  static VuInstructionAtom make_vf(int idx);
  static VuInstructionAtom make_vi(int idx);
  static VuInstructionAtom make_label(int idx);
  static VuInstructionAtom make_q();
  static VuInstructionAtom make_acc();
  static VuInstructionAtom make_imm(u64 value);
  static VuInstructionAtom make_load_store_imm(s64 value);
  static VuInstructionAtom make_i();
  static VuInstructionAtom make_p();
  static VuInstructionAtom make_r();

  bool is_int_reg(int idx) const;

  std::string to_string(const std::vector<std::string>& labels) const;
  Kind kind() const { return m_kind; }
  s64 value() const { return m_value; }

 private:
  VuInstructionAtom(Kind kind, s64 idx = -1) : m_kind(kind), m_value(idx) {}
  Kind m_kind = Kind::INVALID;
  s64 m_value = -1;
};

struct VuInstruction {
  VuInstrK kind = VuInstrK::INVALID;
  std::optional<VuInstructionAtom> dst;
  std::vector<VuInstructionAtom> src;
  std::optional<int> bc;
  std::optional<int> mask;
  std::optional<int> iemdt;
  std::optional<int> first_src_field, second_src_field;
  float fp;

  bool i_bit() const { return iemdt && (*iemdt & 0b1000000); }

  static VuInstruction make_fp_constant(u32 value);
};

struct VuInstructionPair {
  VuInstruction upper, lower;
};

}  // namespace decompiler
