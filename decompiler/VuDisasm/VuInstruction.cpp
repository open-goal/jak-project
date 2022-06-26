#include "VuInstruction.h"

#include "common/util/Assert.h"

#include "third-party/fmt/core.h"

namespace decompiler {
VuInstructionAtom VuInstructionAtom::make_vf(int idx) {
  ASSERT(idx >= 0 && idx <= 31);
  return {Kind::VF, idx};
}

VuInstructionAtom VuInstructionAtom::make_vi(int idx) {
  ASSERT(idx >= 0 && idx <= 15);
  return {Kind::VI, idx};
}

VuInstructionAtom VuInstructionAtom::make_label(int idx) {
  return {Kind::LABEL, idx};
}

VuInstructionAtom VuInstructionAtom::make_q() {
  return {Kind::Q};
}

VuInstructionAtom VuInstructionAtom::make_acc() {
  return {Kind::ACC};
}

VuInstructionAtom VuInstructionAtom::make_imm(u64 value) {
  return {Kind::IMM, (s64)value};
}

VuInstructionAtom VuInstructionAtom::make_load_store_imm(s64 value) {
  return {Kind::LOAD_STORE_IMM, value};
}

VuInstructionAtom VuInstructionAtom::make_i() {
  return {Kind::I};
}

VuInstructionAtom VuInstructionAtom::make_p() {
  return {Kind::P};
}

VuInstructionAtom VuInstructionAtom::make_r() {
  return {Kind::R};
}

bool VuInstructionAtom::is_int_reg(int idx) const {
  return m_kind == Kind::VI && m_value == idx;
}

std::string VuInstructionAtom::to_string(const std::vector<std::string>& labels) const {
  switch (m_kind) {
    case Kind::VF:
      return fmt::format("vf{:02d}", m_value);
    case Kind::Q:
      return "Q";
    case Kind::ACC:
      return "ACC";
    case Kind::IMM:
      if (m_value < 0) {
        return fmt::format("-0x{:x}", -m_value);
      } else {
        return fmt::format("0x{:x}", m_value);
      }

    case Kind::I:
      return "I";
    case Kind::P:
      return "P";
    case Kind::R:
      return "R";
    case Kind::LABEL:
      if (m_value < (int)labels.size()) {
        return labels.at(m_value);
      } else {
        return "BAD-LABEL";
      }
    case Kind::LOAD_STORE_IMM:
      return fmt::format("{}", m_value);
    case Kind::VI:
      return fmt::format("vi{:02d}", m_value);
    case Kind::INVALID:
    default:
      ASSERT(false);
  }
}

VuInstruction VuInstruction::make_fp_constant(u32 value) {
  VuInstruction result;
  memcpy(&result.fp, &value, sizeof(float));
  result.kind = VuInstrK::FP_CONSTANT;
  return result;
}
}  // namespace decompiler
