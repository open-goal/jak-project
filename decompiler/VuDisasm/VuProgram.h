#pragma once

#include <vector>

#include "decompiler/VuDisasm/VuInstruction.h"

namespace decompiler {
class VuProgram {
 public:
  VuProgram() = default;
  void add_instruction(const VuInstruction& upper, const VuInstruction& lower) {
    VuInstructionPair p;
    p.upper = upper;
    p.lower = lower;
    m_instructions.push_back(p);
  }
  const std::vector<VuInstructionPair>& instructions() const { return m_instructions; }

 private:
  std::vector<VuInstructionPair> m_instructions;
};
}  // namespace decompiler
