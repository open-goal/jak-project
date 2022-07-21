#pragma once

#include <string>
#include <vector>

namespace LSPData {
struct MIPSInstruction {
  std::string type;
  std::string mnemonic;
  std::string description;
};

extern const std::vector<MIPSInstruction> MIPS_INSTRUCTION_LIST;
}  // namespace LSPData
