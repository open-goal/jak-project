/*!
 * The InstructionParser converts a string like "daddu a0, s7, r0" into an Instruction.
 * It is used to generate test sequences of instructions for decompiler algorithms.
 */

#pragma once

#include <string>
#include <unordered_map>

#include "DecompilerLabel.h"
#include "Instruction.h"

namespace decompiler {
struct ParsedProgram {
  std::vector<DecompilerLabel> labels;
  std::vector<Instruction> instructions;
  std::string print();
};

class InstructionParser {
 public:
  InstructionParser();
  Instruction parse_single_instruction(std::string str, const std::vector<DecompilerLabel>& labels);
  ParsedProgram parse_program(const std::string& str,
                              const std::vector<std::string>& predefined_labels = {});

 private:
  std::unordered_map<std::string, int> m_opcode_name_lookup;
  std::unordered_map<std::string, int> m_opcode_name_broadcast_lookup;
};
}  // namespace decompiler