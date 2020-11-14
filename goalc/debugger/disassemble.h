#pragma once

#include <string>
#include <vector>
#include "common/common_types.h"
#include "goalc/emitter/Instruction.h"

struct InstructionInfo {
  emitter::Instruction instruction;  //! the actual x86 instruction
  enum Kind { PROLOGUE, IR, EPILOGUE } kind;
  int ir_idx = -1;
  int offset = -1;

  InstructionInfo(const emitter::Instruction& _instruction, Kind _kind)
      : instruction(_instruction), kind(_kind) {}

  InstructionInfo(const emitter::Instruction& _instruction, Kind _kind, int _ir_idx)
      : instruction(_instruction), kind(_kind), ir_idx(_ir_idx) {}
};

std::string disassemble_x86(u8* data, int len, u64 base_addr);
std::string disassemble_x86(u8* data, int len, u64 base_addr, u64 highlight_addr);

std::string disassemble_x86_function(u8* data,
                                     int len,
                                     u64 base_addr,
                                     u64 highlight_addr,
                                     const std::vector<InstructionInfo>& x86_instructions,
                                     const std::vector<std::string>& irs,
                                     bool* had_failure);