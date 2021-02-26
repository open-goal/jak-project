#pragma once

#include <string>
#include <optional>
#include <cassert>
#include <utility>
#include <map>
#include "common/goos/Object.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/Disasm/Instruction.h"
#include "decompiler/IR2/IR2_common.h"
#include "Env.h"

namespace decompiler {

struct OpenGOALAsm {
  // TODO - to support I register operations, I think I'll need to add instruction kinds
  // and add a new look-ahead to check for a preceeding `LOI`, load the constant into a `vector` and perform the normal operation
  enum class AdditionalVURegisters { ACC, Q_SRC, Q_DST, R, I, VF0 };
  enum class InstructionModifiers { BROADCAST, DEST_MASK, FTF, FSF, OFFSET };

  struct Function {
    std::string funcTemplate = "";
    std::vector<AdditionalVURegisters> additionalRegs = {};
    std::vector<InstructionModifiers> modifiers = {};
  };

  OpenGOALAsm(InstructionKind kind);

  bool valid = true;
  Function func;
};
extern const std::map<InstructionKind, OpenGOALAsm::Function> MIPS_ASM_TO_OPEN_GOAL_FUNCS;
}  // namespace decompiler
