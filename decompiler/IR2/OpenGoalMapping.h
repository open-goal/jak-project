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
#include "AtomicOp.h"

namespace decompiler {

struct OpenGOALAsm {
  // TODO - to support I register operations, I think I'll need to add instruction kinds
  // and add a new look-ahead to check for a preceeding `LOI`, load the constant into a `vector` and
  // perform the normal operation
  enum class AdditionalVURegisters { ACC, Q_SRC, Q_DST, R, I, VF0 };
  enum class InstructionModifiers { BROADCAST, DEST_MASK, FTF, FSF, OFFSET };

  struct Function {
    std::string funcTemplate = "";
    std::vector<AdditionalVURegisters> additionalRegs = {};
    std::vector<InstructionModifiers> modifiers = {};

    bool allows_modifier(InstructionModifiers);
  };

  OpenGOALAsm(Instruction _instr);

  OpenGOALAsm(Instruction _instr,
              std::optional<Variable> _dst,
              std::vector<std::optional<Variable>> _src);

  bool valid = true;
  std::optional<Variable> m_dst;
  std::vector<std::optional<Variable>> m_src;
  Instruction instr;
  OpenGOALAsm::Function func;

  std::string full_function_name();
  std::vector<goos::Object> get_args(const std::vector<DecompilerLabel>& labels, const Env& env);
};
extern const std::map<InstructionKind, OpenGOALAsm::Function> MIPS_ASM_TO_OPEN_GOAL_FUNCS;
}  // namespace decompiler
