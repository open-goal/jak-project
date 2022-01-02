#pragma once

#include <string>
#include <optional>
#include <utility>
#include <map>
#include "common/goos/Object.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/Disasm/Instruction.h"
#include "decompiler/IR2/IR2_common.h"
#include "Env.h"
#include "AtomicOp.h"
#include "common/util/assert.h"

namespace decompiler {

struct OpenGOALAsm {
  enum class InstructionModifiers {
    BROADCAST,
    DEST_MASK,
    FTF,
    FSF,
    OFFSET,
    SWAP_FIRST_TWO_SOURCE_ARGS,
    ACC_THIRD_SRC_ARG
  };

  struct Function {
    std::string funcTemplate = "";
    std::vector<InstructionModifiers> modifiers = {};

    bool allows_modifier(InstructionModifiers);
  };

  OpenGOALAsm(Instruction _instr);

  OpenGOALAsm(Instruction _instr,
              std::optional<RegisterAccess> _dst,
              const std::vector<std::optional<RegisterAccess>>& _src);

  bool valid = true;
  bool todo = false;
  Instruction instr;
  std::optional<RegisterAccess> m_dst;
  std::vector<std::optional<RegisterAccess>> m_src;
  OpenGOALAsm::Function func;

  std::string full_function_name();
  std::vector<goos::Object> get_args(const std::vector<DecompilerLabel>& labels, const Env& env);
};
extern const std::map<InstructionKind, OpenGOALAsm::Function> MIPS_ASM_TO_OPEN_GOAL_FUNCS;
}  // namespace decompiler
