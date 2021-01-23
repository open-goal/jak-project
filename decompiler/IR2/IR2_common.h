#pragma once
#include <unordered_set>
#include "common/common_types.h"
#include "decompiler/Disasm/Register.h"

namespace decompiler {
enum class VariableMode : u8 {
  READ,  // represents value of the variable at the beginning of the instruction
  WRITE  // represents value of the variable at the end of the instruction
};

/*!
 * A register plus an integer ID.
 */
struct RegId {
  Register reg;
  int id = -1;
  struct hash {
    auto operator()(const RegId& x) const {
      return Register::hash()(x.reg) ^ std::hash<int>()(x.id);
    }
  };

  bool operator==(const RegId& other) const { return reg == other.reg && id == other.id; }
  bool operator!=(const RegId& other) const { return !((*this) == other); }
  bool operator<(const RegId& other) const {
    if (reg == other.reg) {
      return id < other.id;
    } else {
      return reg < other.reg;
    }
  }
};

class Env;
/*!
 * A "Variable" represents a register at a given instruction index.
 * The register can either be a GOAL local variable or a GOAL register used in inline assembly.
 * Because OpenGOAL's registers don't one-to-one map to GOAL registers, GOAL "inline assembly
 * registers" will become OpenGOAL variables, and are treated similarly to variables in
 * decompilation.
 *
 * In the earlier parts of decompilation, this just behaves like a register in all cases.
 * But in later parts registers can be mapped to real local variables with types. A variable can
 * look itself up in an environment to determine what "local variable" it is.
 *
 * Note: a variable is _not_ allowed to be R0, AT, S7, K0, K1, FP, or RA by default, as these
 * can never hold normal GOAL locals.  Inline assembly may use these, but you must set the allow_all
 * flag to true in the constructor of Variable to indicate this is what you really want.
 *
 * Note: access to the process pointer (s6) is handled as a variable. As a result, you may always
 * use s6 as a variable.
 */
class Variable {
 public:
  Variable() = default;
  Variable(VariableMode mode, Register reg, int atomic_idx, bool allow_all = false);

  enum class Print {
    AS_REG,       // print as a PS2 register name
    FULL,         // print as a register name, plus an index, plus read or write
    AS_VARIABLE,  // print local variable name, error if impossible
    AUTOMATIC,    // print as variable, but if that's not possible print as reg.
  };

  std::string to_string(const Env* env, Print mode = Print::AUTOMATIC) const;

  bool operator==(const Variable& other) const;
  bool operator!=(const Variable& other) const;

  const Register& reg() const { return m_reg; }
  VariableMode mode() const { return m_mode; }
  int idx() const { return m_atomic_idx; }

  struct hash {
    auto operator()(const Variable& x) const {
      return (Register::hash()(x.m_reg) << 2) ^ (int(x.m_mode) << 1) ^ x.m_atomic_idx;
    }
  };

 private:
  VariableMode m_mode = VariableMode::READ;  // do we represent a read or a write?
  Register m_reg;                            // the EE register
  int m_atomic_idx = -1;                     // the index in the function's list of AtomicOps
};

using VariableSet = std::unordered_set<Variable, Variable::hash>;

enum class FixedOperatorKind { GPR_TO_FPR, DIVISION, ADDITION, INVALID };
}  // namespace decompiler
