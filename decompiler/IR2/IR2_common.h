#pragma once
#include <unordered_set>
#include "common/common_types.h"
#include "common/goos/Object.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"
#include "third-party/fmt/core.h"

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

  goos::Object to_form(const Env& env, Print mode = Print::AUTOMATIC) const;

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

enum class FixedOperatorKind {
  GPR_TO_FPR,
  DIVISION,
  ADDITION,
  SUBTRACTION,
  MULTIPLICATION,
  SQRT,
  ARITH_SHIFT,
  MOD,
  ABS,
  MIN,
  MAX,
  FABS,
  FMIN,
  FMAX,
  LOGAND,
  LOGIOR,
  LOGXOR,
  LOGNOR,
  LOGNOT,
  SHL,
  SHR,
  SAR,
  CAR,
  CDR,
  NEW,
  OBJECT_NEW,
  TYPE_NEW,
  LT,
  GT,
  LEQ,
  GEQ,
  EQ,
  NEQ,
  CONS,
  METHOD_OF_OBJECT,
  INVALID
};

struct VariableNames {
  struct VarInfo {
    VarInfo() = default;
    std::string name() const { return fmt::format("{}-{}", reg_id.reg.to_charp(), reg_id.id); }
    TP_Type type;
    RegId reg_id;
    bool initialized = false;
  };

  // todo - this is kind of gross.
  std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash> read_vars,
      write_vars;
  std::unordered_map<Register, std::vector<int>, Register::hash> read_opid_to_varid,
      write_opid_to_varid;

  std::unordered_set<int> eliminated_move_op_ids;

  const VarInfo& lookup(Register reg, int op_id, VariableMode mode) const {
    if (mode == VariableMode::READ) {
      return read_vars.at(reg).at(read_opid_to_varid.at(reg).at(op_id));
    } else {
      return write_vars.at(reg).at(write_opid_to_varid.at(reg).at(op_id));
    }
  }
};

struct SetVarInfo {
  // is this a compiler-inserted move at the beginning of a function
  // that should be eliminated?
  bool is_eliminated_coloring_move = false;
  // is this a (set! var expr) which consumes the reg for expr,
  // and var is written and unused?
  bool is_dead_set = false;
  // is this a (set! var #f) where the value of #f isn't used?
  bool is_dead_false = false;
};

}  // namespace decompiler
