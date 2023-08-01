#pragma once
#include <unordered_set>

#include "common/common_types.h"
#include "common/goos/Object.h"

#include "decompiler/Disasm/Register.h"
#include "decompiler/Function/Warnings.h"
#include "decompiler/util/TP_Type.h"

#include "third-party/fmt/core.h"

namespace decompiler {
enum class AccessMode : u8 {
  READ,  // represents value of the variable/register at the beginning of the instruction
  WRITE  // represents value of the variable/register at the end of the instruction
};

/*!
 * A register plus an integer ID.
 */
struct RegId {
  RegId() = default;
  RegId(Register _reg, int _id) : reg(_reg), id(_id) {}
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

  std::string print() const { return fmt::format("{}-{}", reg.to_charp(), id); }
};

class Env;
/*!
 * A "RegisterAccess" represents a register at a given instruction index.
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
class RegisterAccess {
 public:
  RegisterAccess() = default;
  RegisterAccess(AccessMode mode, Register reg, int atomic_idx, bool allow_all = false);

  enum class Print {
    AS_REG,               // print as a PS2 register name
    FULL,                 // print as a register name, plus an index, plus read or write
    AS_VARIABLE,          // print local variable name, error if impossible
    AS_VARIABLE_NO_CAST,  // same as above, but no cast
    AUTOMATIC,            // print as variable, but if that's not possible print as reg.
  };

  goos::Object to_form(const Env& env, Print mode = Print::AUTOMATIC) const;
  std::string to_string(const Env& env, Print mode = Print::AUTOMATIC) const;

  bool operator==(const RegisterAccess& other) const;
  bool operator!=(const RegisterAccess& other) const;

  const Register& reg() const { return m_reg; }
  AccessMode mode() const { return m_mode; }
  int idx() const { return m_atomic_idx; }

  struct hash {
    auto operator()(const RegisterAccess& x) const {
      return (Register::hash()(x.m_reg) << 2) ^ (int(x.m_mode) << 1) ^ x.m_atomic_idx;
    }
  };

 private:
  AccessMode m_mode = AccessMode::READ;  // do we represent a read or a write?
  Register m_reg;                        // the EE register
  int m_atomic_idx = -1;                 // the index in the function's list of AtomicOps
};

using RegAccessSet = std::unordered_set<RegisterAccess, RegisterAccess::hash>;

template <typename T>
using RegAccessMap = std::unordered_map<RegisterAccess, T, RegisterAccess::hash>;

enum class FixedOperatorKind {
  GPR_TO_FPR,
  DIVISION,
  ADDITION,
  ADDITION_PTR,
  ADDITION_IN_PLACE,
  ADDITION_PTR_IN_PLACE,
  SUBTRACTION,
  SUBTRACTION_PTR,
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
  LOGAND_IN_PLACE,
  LOGIOR,
  LOGIOR_IN_PLACE,
  LOGXOR,
  LOGXOR_IN_PLACE,
  LOGNOR,
  LOGNOT,
  LOGCLEAR,
  LOGCLEAR_IN_PLACE,
  LOGTEST,
  LOGTESTA,
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
  METHOD_OF_TYPE,
  NULLP,
  PAIRP,
  NONE,
  PCPYLD,
  SYMBOL_TO_STRING,
  ADDRESS_OF,
  ASM_SLLV_R0,
  ASM_MADDS,
  VECTOR_PLUS,
  VECTOR_MINUS,
  VECTOR_CROSS,
  VECTOR_FLOAT_PRODUCT,
  L32_NOT_FALSE_CBOOL,
  VECTOR_3_DOT,
  PROCESS_TO_PPOINTER,
  PPOINTER_TO_HANDLE,
  PROCESS_TO_HANDLE,
  PPOINTER_TO_PROCESS,
  VECTOR_4_DOT,
  VECTOR_LENGTH,
  VECTOR_PLUS_FLOAT_TIMES,
  SEND_EVENT,
  CPAD_PRESSED_P,
  CPAD_HOLD_P,
  FOCUS_TEST,
  INVALID
};

// for the use of a variable
struct AccessRecord {
  int op_id = -1;
  int block_id = -1;
  AccessMode mode = AccessMode::WRITE;
  bool disabled = false;
};

struct UseDefInfo {
  std::vector<AccessRecord> uses;
  std::vector<AccessRecord> defs;

  void disable_use(int op_id, const Register& this_reg) {
    for (auto& x : uses) {
      if (x.op_id == op_id) {
        if (x.disabled) {
          throw std::runtime_error("Invalid disable use twice");
        }
        x.disabled = true;
        return;
      }
    }

    throw std::runtime_error(
        fmt::format("Invalid disable use on register {} at op {}. The decompiler expects something "
                    "to use the value stored in this register, but nothing does.  This could be "
                    "caused by a missing return type or function argument.",
                    this_reg.to_charp(), op_id));
  }

  void disable_def(int op_id, DecompWarnings& warnings) {
    for (auto& x : defs) {
      if (x.op_id == op_id) {
        if (x.disabled) {
          warnings.warning(
              "disable def twice: {}. This may happen when a cond (no else) is nested inside of "
              "another conditional, but it should be rare.",
              x.op_id);
        }
        x.disabled = true;
        return;
      }
    }

    throw std::runtime_error("Invalid disable def");
  }

  int use_count() const {
    int count = 0;
    for (auto& x : uses) {
      if (!x.disabled) {
        count++;
      }
    }
    return count;
  }

  int def_count() const {
    int count = 0;
    for (auto& x : defs) {
      if (!x.disabled) {
        count++;
      }
    }
    return count;
  }

  std::unordered_set<int> ssa_vars;
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
  // program var to info
  std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash> read_vars,
      write_vars;

  // access to program var.
  std::unordered_map<Register, std::vector<int>, Register::hash> read_opid_to_varid,
      write_opid_to_varid;

  std::unordered_set<int> eliminated_move_op_ids;

  std::unordered_map<RegId, UseDefInfo, RegId::hash> use_def_info;

  void disable_use(const RegisterAccess& access) {
    ASSERT(access.mode() == AccessMode::READ);
    auto var_id = read_opid_to_varid.at(access.reg()).at(access.idx());
    use_def_info.at(RegId(access.reg(), var_id)).disable_use(access.idx(), access.reg());
  }

  void disable_def(const RegisterAccess& access, DecompWarnings& warnings) {
    ASSERT(access.mode() == AccessMode::WRITE);
    auto var_id = write_opid_to_varid.at(access.reg()).at(access.idx());
    use_def_info.at(RegId(access.reg(), var_id)).disable_def(access.idx(), warnings);
  }

  const VarInfo& lookup(Register reg, int op_id, AccessMode mode) const {
    if (mode == AccessMode::READ) {
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
  // it this a move that can be compacted? For now, this is always false, but we may want to
  // allow more advanced compaction later.
  bool is_compactable = false;
};

}  // namespace decompiler
