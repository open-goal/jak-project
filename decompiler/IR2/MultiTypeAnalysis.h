#pragma once

#include <vector>
#include <array>
#include "common/util/CopyOnWrite.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"
#include "common/type_system/TypeSystem.h"

namespace decompiler {

class InstrTypeState;
class DecompWarnings;
struct PossibleType;

struct DerefHint {
  struct Token {
    enum class Kind {
      INTEGER, FIELD, VAR, INVALID
    } kind = Kind::INVALID;
    int integer = 0;
    std::string name;
    bool matches(const FieldReverseLookupOutput::Token& other)const ;
  };
  std::vector<Token> tokens;

  bool matches(const FieldReverseLookupOutput& value)const ;
};

/*!
 * Represents a reference to a type decision made on a previous instruction.
 */
struct TypeDecisionParent {
  InstrTypeState* instruction = nullptr;
  Register reg;
  int type_index = -1;

  const PossibleType& get() const;
};

/*!
 * Represents a possibility for the type in a register.
 * Can be "invalid", meaning it is eliminated from the possible types due to a constraint.
 * Use is_valid to check that it hasn't been eliminated.
 */
struct PossibleType {
  TP_Type type;                                        // the actual type.
  std::optional<FieldReverseLookupOutput> deref_path;  // the field accessed to get here
  double deref_score = 0.;
  TypeDecisionParent parent;                           // the decision we made to allow this.
  void eliminate() { m_valid_cache = false; }
  bool is_valid() const;  // true, unless we were eliminated.

 private:
  mutable bool m_valid_cache = true;
};

/*!
 * The set of all possible types in a register.
 */
struct RegisterTypeState {
  std::optional<int> single_type_cache;
  std::vector<PossibleType> possible_types;
  void reduce_to_single_type(DecompWarnings* warnings, int op_idx, const DerefHint* hint);
  const PossibleType& get_single_type_decision() const;
  const TP_Type& get_single_tp_type() const;
  void try_elimination(const TypeSpec& desired_types, const TypeSystem& ts);
};

class InstrTypeState {
 public:
  explicit InstrTypeState(const CopyOnWrite<RegisterTypeState>& default_value) {
    m_regs.fill(default_value);
  }

  const RegisterTypeState& get_const(const Register& reg) const {
    assert(reg.reg_id() < Reg::MAX_VAR_REG_ID);
    return *m_regs[reg.reg_id()];
  }

  CopyOnWrite<RegisterTypeState>& get(const Register& reg) {
    assert(reg.reg_id() < Reg::MAX_VAR_REG_ID);
    return m_regs[reg.reg_id()];
  }

 private:
  std::array<CopyOnWrite<RegisterTypeState>, Reg::MAX_VAR_REG_ID> m_regs;
};
}  // namespace decompiler