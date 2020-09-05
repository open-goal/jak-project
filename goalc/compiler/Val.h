/*!
 * @file Val.h
 * The GOAL Value. A value represents a place (where the value is stored) and a type.
 */

#ifndef JAK_VAL_H
#define JAK_VAL_H

#include <utility>
#include <string>
#include <stdexcept>
#include "third-party/fmt/core.h"
#include "common/type_system/TypeSystem.h"
#include "goalc/regalloc/IRegister.h"

class RegVal;
class FunctionEnv;

/*!
 * Parent class for every Val.
 */
class Val {
 public:
  explicit Val(TypeSpec ts) : m_ts(std::move(ts)) {}

  virtual bool is_register() const { return false; }

  virtual IRegister ireg() const {
    throw std::runtime_error("get_ireg called on invalid Val: " + print());
  }

  virtual std::string print() const = 0;
  virtual RegVal* to_reg(FunctionEnv* fe) const = 0;
  virtual RegVal* to_gpr(FunctionEnv* fe) const;
  virtual RegVal* to_xmm(FunctionEnv* fe) const;

  const TypeSpec& type() const { return m_ts; }

 protected:
  TypeSpec m_ts;
};

/*!
 * Special None Val used for the value of anything returning (none).
 */
class None : public Val {
  explicit None(TypeSpec _ts) : Val(std::move(_ts)) {}
  explicit None(const TypeSystem& _ts) : Val(_ts.make_typespec("none")) {}
  std::string print() const override { return "none"; }
  RegVal* to_reg(FunctionEnv* fe) const override;
};

/*!
 * A Val stored in a register.
 */
class RegVal : public Val {
 public:
  RegVal(IRegister ireg, TypeSpec ts) : Val(std::move(ts)), m_ireg(ireg) {}
  bool is_register() const override { return true; }
  IRegister ireg() const override { return m_ireg; }
  std::string print() const override { return m_ireg.to_string(); };
  RegVal* to_reg(FunctionEnv* fe) const override;
  RegVal* to_gpr(FunctionEnv* fe) const override;
  RegVal* to_xmm(FunctionEnv* fe) const override;

 protected:
  IRegister m_ireg;
};

// Symbol
// Lambda
// Static
// MemOffConstant
// MemOffVar
// MemDeref
// PairEntry
// Alias
// IntegerConstant
// FloatConstant
// Bitfield

#endif  // JAK_VAL_H
