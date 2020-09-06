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
#include "Lambda.h"

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
  virtual const RegVal* to_reg(FunctionEnv* fe) const {
    (void)fe;
    throw std::runtime_error("to_reg called on invalid Val: " + print());
  }
  virtual const RegVal* to_gpr(FunctionEnv* fe) const;
  virtual const RegVal* to_xmm(FunctionEnv* fe) const;

  const TypeSpec& type() const { return m_ts; }
  void set_type(TypeSpec ts) { m_ts = std::move(ts); }

 protected:
  TypeSpec m_ts;
};

/*!
 * Special None Val used for the value of anything returning (none).
 */
class None : public Val {
 public:
  explicit None(TypeSpec _ts) : Val(std::move(_ts)) {}
  explicit None(const TypeSystem& _ts) : Val(_ts.make_typespec("none")) {}
  std::string print() const override { return "none"; }
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
  const RegVal* to_reg(FunctionEnv* fe) const override;
  const RegVal* to_gpr(FunctionEnv* fe) const override;
  const RegVal* to_xmm(FunctionEnv* fe) const override;

 protected:
  IRegister m_ireg;
};

/*!
 * A Val representing a symbol. This is confusing but it's not actually the value of the symbol,
 * but instead the symbol itself.
 */
class SymbolVal : public Val {
 public:
  SymbolVal(std::string name, TypeSpec ts) : Val(std::move(ts)), m_name(std::move(name)) {}
  const std::string& name() { return m_name; }
  std::string print() const override { return "<" + m_name + ">"; }

 protected:
  std::string m_name;
};

/*!
 * A Val representing a GOAL lambda. It can be a "real" x86-64 function, in which case the
 * FunctionEnv is set. Otherwise, just contains a Lambda.
 */
class LambdaVal : public Val {
 public:
  LambdaVal(TypeSpec ts, Lambda lam) : Val(ts), m_lam(lam) {}
  std::string print() const override { return "lambda-" + m_lam.debug_name; }

 protected:
  Lambda m_lam;
  FunctionEnv* fe = nullptr;
};

// Static
// MemOffConstant
// MemOffVar
// MemDeref
// PairEntry
// Alias

class IntegerConstantVal : public Val {
 public:
  IntegerConstantVal(TypeSpec ts, s64 value) : Val(ts), m_value(value) {}
  std::string print() const override { return "integer-constant-" + std::to_string(m_value); }
  const RegVal* to_reg(FunctionEnv* fe) const override;

 protected:
  s64 m_value = -1;
};
// IntegerConstant
// FloatConstant
// Bitfield

#endif  // JAK_VAL_H
