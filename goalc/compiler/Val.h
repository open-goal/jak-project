#pragma once

/*!
 * @file Val.h
 * The GOAL Value. A value represents a place (where the value is stored) and a type.
 */

#ifndef JAK_VAL_H
#define JAK_VAL_H

#include <utility>
#include <string>
#include <stdexcept>
#include <optional>
#include "common/type_system/TypeSystem.h"
#include "goalc/regalloc/IRegister.h"
#include "Lambda.h"
#include "StaticObject.h"

class RegVal;
class Env;
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
  virtual RegVal* to_reg(Env* fe) {
    (void)fe;
    throw std::runtime_error("to_reg called on invalid Val: " + print());
  }
  virtual RegVal* to_gpr(Env* fe);
  virtual RegVal* to_fpr(Env* fe);
  virtual RegVal* to_xmm128(Env* fe);

  const TypeSpec& type() const { return m_ts; }
  void set_type(TypeSpec ts) { m_ts = std::move(ts); }
  bool settable() const { return m_is_settable; }
  void mark_as_settable() { m_is_settable = true; }
  virtual ~Val() = default;

 protected:
  TypeSpec m_ts;
  bool m_is_settable = false;
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
  RegVal(IRegister ireg, const TypeSpec& ts) : Val(coerce_to_reg_type(ts)), m_ireg(ireg) {}
  bool is_register() const override { return true; }
  IRegister ireg() const override { return m_ireg; }
  std::string print() const override { return m_ireg.to_string(); };
  RegVal* to_reg(Env* fe) override;
  RegVal* to_gpr(Env* fe) override;
  RegVal* to_fpr(Env* fe) override;
  RegVal* to_xmm128(Env* fe) override;
  void set_rlet_constraint(emitter::Register reg);
  const std::optional<emitter::Register>& rlet_constraint() const;

 protected:
  IRegister m_ireg;
  std::optional<emitter::Register> m_rlet_constraint = std::nullopt;
};

/*!
 * A Val representing a symbol. This is confusing but it's not actually the value of the symbol,
 * but instead the symbol itself.
 */
class SymbolVal : public Val {
 public:
  SymbolVal(std::string name, TypeSpec ts) : Val(std::move(ts)), m_name(std::move(name)) {
    // this is for define, which looks at the SymbolVal and not the SymbolValueVal.
    mark_as_settable();
  }
  const std::string& name() const { return m_name; }
  std::string print() const override { return "<" + m_name + ">"; }
  RegVal* to_reg(Env* fe) override;

 protected:
  std::string m_name;
};

class SymbolValueVal : public Val {
 public:
  SymbolValueVal(const SymbolVal* sym, TypeSpec ts, bool sext)
      : Val(std::move(ts)), m_sym(sym), m_sext(sext) {
    // this is for set, which looks at the Symbol's Value.
    mark_as_settable();
  }
  const std::string& name() const { return m_sym->name(); }
  std::string print() const override { return "[<" + name() + ">]"; }
  RegVal* to_reg(Env* fe) override;
  const SymbolVal* sym() const { return m_sym; }

 protected:
  const SymbolVal* m_sym = nullptr;
  bool m_sext = false;
};

/*!
 * A Val representing a GOAL lambda. It can be a "real" x86-64 function, in which case the
 * FunctionEnv is set. Otherwise, just contains a Lambda.
 */
class LambdaVal : public Val {
 public:
  explicit LambdaVal(TypeSpec ts) : Val(std::move(ts)) {}
  std::string print() const override { return "lambda-" + lambda.debug_name; }
  FunctionEnv* func = nullptr;
  Lambda lambda;
  RegVal* to_reg(Env* fe) override;
};

class InlinedLambdaVal : public Val {
 public:
  explicit InlinedLambdaVal(TypeSpec ts, LambdaVal* _lv) : Val(std::move(ts)), lv(_lv) {}
  std::string print() const override { return "inline-lambda-" + lv->lambda.debug_name; }
  LambdaVal* lv = nullptr;
  RegVal* to_reg(Env* fe) override;
};

class StaticVal : public Val {
 public:
  StaticVal(StaticObject* _obj, TypeSpec _ts) : Val(std::move(_ts)), obj(_obj) {}
  StaticObject* obj = nullptr;
  std::string print() const override { return "[" + obj->print() + "]"; }
  RegVal* to_reg(Env* fe) override;
};

struct MemLoadInfo {
  MemLoadInfo() = default;
  explicit MemLoadInfo(const DerefInfo& di) {
    assert(di.can_deref);
    assert(di.mem_deref);
    sign_extend = di.sign_extend;
    size = di.load_size;
    reg = di.reg;
  }

  RegClass reg = RegClass::INVALID;
  bool sign_extend = false;
  int size = -1;
};

/*!
 * A spot on the stack.
 */
class StackVarAddrVal : public Val {
 public:
  StackVarAddrVal(TypeSpec ts, int slot, int slot_count)
      : Val(std::move(ts)), m_slot(slot), m_slot_count(slot_count) {}
  int slot() const { return m_slot; }
  int slot_count() const { return m_slot_count; }
  std::string print() const override { return "stack-" + std::to_string(m_slot); }

  RegVal* to_reg(Env* fe) override;

 private:
  int m_slot, m_slot_count;
};

class MemoryOffsetConstantVal : public Val {
 public:
  MemoryOffsetConstantVal(TypeSpec ts, Val* _base, s64 _offset)
      : Val(std::move(ts)), base(_base), offset(_offset) {
    auto base_as_offset = dynamic_cast<MemoryOffsetConstantVal*>(base);
    if (base_as_offset) {
      offset += base_as_offset->offset;
      base = base_as_offset->base;
    }
  }
  std::string print() const override {
    return "(" + base->print() + " + " + std::to_string(offset) + ")";
  }
  RegVal* to_reg(Env* fe) override;
  Val* base = nullptr;
  s64 offset = 0;
};

class MemoryOffsetVal : public Val {
 public:
  MemoryOffsetVal(TypeSpec ts, Val* _base, Val* _offset)
      : Val(std::move(ts)), base(_base), offset(_offset) {}
  std::string print() const override { return "(" + base->print() + " + " + offset->print() + ")"; }
  RegVal* to_reg(Env* fe) override;
  Val* base = nullptr;
  Val* offset = nullptr;
};

class MemoryDerefVal : public Val {
 public:
  MemoryDerefVal(TypeSpec ts, Val* _base, MemLoadInfo _info)
      : Val(std::move(ts)), base(_base), info(_info) {}
  std::string print() const override { return "[" + base->print() + "]"; }
  RegVal* to_reg(Env* fe) override;
  RegVal* to_fpr(Env* fe) override;
  Val* base = nullptr;
  MemLoadInfo info;
};

class PairEntryVal : public Val {
 public:
  PairEntryVal(TypeSpec ts, Val* _base, bool _is_car)
      : Val(std::move(ts)), base(_base), is_car(_is_car) {}
  std::string print() const override;
  RegVal* to_reg(Env* fe) override;
  Val* base = nullptr;
  bool is_car = false;
};

class AliasVal : public Val {
 public:
  AliasVal(TypeSpec ts, Val* _base) : Val(std::move(ts)), base(_base) {}
  std::string print() const override { return "alias-of-" + base->print(); }
  RegVal* to_reg(Env* fe) override;
  Val* base = nullptr;
};

class IntegerConstantVal : public Val {
 public:
  IntegerConstantVal(TypeSpec ts, s64 value) : Val(std::move(ts)), m_value(value) {}
  std::string print() const override { return "integer-constant-" + std::to_string(m_value); }
  RegVal* to_reg(Env* fe) override;
  s64 value() const { return m_value; }

 protected:
  s64 m_value = -1;
};

class FloatConstantVal : public Val {
 public:
  FloatConstantVal(TypeSpec ts, StaticFloat* value) : Val(std::move(ts)), m_value(value) {}
  std::string print() const override { return "float-constant-" + m_value->print(); }
  RegVal* to_reg(Env* fe) override;

 protected:
  StaticFloat* m_value = nullptr;
};

class BitFieldVal : public Val {
 public:
  BitFieldVal(TypeSpec ts, Val* parent, int offset, int size, bool sign_extend)
      : Val(std::move(ts)),
        m_parent(parent),
        m_offset(offset),
        m_size(size),
        m_sign_extend(sign_extend) {
    m_is_settable = parent->settable();
  }
  std::string print() const override;
  RegVal* to_reg(Env* env) override;
  int offset() const { return m_offset; }
  int size() const { return m_size; }
  bool sext() const { return m_sign_extend; }
  Val* parent() { return m_parent; }

 protected:
  Val* m_parent = nullptr;
  int m_offset = -1;
  int m_size = -1;
  bool m_sign_extend = false;
};

#endif  // JAK_VAL_H
