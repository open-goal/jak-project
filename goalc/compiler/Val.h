#pragma once

/*!
 * @file Val.h
 * The GOAL Value. A value represents a place (where the value is stored) and a type.
 */

#include <optional>
#include <stdexcept>
#include <string>
#include <utility>

#include "Lambda.h"
#include "StaticObject.h"

#include "common/type_system/TypeSystem.h"

#include "goalc/compiler/ConstantValue.h"
#include "goalc/regalloc/IRegister.h"

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
  virtual RegVal* to_reg(const goos::Object& /*form*/, Env* fe) {
    (void)fe;
    throw std::runtime_error("to_reg called on invalid Val: " + print());
  }
  virtual RegVal* to_gpr(const goos::Object& form, Env* fe);
  virtual RegVal* to_fpr(const goos::Object& form, Env* fe);
  virtual RegVal* to_xmm128(const goos::Object& form, Env* fe);

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
  void change_class(RegClass new_class) { m_ireg.reg_class = new_class; }
  std::string print() const override { return m_ireg.to_string(); };
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  RegVal* to_gpr(const goos::Object& form, Env* fe) override;
  RegVal* to_fpr(const goos::Object& form, Env* fe) override;
  RegVal* to_xmm128(const goos::Object& form, Env* fe) override;
  void set_rlet_constraint(emitter::Register reg);
  const std::optional<emitter::Register>& rlet_constraint() const;
  void force_on_stack() { m_on_stack = true; }
  bool forced_on_stack() const { return m_on_stack; }

 protected:
  IRegister m_ireg;
  std::optional<emitter::Register> m_rlet_constraint = std::nullopt;
  bool m_on_stack = false;  // this should be spilled onto the stack always
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
  RegVal* to_reg(const goos::Object& form, Env* fe) override;

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
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
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
  explicit LambdaVal(TypeSpec ts, bool immediate) : Val(std::move(ts)), is_immediate(immediate) {}
  std::string print() const override { return "lambda-" + lambda.debug_name; }
  FunctionEnv* func = nullptr;
  Lambda lambda;
  bool is_immediate = false;
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
};

class InlinedLambdaVal : public Val {
 public:
  explicit InlinedLambdaVal(TypeSpec ts, InlineableFunction _lv) : Val(std::move(ts)), lv(_lv) {}
  std::string print() const override { return "inline-lambda-" + lv.lambda.debug_name; }
  InlineableFunction lv;
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
};

class StaticVal : public Val {
 public:
  StaticVal(StaticObject* _obj, TypeSpec _ts) : Val(std::move(_ts)), obj(_obj) {}
  StaticObject* obj = nullptr;
  std::string print() const override { return "[" + obj->print() + "]"; }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
};

struct MemLoadInfo {
  MemLoadInfo() = default;
  explicit MemLoadInfo(const DerefInfo& di) {
    ASSERT(di.can_deref);
    ASSERT(di.mem_deref);
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

  RegVal* to_reg(const goos::Object& form, Env* fe) override;

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
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  Val* base = nullptr;
  s64 offset = 0;
};

class MemoryOffsetVal : public Val {
 public:
  MemoryOffsetVal(TypeSpec ts, Val* _base, Val* _offset)
      : Val(std::move(ts)), base(_base), offset(_offset) {}
  std::string print() const override { return "(" + base->print() + " + " + offset->print() + ")"; }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  Val* base = nullptr;
  Val* offset = nullptr;
};

class MemoryDerefVal : public Val {
 public:
  MemoryDerefVal(TypeSpec ts, Val* _base, MemLoadInfo _info)
      : Val(std::move(ts)), base(_base), info(_info) {}
  std::string print() const override { return "[" + base->print() + "]"; }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  RegVal* to_fpr(const goos::Object& form, Env* fe) override;
  Val* base = nullptr;
  MemLoadInfo info;
};

class PairEntryVal : public Val {
 public:
  PairEntryVal(TypeSpec ts, Val* _base, bool _is_car)
      : Val(std::move(ts)), base(_base), is_car(_is_car) {}
  std::string print() const override;
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  Val* base = nullptr;
  bool is_car = false;
};

class AliasVal : public Val {
 public:
  AliasVal(TypeSpec ts, Val* _base) : Val(std::move(ts)), base(_base) {}
  std::string print() const override { return "alias-of-" + base->print(); }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  RegVal* to_xmm128(const goos::Object& form, Env* fe) override;
  Val* base = nullptr;
};

class IntegerConstantVal : public Val {
 public:
  IntegerConstantVal(TypeSpec ts, const void* data, int size)
      : Val(std::move(ts)), m_value(data, size) {
    ASSERT(size == 8 || size == 16);
  }

  std::string print() const override { return std::string("integer-constant-") + m_value.print(); }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;
  RegVal* to_xmm128(const goos::Object& form, Env* fe) override;
  const ConstantValue& value() const { return m_value; }

 protected:
  ConstantValue m_value;
};

class FloatConstantVal : public Val {
 public:
  FloatConstantVal(TypeSpec ts, StaticFloat* value) : Val(std::move(ts)), m_value(value) {}
  std::string print() const override { return "float-constant-" + m_value->print(); }
  RegVal* to_reg(const goos::Object& form, Env* fe) override;

 protected:
  StaticFloat* m_value = nullptr;
};

class BitFieldVal : public Val {
 public:
  BitFieldVal(TypeSpec ts, Val* parent, int offset, int size, bool sign_extend, bool use128)
      : Val(std::move(ts)),
        m_parent(parent),
        m_offset(offset),
        m_size(size),
        m_sign_extend(sign_extend),
        m_use_128(use128) {
    m_is_settable = parent->settable();
  }
  std::string print() const override;
  RegVal* to_reg(const goos::Object& form, Env* env) override;
  int offset() const { return m_offset; }
  int size() const { return m_size; }
  bool sext() const { return m_sign_extend; }
  bool use_128_bit() const { return m_use_128; }
  Val* parent() { return m_parent; }

 protected:
  Val* m_parent = nullptr;
  int m_offset = -1;
  int m_size = -1;
  bool m_sign_extend = false;
  bool m_use_128 = false;
};

template <typename T>
struct ValOrConstant {
  explicit ValOrConstant(const T& c) : constant(c), val(nullptr) {}
  explicit ValOrConstant(Val* v) : val(v) {}

  T constant;
  Val* val = nullptr;

  bool is_constant() const { return val == nullptr; }
  bool is_variable() const { return val != nullptr; }
};

using ValOrConstInt = ValOrConstant<s64>;
using ValOrConstFloat = ValOrConstant<float>;