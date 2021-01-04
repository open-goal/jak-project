#include <cassert>
#include "third-party/fmt/core.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "AtomicOp.h"

/////////////////////////////
// VARIABLE
/////////////////////////////

Variable::Variable(Mode mode, Register reg, int atomic_idx, bool allow_all)
    : m_mode(mode), m_reg(reg), m_atomic_idx(atomic_idx) {
  // make sure we're using a valid GPR.
  if (reg.get_kind() == Reg::GPR && !allow_all) {
    assert(Reg::allowed_local_gprs[reg.get_gpr()] || reg.get_gpr() == Reg::S6);
  }
}

std::string Variable::to_string(const Env* env, Print mode) const {
  switch (mode) {
    case Print::AS_REG:
      return m_reg.to_string();
    case Print::FULL:
      return fmt::format("{}-{:03d}-{}", m_reg.to_charp(), m_atomic_idx,
                         m_mode == Mode::READ ? 'r' : 'w');
    case Print::AS_VARIABLE:
      return env->get_variable_name(m_reg, m_atomic_idx);
    case Print::AUTOMATIC:
      if (env->has_local_vars()) {
        return env->get_variable_name(m_reg, m_atomic_idx);
      } else {
        return m_reg.to_string();
      }
    default:
      assert(false);
  }
}

bool Variable::operator==(const Variable& other) const {
  return m_mode == other.m_mode && m_reg == other.m_reg && m_atomic_idx && other.m_atomic_idx;
}

bool Variable::operator!=(const Variable& other) const {
  return !((*this) == other);
}

/////////////////////////////
// AtomicOp
/////////////////////////////
AtomicOp::AtomicOp(int my_idx) : m_my_idx(my_idx) {}

bool AtomicOp::operator!=(const AtomicOp& other) const {
  return !((*this) == other);
}

/////////////////////////////
// SimpleAtom
/////////////////////////////

SimpleAtom SimpleAtom::make_var(const Variable& var) {
  SimpleAtom result;
  result.m_kind = Kind::VARIABLE;
  result.m_variable = var;
  return result;
}

SimpleAtom SimpleAtom::make_sym_ptr(const std::string& name) {
  SimpleAtom result;
  result.m_kind = Kind::SYMBOL_PTR;
  result.m_string = name;
  return result;
}

SimpleAtom SimpleAtom::make_sym_val(const std::string& name) {
  SimpleAtom result;
  result.m_kind = Kind::SYMBOL_VAL;
  result.m_string = name;
  return result;
}

SimpleAtom SimpleAtom::make_empty_list() {
  SimpleAtom result;
  result.m_kind = Kind::EMPTY_LIST;
  return result;
}

SimpleAtom SimpleAtom::make_int_constant(s64 value) {
  SimpleAtom result;
  result.m_int = value;
  return result;
}

goos::Object SimpleAtom::to_form(const LinkedObjectFile* file, const Env* env) const {
  switch (m_kind) {
    case Kind::VARIABLE:
      return pretty_print::to_symbol(m_variable.to_string(env));
    case Kind::INTEGER_CONSTANT:
      return pretty_print::to_symbol(std::to_string(m_int));
    case Kind::SYMBOL_PTR:
      return pretty_print::to_symbol(fmt::format("'{}", m_string));
    case Kind::SYMBOL_VAL:
      return pretty_print::to_symbol(m_string);
    case Kind::STATIC_ADDRESS:
      return pretty_print::to_symbol(file->get_label_name(m_int));
    default:
      assert(false);
      return {};
  }
}

bool SimpleAtom::operator==(const SimpleAtom& other) const {
  if (other.m_kind != m_kind) {
    return false;
  }

  switch (m_kind) {
    case Kind::VARIABLE:
      return m_variable == other.m_variable;
    case Kind::INTEGER_CONSTANT:
      return m_int == other.m_int;
    case Kind::SYMBOL_VAL:
    case Kind::SYMBOL_PTR:
      return m_string == other.m_string;
    case Kind::EMPTY_LIST:
      return true;
    case Kind::STATIC_ADDRESS:
      return m_int == other.m_int;
    default:
      assert(false);
      return false;
  }
}

/////////////////////////////
// SimpleExpression
/////////////////////////////

namespace {
std::string get_simple_expression_op_name(SimpleExpression::Kind kind) {
  switch (kind) {
    case SimpleExpression::Kind::DIV_S:
      return "/.s";
    case SimpleExpression::Kind::MUL_S:
      return "*.s";
    case SimpleExpression::Kind::ADD_S:
      return "+.s";
    case SimpleExpression::Kind::SUB_S:
      return "-.s";
    case SimpleExpression::Kind::MIN_S:
      return "min.s";
    case SimpleExpression::Kind::MAX_S:
      return "max.s";
    case SimpleExpression::Kind::FLOAT_TO_INT:
      return "f2i";
    case SimpleExpression::Kind::INT_TO_FLOAT:
      return "i2f";
    case SimpleExpression::Kind::ABS_S:
      return "abs.s";
    case SimpleExpression::Kind::NEG_S:
      return "neg.s";
    case SimpleExpression::Kind::SQRT_S:
      return "sqrt.s";
    case SimpleExpression::Kind::ADD:
      return "+";
    case SimpleExpression::Kind::SUB:
      return "-";
    case SimpleExpression::Kind::MUL_SIGNED:
      return "*.si";
    case SimpleExpression::Kind::DIV_SIGNED:
      return "/.si";
    case SimpleExpression::Kind::MOD_SIGNED:
      return "%.si";
    case SimpleExpression::Kind::DIV_UNSIGNED:
      return "/.ui";
    case SimpleExpression::Kind::MOD_UNSIGNED:
      return "%.ui";
    case SimpleExpression::Kind::OR:
      return "logior";
    case SimpleExpression::Kind::AND:
      return "logand";
    case SimpleExpression::Kind::NOR:
      return "lognor";
    case SimpleExpression::Kind::XOR:
      return "logxor";
    case SimpleExpression::Kind::LEFT_SHIFT:
      return "shl";
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
      return "sra";
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
      return "srl";
    case SimpleExpression::Kind::MUL_UNSIGNED:
      return "*.ui";
    case SimpleExpression::Kind::NOT:
      return "lognot";
    case SimpleExpression::Kind::NEG:
      return "-";
    default:
      assert(false);
  }
}

int get_simple_expression_arg_count(SimpleExpression::Kind kind) {
  switch (kind) {
    case SimpleExpression::Kind::IDENTITY:
      return 1;
    case SimpleExpression::Kind::DIV_S:
    case SimpleExpression::Kind::MUL_S:
    case SimpleExpression::Kind::ADD_S:
    case SimpleExpression::Kind::SUB_S:
    case SimpleExpression::Kind::MIN_S:
    case SimpleExpression::Kind::MAX_S:
      return 2;
    case SimpleExpression::Kind::FLOAT_TO_INT:
    case SimpleExpression::Kind::INT_TO_FLOAT:
    case SimpleExpression::Kind::ABS_S:
    case SimpleExpression::Kind::NEG_S:
    case SimpleExpression::Kind::SQRT_S:
      return 1;
    case SimpleExpression::Kind::ADD:
    case SimpleExpression::Kind::SUB:
    case SimpleExpression::Kind::MUL_SIGNED:
    case SimpleExpression::Kind::DIV_SIGNED:
    case SimpleExpression::Kind::MOD_SIGNED:
    case SimpleExpression::Kind::DIV_UNSIGNED:
    case SimpleExpression::Kind::MOD_UNSIGNED:
    case SimpleExpression::Kind::OR:
    case SimpleExpression::Kind::AND:
    case SimpleExpression::Kind::NOR:
    case SimpleExpression::Kind::XOR:
    case SimpleExpression::Kind::LEFT_SHIFT:
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
    case SimpleExpression::Kind::MUL_UNSIGNED:
      return 2;
    case SimpleExpression::Kind::NOT:
    case SimpleExpression::Kind::NEG:
      return 1;
    default:
      assert(false);
  }
}
}  // namespace

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0) {
  m_args[0] = arg0;
  m_kind = kind;
  n_args = 1;
  assert(get_simple_expression_arg_count(kind) == 1);
}

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1) {
  m_args[0] = arg0;
  m_args[1] = arg1;
  m_kind = kind;
  n_args = 2;
  assert(get_simple_expression_arg_count(kind) == 2);
}

goos::Object SimpleExpression::to_form(const LinkedObjectFile* file, const Env* env) const {
  std::vector<goos::Object> forms;
  if (m_kind == Kind::IDENTITY) {
    assert(args() == 1);
    return get_arg(0).to_form(file, env);
  } else {
    forms.push_back(pretty_print::to_symbol(get_simple_expression_op_name(m_kind)));
    for (int i = 0; i < args(); i++) {
      forms.push_back(get_arg(i).to_form(file, env));
    }
    return pretty_print::build_list(forms);
  }
}

bool SimpleExpression::operator==(const SimpleExpression& other) const {
  if (m_kind != other.m_kind) {
    return false;
  }
  assert(args() == other.args());
  for (int i = 0; i < args(); i++) {
    if (other.get_arg(i) != get_arg(i)) {
      return false;
    }
  }
  return true;
}

/////////////////////////////
// SetVarOp
/////////////////////////////

goos::Object SetVarOp::to_form(const LinkedObjectFile* file, const Env* env) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"),
                                  pretty_print::to_symbol(m_dst.to_string(env)),
                                  m_src.to_form(file, env));
}
