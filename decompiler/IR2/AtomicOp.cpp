#include <cassert>
#include <utility>
#include <stdexcept>
#include "third-party/fmt/core.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "AtomicOp.h"

namespace decompiler {
/////////////////////////////
// VARIABLE
/////////////////////////////

Variable::Variable(Mode mode, Register reg, int atomic_idx, bool allow_all)
    : m_mode(mode), m_reg(reg), m_atomic_idx(atomic_idx) {
  // make sure we're using a valid GPR.
  if (reg.get_kind() == Reg::GPR && !allow_all) {
    if (!(Reg::allowed_local_gprs[reg.get_gpr()] || reg.get_gpr() == Reg::S6)) {
      throw std::runtime_error("Variable could not be constructed from register " +
                               reg.to_string());
    }
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

std::string AtomicOp::to_string(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  return pretty_print::to_string(to_form(labels, env));
}
bool AtomicOp::operator!=(const AtomicOp& other) const {
  return !((*this) == other);
}

/*!
 * Add GOAL temp registers to the clobber list.
 */
void AtomicOp::clobber_temps() {
  for (auto clobber : {Reg::V1, Reg::AT, Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::T0, Reg::T1,
                       Reg::T2, Reg::T3, Reg::T4, Reg::T5, Reg::T6, Reg::T7, Reg::T8, Reg::T9}) {
    m_clobber_regs.push_back(Register(Reg::GPR, clobber));
  }
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
  result.m_kind = Kind::INTEGER_CONSTANT;
  result.m_int = value;
  return result;
}

SimpleAtom SimpleAtom::make_static_address(int static_label_id) {
  SimpleAtom result;
  result.m_kind = Kind::STATIC_ADDRESS;
  result.m_int = static_label_id;
  return result;
}

goos::Object SimpleAtom::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  switch (m_kind) {
    case Kind::VARIABLE:
      return pretty_print::to_symbol(m_variable.to_string(env));
    case Kind::INTEGER_CONSTANT:
      return pretty_print::to_symbol(std::to_string(m_int));
    case Kind::SYMBOL_PTR:
      return pretty_print::to_symbol(fmt::format("'{}", m_string));
    case Kind::SYMBOL_VAL:
      return pretty_print::to_symbol(m_string);
    case Kind::EMPTY_LIST:
      return pretty_print::to_symbol("'()");
    case Kind::STATIC_ADDRESS:
      return pretty_print::to_symbol(labels.at(m_int).name);
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

void SimpleAtom::get_regs(std::vector<Register>* out) const {
  if (is_var()) {
    out->push_back(var().reg());
  }
}

SimpleExpression SimpleAtom::as_expr() const {
  return SimpleExpression(SimpleExpression::Kind::IDENTITY, *this);
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
      return "mod.si";
    case SimpleExpression::Kind::DIV_UNSIGNED:
      return "/.ui";
    case SimpleExpression::Kind::MOD_UNSIGNED:
      return "mod.ui";
    case SimpleExpression::Kind::OR:
      return "logior";
    case SimpleExpression::Kind::AND:
      return "logand";
    case SimpleExpression::Kind::NOR:
      return "lognor";
    case SimpleExpression::Kind::XOR:
      return "logxor";
    case SimpleExpression::Kind::LEFT_SHIFT:
      return "sll";
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
      return "sra";
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
      return "srl";
    case SimpleExpression::Kind::MUL_UNSIGNED:
      return "*.ui";
    case SimpleExpression::Kind::LOGNOT:
      return "lognot";
    case SimpleExpression::Kind::NEG:
      return "-";
    case SimpleExpression::Kind::GPR_TO_FPR:
      return "gpr->fpr";
    case SimpleExpression::Kind::FPR_TO_GPR:
      return "fpr->gpr";
    case SimpleExpression::Kind::MIN_SIGNED:
      return "min.si";
    case SimpleExpression::Kind::MIN_UNSIGNED:
      return "min.ui";
    case SimpleExpression::Kind::MAX_SIGNED:
      return "max.si";
    case SimpleExpression::Kind::MAX_UNSIGNED:
      return "max.ui";
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
    case SimpleExpression::Kind::LOGNOT:
    case SimpleExpression::Kind::NEG:
    case SimpleExpression::Kind::GPR_TO_FPR:
    case SimpleExpression::Kind::FPR_TO_GPR:
      return 1;
    case SimpleExpression::Kind::MIN_SIGNED:
    case SimpleExpression::Kind::MIN_UNSIGNED:
    case SimpleExpression::Kind::MAX_SIGNED:
    case SimpleExpression::Kind::MAX_UNSIGNED:
      return 2;
    default:
      assert(false);
  }
}
}  // namespace

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0) : n_args(1) {
  m_args[0] = arg0;
  m_kind = kind;
  assert(get_simple_expression_arg_count(kind) == 1);
}

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1)
    : n_args(2) {
  m_args[0] = arg0;
  m_args[1] = arg1;
  m_kind = kind;
  assert(get_simple_expression_arg_count(kind) == 2);
}

goos::Object SimpleExpression::to_form(const std::vector<DecompilerLabel>& labels,
                                       const Env* env) const {
  std::vector<goos::Object> forms;
  if (m_kind == Kind::IDENTITY) {
    // we are "identity" so just pass through the atom
    assert(args() == 1);
    return get_arg(0).to_form(labels, env);
  } else {
    forms.push_back(pretty_print::to_symbol(get_simple_expression_op_name(m_kind)));
    for (int i = 0; i < args(); i++) {
      forms.push_back(get_arg(i).to_form(labels, env));
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

void SimpleExpression::get_regs(std::vector<Register>* out) const {
  for (s8 i = 0; i < args(); i++) {
    get_arg(i).get_regs(out);
  }
}

/////////////////////////////
// SetVarOp
/////////////////////////////

goos::Object SetVarOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"),
                                  pretty_print::to_symbol(m_dst.to_string(env)),
                                  m_src.to_form(labels, env));
}

bool SetVarOp::operator==(const AtomicOp& other) const {
  if (typeid(SetVarOp) != typeid(other)) {
    return false;
  }
  auto po = dynamic_cast<const SetVarOp*>(&other);
  assert(po);
  return m_dst == po->m_dst && m_src == po->m_src;
}

bool SetVarOp::is_variable_set() const {
  return true;
}

bool SetVarOp::is_sequence_point() const {
  if (m_src.is_identity()) {
    auto& atom = m_src.get_arg(0);
    if (atom.is_var()) {
      if (atom.var().reg().get_kind() == m_dst.reg().get_kind()) {
        // if we're setting a register equal to another register of the same kind.
        // todo - this may also be a non-sequence point operation moving a float to a GPR?
        return false;
      }
    }
  }
  return true;
}

Variable SetVarOp::get_set_destination() const {
  return m_dst;
}

std::unique_ptr<Expr> SetVarOp::get_set_source_as_expr() const {
  throw std::runtime_error("get_set_source_as_expr NYI for SetVarOp");
}

std::unique_ptr<Expr> SetVarOp::get_as_expr() const {
  throw std::runtime_error("get_as_expr NYI for SetVarOp");
}

void SetVarOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_src.get_regs(&m_read_regs);
}

/////////////////////////////
// AsmOp
/////////////////////////////

AsmOp::AsmOp(Instruction instr, int my_idx) : AtomicOp(my_idx), m_instr(std::move(instr)) {
  assert(m_instr.n_dst <= 1);
  if (m_instr.n_dst == 1) {
    auto& dst = m_instr.get_dst(0);
    if (dst.is_reg()) {
      m_dst = Variable(Variable::Mode::WRITE, dst.get_reg(), my_idx, true);
    }
  }

  assert(m_instr.n_src <= 3);
  for (int i = 0; i < m_instr.n_src; i++) {
    auto& src = m_instr.get_src(i);
    if (src.is_reg()) {
      m_src[i] = Variable(Variable::Mode::READ, src.get_reg(), my_idx, true);
    }
  }
}

goos::Object AsmOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("." + m_instr.op_name_to_string()));
  assert(m_instr.n_dst <= 1);

  if (m_instr.n_dst == 1) {
    if (m_dst.has_value()) {
      // then print it as a variable
      forms.push_back(pretty_print::to_symbol(m_dst.value().to_string(env)));
    } else {
      // print the atom
      forms.push_back(pretty_print::to_symbol(m_instr.get_dst(0).to_string(labels)));
    }
  }

  assert(m_instr.n_src <= 3);
  for (int i = 0; i < m_instr.n_src; i++) {
    if (m_src[i].has_value()) {
      forms.push_back(pretty_print::to_symbol(m_src[i].value().to_string(env)));
    } else {
      forms.push_back(pretty_print::to_symbol(m_instr.get_src(i).to_string(labels)));
    }
  }

  return pretty_print::build_list(forms);
}

bool AsmOp::operator==(const AtomicOp& other) const {
  if (typeid(AsmOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const AsmOp*>(&other);
  assert(po);

  return (m_instr == po->m_instr) && (m_dst == po->m_dst) && (m_src[0] == po->m_src[0]) &&
         (m_src[1] == po->m_src[1]) && (m_src[2] == po->m_src[2]);
}

bool AsmOp::is_variable_set() const {
  return false;
}

bool AsmOp::is_sequence_point() const {
  return true;
}

Variable AsmOp::get_set_destination() const {
  throw std::runtime_error("AsmOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> AsmOp::get_set_source_as_expr() const {
  throw std::runtime_error("AsmOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> AsmOp::get_as_expr() const {
  throw std::runtime_error("AsmOp::get_as_expr is not implemented.");
}

void AsmOp::update_register_info() {
  if (m_dst.has_value()) {
    m_write_regs.push_back(m_dst->reg());
  }

  for (auto& src : m_src) {
    if (src.has_value()) {
      m_read_regs.push_back(src->reg());
    }
  }
}

/////////////////////////////
// Condition
/////////////////////////////

namespace {
std::string get_condition_kind_name(IR2_Condition::Kind kind) {
  switch (kind) {
    case IR2_Condition::Kind::NOT_EQUAL:
      return "!=";
    case IR2_Condition::Kind::EQUAL:
      return "=";
    case IR2_Condition::Kind::LESS_THAN_SIGNED:
      return "<.si";
    case IR2_Condition::Kind::LESS_THAN_UNSIGNED:
      return "<.ui";
    case IR2_Condition::Kind::GREATER_THAN_SIGNED:
      return ">.si";
    case IR2_Condition::Kind::GREATER_THAN_UNSIGNED:
      return ">.ui";
    case IR2_Condition::Kind::LEQ_SIGNED:
      return "<=.si";
    case IR2_Condition::Kind::GEQ_SIGNED:
      return ">=.si";
    case IR2_Condition::Kind::LEQ_UNSIGNED:
      return "<=.ui";
    case IR2_Condition::Kind::GEQ_UNSIGNED:
      return ">=.ui";
    case IR2_Condition::Kind::ZERO:
      return "zero?";
    case IR2_Condition::Kind::NONZERO:
      return "nonzero?";
    case IR2_Condition::Kind::FALSE:
      return "not";
    case IR2_Condition::Kind::TRUTHY:
      return "truthy";
    case IR2_Condition::Kind::ALWAYS:
      return "#t";
    case IR2_Condition::Kind::NEVER:
      return "#f";
    case IR2_Condition::Kind::FLOAT_EQUAL:
      return "=.s";
    case IR2_Condition::Kind::FLOAT_NOT_EQUAL:
      return "!=.s";
    case IR2_Condition::Kind::FLOAT_LESS_THAN:
      return "<.s";
    case IR2_Condition::Kind::FLOAT_GEQ:
      return ">=.s";
    case IR2_Condition::Kind::FLOAT_GREATER_THAN:
      return ">.s";
    case IR2_Condition::Kind::FLOAT_LEQ:
      return "<=.s";
    case IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED:
      return ">0.si";
    case IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED:
      return ">0.ui";
    case IR2_Condition::Kind::GEQ_ZERO_SIGNED:
      return ">=0.si";
    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED:
      return "<0.si";
    case IR2_Condition::Kind::LEQ_ZERO_SIGNED:
      return "<=0.si";
    case IR2_Condition::Kind::LEQ_ZERO_UNSIGNED:
      return "<=0.ui";
    case IR2_Condition::Kind::IS_PAIR:
      return "pair?";
    case IR2_Condition::Kind::IS_NOT_PAIR:
      return "not-pair?";
    case IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED:
      return "<0.ui";
    case IR2_Condition::Kind::GEQ_ZERO_UNSIGNED:
      return ">=0.ui";
    default:
      assert(false);
  }
}

int get_condition_num_args(IR2_Condition::Kind kind) {
  switch (kind) {
    case IR2_Condition::Kind::NOT_EQUAL:
    case IR2_Condition::Kind::EQUAL:
    case IR2_Condition::Kind::LESS_THAN_SIGNED:
    case IR2_Condition::Kind::LESS_THAN_UNSIGNED:
    case IR2_Condition::Kind::GREATER_THAN_SIGNED:
    case IR2_Condition::Kind::GREATER_THAN_UNSIGNED:
    case IR2_Condition::Kind::LEQ_SIGNED:
    case IR2_Condition::Kind::GEQ_SIGNED:
    case IR2_Condition::Kind::LEQ_UNSIGNED:
    case IR2_Condition::Kind::GEQ_UNSIGNED:
    case IR2_Condition::Kind::FLOAT_EQUAL:
    case IR2_Condition::Kind::FLOAT_NOT_EQUAL:
    case IR2_Condition::Kind::FLOAT_LESS_THAN:
    case IR2_Condition::Kind::FLOAT_GEQ:
    case IR2_Condition::Kind::FLOAT_GREATER_THAN:
    case IR2_Condition::Kind::FLOAT_LEQ:
      return 2;
    case IR2_Condition::Kind::ZERO:
    case IR2_Condition::Kind::NONZERO:
    case IR2_Condition::Kind::FALSE:
    case IR2_Condition::Kind::TRUTHY:
    case IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED:
    case IR2_Condition::Kind::GEQ_ZERO_SIGNED:
    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED:
    case IR2_Condition::Kind::LEQ_ZERO_SIGNED:
    case IR2_Condition::Kind::IS_PAIR:
    case IR2_Condition::Kind::IS_NOT_PAIR:
    case IR2_Condition::Kind::LEQ_ZERO_UNSIGNED:
    case IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED:
    case IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED:
    case IR2_Condition::Kind::GEQ_ZERO_UNSIGNED:
      return 1;
    case IR2_Condition::Kind::ALWAYS:
    case IR2_Condition::Kind::NEVER:
      return 0;
    default:
      assert(false);
  }
}

IR2_Condition::Kind get_condition_opposite(IR2_Condition::Kind kind) {
  switch (kind) {
    case IR2_Condition::Kind::NOT_EQUAL:
      return IR2_Condition::Kind::EQUAL;
    case IR2_Condition::Kind::EQUAL:
      return IR2_Condition::Kind::NOT_EQUAL;
    case IR2_Condition::Kind::LESS_THAN_SIGNED:
      return IR2_Condition::Kind::GEQ_SIGNED;
    case IR2_Condition::Kind::GREATER_THAN_SIGNED:
      return IR2_Condition::Kind::LEQ_SIGNED;
    case IR2_Condition::Kind::LEQ_SIGNED:
      return IR2_Condition::Kind::GREATER_THAN_SIGNED;
    case IR2_Condition::Kind::GEQ_SIGNED:
      return IR2_Condition::Kind::LESS_THAN_SIGNED;
    case IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED:
      return IR2_Condition::Kind::LEQ_ZERO_SIGNED;
    case IR2_Condition::Kind::LEQ_ZERO_SIGNED:
      return IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED;
    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED:
      return IR2_Condition::Kind::GEQ_ZERO_SIGNED;
    case IR2_Condition::Kind::GEQ_ZERO_SIGNED:
      return IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED;
    case IR2_Condition::Kind::LESS_THAN_UNSIGNED:
      return IR2_Condition::Kind::GEQ_UNSIGNED;
    case IR2_Condition::Kind::GREATER_THAN_UNSIGNED:
      return IR2_Condition::Kind::LEQ_UNSIGNED;
    case IR2_Condition::Kind::LEQ_UNSIGNED:
      return IR2_Condition::Kind::GREATER_THAN_UNSIGNED;
    case IR2_Condition::Kind::GEQ_UNSIGNED:
      return IR2_Condition::Kind::LESS_THAN_UNSIGNED;
    case IR2_Condition::Kind::ZERO:
      return IR2_Condition::Kind::NONZERO;
    case IR2_Condition::Kind::NONZERO:
      return IR2_Condition::Kind::ZERO;
    case IR2_Condition::Kind::FALSE:
      return IR2_Condition::Kind::TRUTHY;
    case IR2_Condition::Kind::TRUTHY:
      return IR2_Condition::Kind::FALSE;
    case IR2_Condition::Kind::ALWAYS:
      return IR2_Condition::Kind::NEVER;
    case IR2_Condition::Kind::NEVER:
      return IR2_Condition::Kind::ALWAYS;
    case IR2_Condition::Kind::FLOAT_EQUAL:
      return IR2_Condition::Kind::FLOAT_NOT_EQUAL;
    case IR2_Condition::Kind::FLOAT_NOT_EQUAL:
      return IR2_Condition::Kind::FLOAT_EQUAL;
    case IR2_Condition::Kind::FLOAT_LESS_THAN:
      return IR2_Condition::Kind::FLOAT_GEQ;
    case IR2_Condition::Kind::FLOAT_GEQ:
      return IR2_Condition::Kind::FLOAT_LESS_THAN;
    case IR2_Condition::Kind::FLOAT_GREATER_THAN:
      return IR2_Condition::Kind::FLOAT_LEQ;
    case IR2_Condition::Kind::FLOAT_LEQ:
      return IR2_Condition::Kind::FLOAT_GREATER_THAN;
    case IR2_Condition::Kind::IS_NOT_PAIR:
      return IR2_Condition::Kind::IS_PAIR;
    case IR2_Condition::Kind::IS_PAIR:
      return IR2_Condition::Kind::IS_NOT_PAIR;
    case IR2_Condition::Kind::LEQ_ZERO_UNSIGNED:
      return IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED;
    case IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED:
      return IR2_Condition::Kind::LEQ_ZERO_UNSIGNED;
    case IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED:
      return IR2_Condition::Kind::GEQ_ZERO_UNSIGNED;
    case IR2_Condition::Kind::GEQ_ZERO_UNSIGNED:
      return IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED;
    default:
      assert(false);
  }
}
}  // namespace

IR2_Condition::IR2_Condition(Kind kind) : m_kind(kind) {
  assert(get_condition_num_args(m_kind) == 0);
}

IR2_Condition::IR2_Condition(Kind kind, const SimpleAtom& src0) : m_kind(kind) {
  m_src[0] = src0;
  assert(get_condition_num_args(m_kind) == 1);
}

IR2_Condition::IR2_Condition(Kind kind, const SimpleAtom& src0, const SimpleAtom& src1)
    : m_kind(kind) {
  m_src[0] = src0;
  m_src[1] = src1;
  assert(get_condition_num_args(m_kind) == 2);
}

void IR2_Condition::invert() {
  m_kind = get_condition_opposite(m_kind);
}

bool IR2_Condition::operator==(const IR2_Condition& other) const {
  if (m_kind == other.m_kind) {
    for (int i = 0; i < get_condition_num_args(m_kind); i++) {
      if (m_src[i] != other.m_src[i]) {
        return false;
      }
    }
    return true;
  } else {
    return false;
  }
}

goos::Object IR2_Condition::to_form(const std::vector<DecompilerLabel>& labels,
                                    const Env* env) const {
  (void)labels;
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(get_condition_kind_name(m_kind)));
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    forms.push_back(m_src[i].to_form(labels, env));
  }
  if (forms.size() > 1) {
    return pretty_print::build_list(forms);
  } else {
    return forms.front();
  }
}

void IR2_Condition::get_regs(std::vector<Register>* out) const {
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    m_src[i].get_regs(out);
  }
}

/////////////////////////////
// SetVarConditionOp
/////////////////////////////

SetVarConditionOp::SetVarConditionOp(Variable dst, IR2_Condition condition, int my_idx)
    : AtomicOp(my_idx), m_dst(dst), m_condition(condition) {}

goos::Object SetVarConditionOp::to_form(const std::vector<DecompilerLabel>& labels,
                                        const Env* env) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"),
                                  pretty_print::to_symbol(m_dst.to_string(env)),
                                  m_condition.to_form(labels, env));
}

bool SetVarConditionOp::operator==(const AtomicOp& other) const {
  if (typeid(SetVarConditionOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const SetVarConditionOp*>(&other);
  assert(po);
  return m_dst == po->m_dst && m_condition == po->m_condition;
}

bool SetVarConditionOp::is_variable_set() const {
  return true;
}

bool SetVarConditionOp::is_sequence_point() const {
  return true;
}

Variable SetVarConditionOp::get_set_destination() const {
  return m_dst;
}

std::unique_ptr<Expr> SetVarConditionOp::get_set_source_as_expr() const {
  throw std::runtime_error("SetVarConditionOp::get_source_as_expr is not yet implemented.");
}

std::unique_ptr<Expr> SetVarConditionOp::get_as_expr() const {
  throw std::runtime_error("SetVarConditionOp::get_as_expr is not yet implemented.");
}

void SetVarConditionOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_condition.get_regs(&m_read_regs);
}

/////////////////////////////
// StoreOp
/////////////////////////////

StoreOp::StoreOp(int size, bool is_float, SimpleExpression addr, SimpleAtom value, int my_idx)
    : AtomicOp(my_idx),
      m_size(size),
      m_is_float(is_float),
      m_addr(std::move(addr)),
      m_value(std::move(value)) {}

goos::Object StoreOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  std::string store_name;
  if (m_is_float) {
    assert(m_size == 4);
    store_name = "s.f!";
  } else {
    switch (m_size) {
      case 1:
        store_name = "s.b!";
        break;
      case 2:
        store_name = "s.h!";
        break;
      case 4:
        store_name = "s.w!";
        break;
      case 8:
        store_name = "s.d!";
        break;
      default:
        assert(false);
    }
  }

  return pretty_print::build_list(pretty_print::to_symbol(store_name), m_addr.to_form(labels, env),
                                  m_value.to_form(labels, env));
}

bool StoreOp::operator==(const AtomicOp& other) const {
  if (typeid(StoreOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const StoreOp*>(&other);
  assert(po);

  return m_addr == po->m_addr && m_value == po->m_value;
}

bool StoreOp::is_variable_set() const {
  return false;
}

bool StoreOp::is_sequence_point() const {
  return true;
}

Variable StoreOp::get_set_destination() const {
  throw std::runtime_error("StoreOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> StoreOp::get_set_source_as_expr() const {
  throw std::runtime_error("StoreOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> StoreOp::get_as_expr() const {
  throw std::runtime_error("StoreOp::get_as_expr is not yet implemented");
}

void StoreOp::update_register_info() {
  m_addr.get_regs(&m_read_regs);
  m_value.get_regs(&m_read_regs);
}

/////////////////////////////
// LoadVarOp
/////////////////////////////

LoadVarOp::LoadVarOp(Kind kind, int size, Variable dst, SimpleExpression src, int my_idx)
    : AtomicOp(my_idx), m_kind(kind), m_size(size), m_dst(dst), m_src(std::move(src)) {}

goos::Object LoadVarOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("set!"),
                                     pretty_print::to_symbol(m_dst.to_string(env))};

  switch (m_kind) {
    case Kind::FLOAT:
      assert(m_size == 4);
      forms.push_back(pretty_print::build_list("l.f", m_src.to_form(labels, env)));
      break;
    case Kind::UNSIGNED:
      switch (m_size) {
        case 1:
          forms.push_back(pretty_print::build_list("l.bu", m_src.to_form(labels, env)));
          break;
        case 2:
          forms.push_back(pretty_print::build_list("l.hu", m_src.to_form(labels, env)));
          break;
        case 4:
          forms.push_back(pretty_print::build_list("l.wu", m_src.to_form(labels, env)));
          break;
        case 8:
          forms.push_back(pretty_print::build_list("l.d", m_src.to_form(labels, env)));
          break;
        default:
          assert(false);
      }
      break;
    case Kind::SIGNED:
      switch (m_size) {
        case 1:
          forms.push_back(pretty_print::build_list("l.b", m_src.to_form(labels, env)));
          break;
        case 2:
          forms.push_back(pretty_print::build_list("l.h", m_src.to_form(labels, env)));
          break;
        case 4:
          forms.push_back(pretty_print::build_list("l.w", m_src.to_form(labels, env)));
          break;
        default:
          assert(false);
      }
      break;
    default:
      assert(false);
  }
  return pretty_print::build_list(forms);
}

bool LoadVarOp::operator==(const AtomicOp& other) const {
  if (typeid(LoadVarOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const LoadVarOp*>(&other);
  assert(po);
  return m_dst == po->m_dst && m_src == po->m_src;
}

bool LoadVarOp::is_variable_set() const {
  return true;
}

bool LoadVarOp::is_sequence_point() const {
  return true;
}

Variable LoadVarOp::get_set_destination() const {
  return m_dst;
}

std::unique_ptr<Expr> LoadVarOp::get_set_source_as_expr() const {
  throw std::runtime_error("LoadVarOp::get_set_source_as_expr is not yet implemented");
}

std::unique_ptr<Expr> LoadVarOp::get_as_expr() const {
  throw std::runtime_error("LoadVarOp::get_as_expr is not yet implemented");
}

void LoadVarOp::update_register_info() {
  m_src.get_regs(&m_read_regs);
  m_write_regs.push_back(m_dst.reg());
}

/////////////////////////////
// IR2_BranchDelay
/////////////////////////////

IR2_BranchDelay::IR2_BranchDelay(Kind kind) : m_kind(kind) {
  assert(m_kind == Kind::NOP);
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind, Variable var0) : m_kind(kind) {
  assert(m_kind == Kind::SET_REG_FALSE || m_kind == Kind::SET_REG_TRUE ||
         m_kind == Kind::SET_BINTEGER || m_kind == Kind::SET_PAIR);
  assert(var0.mode() == Variable::Mode::WRITE);
  m_var[0] = var0;
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind, Variable var0, Variable var1) : m_kind(kind) {
  assert(m_kind == Kind::NEGATE || m_kind == Kind::SET_REG_REG);
  assert(var0.mode() == Variable::Mode::WRITE);
  assert(var1.mode() == Variable::Mode::READ);
  m_var[0] = var0;
  m_var[1] = var1;
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind, Variable var0, Variable var1, Variable var2)
    : m_kind(kind) {
  assert(m_kind == Kind::DSLLV);
  assert(var0.mode() == Variable::Mode::WRITE);
  assert(var1.mode() == Variable::Mode::READ);
  assert(var2.mode() == Variable::Mode::READ);
  m_var[0] = var0;
  m_var[1] = var1;
  m_var[2] = var2;
}

goos::Object IR2_BranchDelay::to_form(const std::vector<DecompilerLabel>& labels,
                                      const Env* env) const {
  (void)labels;
  switch (m_kind) {
    case Kind::NOP:
      return pretty_print::build_list("nop!");
    case Kind::SET_REG_FALSE:
      assert(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env), "#f");
    case Kind::SET_REG_TRUE:
      assert(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env), "#t");
    case Kind::SET_REG_REG:
      assert(m_var[0].has_value());
      assert(m_var[1].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env), m_var[1]->to_string(env));
    case Kind::SET_BINTEGER:
      assert(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env), "binteger");
    case Kind::SET_PAIR:
      assert(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env), "pair");
    case Kind::DSLLV:
      assert(m_var[0].has_value());
      assert(m_var[1].has_value());
      assert(m_var[2].has_value());
      return pretty_print::build_list(
          "set!", m_var[0]->to_string(env),
          pretty_print::build_list("sll", m_var[1]->to_string(env), m_var[2]->to_string(env)));
    case Kind::NEGATE:
      assert(m_var[0].has_value());
      assert(m_var[1].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_string(env),
                                      pretty_print::build_list("-", m_var[1]->to_string(env)));
    default:
      assert(false);
  }
}

bool IR2_BranchDelay::operator==(const IR2_BranchDelay& other) const {
  for (int i = 0; i < 3; i++) {
    if (m_var[i] != other.m_var[i]) {
      return false;
    }
  }
  return m_kind == other.m_kind;
}

void IR2_BranchDelay::get_regs(std::vector<Register>* write, std::vector<Register>* read) const {
  switch (m_kind) {
    case Kind::NOP:
      break;
    case Kind::SET_REG_FALSE:
    case Kind::SET_REG_TRUE:
    case Kind::SET_BINTEGER:
    case Kind::SET_PAIR:
      write->push_back(m_var[0]->reg());
      break;
    case Kind::SET_REG_REG:
    case Kind::NEGATE:
      write->push_back(m_var[0]->reg());
      read->push_back(m_var[1]->reg());
      break;
    case Kind::DSLLV:
      write->push_back(m_var[0]->reg());
      read->push_back(m_var[1]->reg());
      read->push_back(m_var[2]->reg());
      break;
    default:
      assert(false);
  }
}

/////////////////////////////
// BranchOp
/////////////////////////////

BranchOp::BranchOp(bool likely,
                   IR2_Condition condition,
                   int label,
                   IR2_BranchDelay branch_delay,
                   int my_idx)
    : AtomicOp(my_idx),
      m_likely(likely),
      m_condition(std::move(condition)),
      m_label(label),
      m_branch_delay(branch_delay) {}

goos::Object BranchOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  std::vector<goos::Object> forms;

  if (m_likely) {
    forms.push_back(pretty_print::to_symbol("bl!"));
  } else {
    forms.push_back(pretty_print::to_symbol("b!"));
  }

  forms.push_back(m_condition.to_form(labels, env));
  forms.push_back(pretty_print::to_symbol(labels.at(m_label).name));
  forms.push_back(m_branch_delay.to_form(labels, env));

  return pretty_print::build_list(forms);
}

bool BranchOp::operator==(const AtomicOp& other) const {
  if (typeid(BranchOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const BranchOp*>(&other);
  assert(po);
  return m_likely == po->m_likely && m_condition == po->m_condition && m_label == po->m_label &&
         m_branch_delay == po->m_branch_delay;
}

bool BranchOp::is_variable_set() const {
  return false;
}

bool BranchOp::is_sequence_point() const {
  return true;
}

Variable BranchOp::get_set_destination() const {
  throw std::runtime_error("BranchOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> BranchOp::get_set_source_as_expr() const {
  throw std::runtime_error("BranchOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> BranchOp::get_as_expr() const {
  throw std::runtime_error("BranchOp::get_as_expr is not yet implemented");
}

void BranchOp::update_register_info() {
  m_condition.get_regs(&m_read_regs);
  m_branch_delay.get_regs(&m_write_regs, &m_read_regs);
}

/////////////////////////////
// SpecialOp
/////////////////////////////

SpecialOp::SpecialOp(Kind kind, int my_idx) : AtomicOp(my_idx), m_kind(kind) {}

goos::Object SpecialOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  (void)labels;
  (void)env;
  switch (m_kind) {
    case Kind::NOP:
      return pretty_print::build_list("nop!");
    case Kind::BREAK:
      return pretty_print::build_list("break!");
    case Kind::CRASH:
      return pretty_print::build_list("crash!");
    case Kind::SUSPEND:
      return pretty_print::build_list("suspend");
    default:
      assert(false);
  }
}

bool SpecialOp::operator==(const AtomicOp& other) const {
  if (typeid(SpecialOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const SpecialOp*>(&other);
  assert(po);

  return m_kind == po->m_kind;
}

bool SpecialOp::is_variable_set() const {
  return false;
}

bool SpecialOp::is_sequence_point() const {
  return true;
}

Variable SpecialOp::get_set_destination() const {
  throw std::runtime_error("SpecialOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> SpecialOp::get_set_source_as_expr() const {
  throw std::runtime_error("SpecialOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> SpecialOp::get_as_expr() const {
  throw std::runtime_error("SpecialOp::get_as_expr not yet implemented");
}

void SpecialOp::update_register_info() {
  switch (m_kind) {
    case Kind::NOP:
    case Kind::BREAK:
    case Kind::CRASH:
      return;
    case Kind::SUSPEND:
      // todo - confirm this is true.
      // the suspend operation is written in a way where it doesn't use temporaries to make the call
      // but the actual suspend operation doesn't seem to preserve temporaries. Maybe the plan was
      // to save temp registers at some point, but they later gave up on this?
      clobber_temps();
      return;
    default:
      assert(false);
  }
}

/////////////////////////////
// CallOp
/////////////////////////////

CallOp::CallOp(int my_idx) : AtomicOp(my_idx) {}

goos::Object CallOp::to_form(const std::vector<DecompilerLabel>& labels, const Env* env) const {
  (void)labels;
  (void)env;
  return pretty_print::build_list("call!");
}

bool CallOp::operator==(const AtomicOp& other) const {
  if (typeid(CallOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const CallOp*>(&other);
  assert(po);
  return true;
}

bool CallOp::is_variable_set() const {
  return false;
}

bool CallOp::is_sequence_point() const {
  return true;
}

Variable CallOp::get_set_destination() const {
  throw std::runtime_error("CallOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> CallOp::get_set_source_as_expr() const {
  throw std::runtime_error("CallOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> CallOp::get_as_expr() const {
  throw std::runtime_error("CallOp::get_as_expr not yet implemented");
}

void CallOp::update_register_info() {
  // throw std::runtime_error("CallOp::update_register_info cannot be done until types are known");
  m_read_regs.push_back(Register(Reg::GPR, Reg::T9));
  clobber_temps();
}

/////////////////////////////
// ConditionalMoveFalseOp
/////////////////////////////

ConditionalMoveFalseOp::ConditionalMoveFalseOp(Variable dst, Variable src, bool on_zero, int my_idx)
    : AtomicOp(my_idx), m_dst(dst), m_src(src), m_on_zero(on_zero) {}

goos::Object ConditionalMoveFalseOp::to_form(const std::vector<DecompilerLabel>& labels,
                                             const Env* env) const {
  (void)labels;
  return pretty_print::build_list(m_on_zero ? "cmove-#f-zero" : "cmove-#f-nonzero",
                                  m_dst.to_string(env), m_src.to_string(env));
}

bool ConditionalMoveFalseOp::operator==(const AtomicOp& other) const {
  if (typeid(ConditionalMoveFalseOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const ConditionalMoveFalseOp*>(&other);
  assert(po);
  return m_dst == po->m_dst && m_src == po->m_src && m_on_zero == po->m_on_zero;
}

bool ConditionalMoveFalseOp::is_variable_set() const {
  return false;
}

bool ConditionalMoveFalseOp::is_sequence_point() const {
  return true;
}

Variable ConditionalMoveFalseOp::get_set_destination() const {
  throw std::runtime_error("ConditionalMoveFalseOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> ConditionalMoveFalseOp::get_set_source_as_expr() const {
  throw std::runtime_error("ConditionalMoveFalseOp cannot be treated as a set! operation");
}

std::unique_ptr<Expr> ConditionalMoveFalseOp::get_as_expr() const {
  throw std::runtime_error("ConditionalMoveFalseOp::get_as_expr is not yet implemented");
}

void ConditionalMoveFalseOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_read_regs.push_back(m_src.reg());
}
}  // namespace decompiler