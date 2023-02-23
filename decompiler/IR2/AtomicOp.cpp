#include "AtomicOp.h"

#include <stdexcept>
#include <utility>

#include "Form.h"
#include "OpenGoalMapping.h"

#include "common/goal_constants.h"
#include "common/goos/PrettyPrinter.h"
#include "common/util/Assert.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "third-party/fmt/core.h"

namespace decompiler {
/////////////////////////////
// VARIABLE
/////////////////////////////

RegisterAccess::RegisterAccess(AccessMode mode, Register reg, int atomic_idx, bool allow_all)
    : m_mode(mode), m_reg(reg), m_atomic_idx(atomic_idx) {
  // make sure we're using a valid GPR.
  if (reg.get_kind() == Reg::GPR && !allow_all) {
    if (!(Reg::allowed_local_gprs[reg.get_gpr()] || reg.get_gpr() == Reg::S6)) {
      throw std::runtime_error("Variable could not be constructed from register " +
                               reg.to_string());
    }
  }
}

goos::Object RegisterAccess::to_form(const Env& env, Print mode) const {
  switch (mode) {
    case Print::AS_REG:
      return pretty_print::to_symbol(m_reg.to_string());
    case Print::FULL:
      return pretty_print::to_symbol(fmt::format("{}-{:03d}-{}", m_reg.to_charp(), m_atomic_idx,
                                                 m_mode == AccessMode::READ ? 'r' : 'w'));
    case Print::AS_VARIABLE:
      return env.get_variable_name_with_cast(m_reg, m_atomic_idx, m_mode);
    case Print::AS_VARIABLE_NO_CAST:
      if (env.has_local_vars()) {
        return pretty_print::to_symbol(env.get_variable_name(*this));
      } else {
        return pretty_print::to_symbol(fmt::format("{}-{:03d}-{}", m_reg.to_charp(), m_atomic_idx,
                                                   m_mode == AccessMode::READ ? 'r' : 'w'));
      }
    case Print::AUTOMATIC:
      if (env.has_local_vars()) {
        return env.get_variable_name_with_cast(m_reg, m_atomic_idx, m_mode);
      } else {
        return pretty_print::to_symbol(m_reg.to_string());
      }
    default:
      ASSERT(false);
      return {};
  }
}

std::string RegisterAccess::to_string(const Env& env, Print mode) const {
  return to_form(env, mode).print();
}

bool RegisterAccess::operator==(const RegisterAccess& other) const {
  return m_mode == other.m_mode && m_reg == other.m_reg && m_atomic_idx == other.m_atomic_idx;
}

bool RegisterAccess::operator!=(const RegisterAccess& other) const {
  return !((*this) == other);
}

/////////////////////////////
// AtomicOp
/////////////////////////////
AtomicOp::AtomicOp(int my_idx) : m_my_idx(my_idx) {}

std::string AtomicOp::to_string(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  return pretty_print::to_string(to_form(labels, env));
}

std::string AtomicOp::to_string(const Env& env) const {
  return to_string(env.file->labels, env);
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

SimpleAtom SimpleAtom::make_var(const RegisterAccess& var) {
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

SimpleAtom SimpleAtom::make_sym_val_ptr(const std::string& name) {
  SimpleAtom result;
  result.m_kind = Kind::SYMBOL_VAL_PTR;
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

/*!
 * Mark this atom as a float. It will be printed as a float.
 * This can only be applied to an "integer" atom.
 * This should be used carefully, as this doesn't handle casts/types - it just changes the
 * representation, which will do the wrong thing unless the type system is aware of this
 * too.
 */
void SimpleAtom::mark_as_float() {
  ASSERT(is_int());
  m_display_int_as_float = true;
}

bool SimpleAtom::is_integer_promoted_to_float() const {
  return m_kind == Kind::INTEGER_CONSTANT && m_display_int_as_float;
}

float SimpleAtom::get_integer_promoted_to_float() const {
  ASSERT(is_integer_promoted_to_float());
  s32 as_s32 = get_int();
  ASSERT(get_int() == (s64)as_s32);
  float result;
  memcpy(&result, &as_s32, 4);
  return result;
}

goos::Object SimpleAtom::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  switch (m_kind) {
    case Kind::VARIABLE:
      return m_variable.to_form(env);
    case Kind::INTEGER_CONSTANT: {
      if (m_display_int_as_float) {
        float f;
        s32 as_s32 = m_int;
        ASSERT(((s64)as_s32) == m_int);  // float should always be a sign extended 32-bit value.
        memcpy(&f, &as_s32, 4);
        if (f == f && std::isfinite(f)) {
          return goos::Object::make_float(f);
        } else {
          // nan or weird
          return pretty_print::to_symbol(fmt::format("(the-as float #x{:x})", (u32)m_int));
        }
      } else {
        if (std::abs(m_int) > INT32_MAX) {
          u64 v = m_int;
          return pretty_print::to_symbol(fmt::format("#x{:x}", v));
        } else {
          return goos::Object::make_integer(m_int);
        }
      }
    }

    case Kind::SYMBOL_PTR:
      if (m_string == "#t") {
        return pretty_print::to_symbol("#t");
      } else {
        return pretty_print::to_symbol(fmt::format("'{}", m_string));
      }
    case Kind::SYMBOL_VAL:
      return pretty_print::to_symbol(m_string);
    case Kind::EMPTY_LIST:
      return pretty_print::to_symbol("'()");
    case Kind::STATIC_ADDRESS:
      return pretty_print::to_symbol(labels.at(m_int).name);
    case Kind::SYMBOL_VAL_PTR:
      return pretty_print::to_symbol(fmt::format("(&-> '{} value)", m_string));
    default:
      ASSERT(false);
      return {};
  }
}

goos::Object SimpleAtom::to_form(const Env& env) const {
  return to_form(env.file->labels, env);
}

std::string SimpleAtom::to_string(const Env& env) const {
  return to_form(env).print();
}

void SimpleAtom::collect_vars(RegAccessSet& vars) const {
  if (is_var()) {
    vars.insert(var());
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
      ASSERT(false);
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
    case SimpleExpression::Kind::PCPYLD:
      return "pcypld";
    case SimpleExpression::Kind::VECTOR_PLUS:
      return "vector+!2";
    case SimpleExpression::Kind::VECTOR_MINUS:
      return "vector-!2";
    case SimpleExpression::Kind::VECTOR_FLOAT_PRODUCT:
      return "vector-float*!2";
    case SimpleExpression::Kind::VECTOR_CROSS:
      return "veccross";
    case SimpleExpression::Kind::SUBU_L32_S7:
      return "subu-s7";
    case SimpleExpression::Kind::VECTOR_3_DOT:
      return "vec3dot";
    case SimpleExpression::Kind::VECTOR_4_DOT:
      return "vec4dot";
    case SimpleExpression::Kind::VECTOR_LENGTH:
      return "veclength";
    case SimpleExpression::Kind::VECTOR_PLUS_FLOAT_TIMES:
      return "vecplusfloattimes";
    case SimpleExpression::Kind::SET_ON_LESS_THAN:
    case SimpleExpression::Kind::SET_ON_LESS_THAN_IMM:
      return "set-on-less-than";
    default:
      ASSERT(false);
      return {};
  }
}
}  // namespace

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
    case SimpleExpression::Kind::PCPYLD:
      return 2;
    case SimpleExpression::Kind::VECTOR_PLUS:
    case SimpleExpression::Kind::VECTOR_MINUS:
    case SimpleExpression::Kind::VECTOR_FLOAT_PRODUCT:
    case SimpleExpression::Kind::VECTOR_CROSS:
      return 3;
    case SimpleExpression::Kind::SUBU_L32_S7:
      return 1;
    case SimpleExpression::Kind::VECTOR_3_DOT:
    case SimpleExpression::Kind::VECTOR_4_DOT:
      return 2;
    case SimpleExpression::Kind::SET_ON_LESS_THAN:
    case SimpleExpression::Kind::SET_ON_LESS_THAN_IMM:
      return 2;
    case SimpleExpression::Kind::VECTOR_LENGTH:
      return 1;
    case SimpleExpression::Kind::VECTOR_PLUS_FLOAT_TIMES:
      return 4;
    default:
      ASSERT(false);
      return -1;
  }
}

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0) : n_args(1) {
  m_args[0] = arg0;
  m_kind = kind;
  ASSERT(get_simple_expression_arg_count(kind) == 1);
}

SimpleExpression::SimpleExpression(Kind kind, const SimpleAtom& arg0, const SimpleAtom& arg1)
    : n_args(2) {
  m_args[0] = arg0;
  m_args[1] = arg1;
  m_kind = kind;
  ASSERT(get_simple_expression_arg_count(kind) == 2);
}

SimpleExpression::SimpleExpression(Kind kind,
                                   const SimpleAtom& arg0,
                                   const SimpleAtom& arg1,
                                   const SimpleAtom& arg2)
    : n_args(3) {
  m_args[0] = arg0;
  m_args[1] = arg1;
  m_args[2] = arg2;
  m_kind = kind;
  ASSERT(get_simple_expression_arg_count(kind) == 3);
}

SimpleExpression::SimpleExpression(Kind kind,
                                   const SimpleAtom& arg0,
                                   const SimpleAtom& arg1,
                                   const SimpleAtom& arg2,
                                   const SimpleAtom& arg3)
    : n_args(4) {
  m_args[0] = arg0;
  m_args[1] = arg1;
  m_args[2] = arg2;
  m_args[3] = arg3;
  m_kind = kind;
  ASSERT(get_simple_expression_arg_count(kind) == 4);
}

goos::Object SimpleExpression::to_form(const std::vector<DecompilerLabel>& labels,
                                       const Env& env) const {
  std::vector<goos::Object> forms;
  if (m_kind == Kind::IDENTITY) {
    // we are "identity" so just pass through the atom
    ASSERT(args() == 1);
    return get_arg(0).to_form(labels, env);
  } else {
    forms.push_back(pretty_print::to_symbol(get_simple_expression_op_name(m_kind)));
    for (int i = 0; i < args(); i++) {
      forms.push_back(get_arg(i).to_form(labels, env));
    }
    return pretty_print::build_list(forms);
  }
}

std::string SimpleExpression::to_string(const Env& env) const {
  return to_form(env.file->labels, env).print();
}

bool SimpleExpression::operator==(const SimpleExpression& other) const {
  if (m_kind != other.m_kind) {
    return false;
  }
  ASSERT(args() == other.args());
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

void SimpleExpression::collect_vars(RegAccessSet& vars) const {
  for (int i = 0; i < args(); i++) {
    get_arg(i).collect_vars(vars);
  }
}

/////////////////////////////
// SetVarOp
/////////////////////////////

goos::Object SetVarOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"), m_dst.to_form(env),
                                  m_src.to_form(labels, env));
}

bool SetVarOp::operator==(const AtomicOp& other) const {
  if (typeid(SetVarOp) != typeid(other)) {
    return false;
  }
  auto po = dynamic_cast<const SetVarOp*>(&other);
  ASSERT(po);
  return m_dst == po->m_dst && m_src == po->m_src;
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

RegisterAccess SetVarOp::get_set_destination() const {
  return m_dst;
}

void SetVarOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_src.get_regs(&m_read_regs);
}

void SetVarOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_dst);
  m_src.collect_vars(vars);
}

/////////////////////////////
// AsmOp
/////////////////////////////

AsmOp::AsmOp(Instruction instr, int my_idx) : AtomicOp(my_idx), m_instr(std::move(instr)) {
  ASSERT(m_instr.n_dst <= 1);
  if (m_instr.n_dst == 1) {
    auto& dst = m_instr.get_dst(0);
    if (dst.is_reg()) {
      auto reg = dst.get_reg();
      if (reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR || reg.get_kind() == Reg::VF) {
        m_dst = RegisterAccess(AccessMode::WRITE, reg, my_idx, true);
      }
    }
  }

  ASSERT(m_instr.n_src <= 4);
  for (int i = 0; i < m_instr.n_src; i++) {
    auto& src = m_instr.get_src(i);
    if (src.is_reg()) {
      auto reg = src.get_reg();
      if (reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR || reg.get_kind() == Reg::VF) {
        if (reg != Register(Reg::GPR, Reg::R0) ||
            (m_instr.kind == InstructionKind::PCPYUD || m_instr.kind == InstructionKind::PEXTUW)) {
          m_src[i] = RegisterAccess(AccessMode::READ, reg, my_idx, true);
        }
      }
    }
  }
}

goos::Object AsmOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("." + m_instr.op_name_to_string()));
  ASSERT(m_instr.n_dst <= 1);

  if (m_instr.n_dst == 1) {
    if (m_dst.has_value()) {
      // then print it as a variable
      forms.push_back(m_dst.value().to_form(env));
    } else {
      // print the atom
      forms.push_back(pretty_print::to_symbol(m_instr.get_dst(0).to_string(labels)));
    }
  }

  ASSERT(m_instr.n_src <= 4);
  for (int i = 0; i < m_instr.n_src; i++) {
    if (m_src[i].has_value()) {
      forms.push_back(m_src[i].value().to_form(env));
    } else {
      forms.push_back(pretty_print::to_symbol(m_instr.get_src(i).to_string(labels)));
    }
  }

  // note: to correctly represent a MOVN/MOVZ in our IR, we need to both read and write the
  // destination register, so we append a read to the end here.
  if (m_instr.kind == InstructionKind::MOVZ || m_instr.kind == InstructionKind::MOVN) {
    RegisterAccess ra(AccessMode::READ, m_dst->reg(), m_dst->idx());
    forms.push_back(ra.to_form(env));
  }

  return pretty_print::build_list(forms);
}

goos::Object AsmOp::to_open_goal_form(const std::vector<DecompilerLabel>& labels,
                                      const Env& env) const {
  std::vector<goos::Object> forms;

  std::vector<std::optional<RegisterAccess>> src;
  for (int i = 0; i < m_instr.n_src; i++) {
    auto v = m_src[i];
    src.push_back(v);
  }

  OpenGOALAsm goalOp = OpenGOALAsm(m_instr, m_dst, src);
  forms.push_back(pretty_print::to_symbol(goalOp.full_function_name()));

  ASSERT(m_instr.n_dst <= 1);
  if (m_instr.n_dst == 1) {
    if (m_dst.has_value()) {
      // then print it as a variable
      forms.push_back(m_dst.value().to_form(env));
    } else {
      // print the atom
      forms.push_back(pretty_print::to_symbol(m_instr.get_dst(0).to_string(labels)));
    }
  }

  ASSERT(m_instr.n_src <= 4);
  std::vector<goos::Object> args = goalOp.get_args(labels, env);
  forms.insert(forms.end(), args.begin(), args.end());

  return pretty_print::build_list(forms);
}  // namespace decompiler

bool AsmOp::operator==(const AtomicOp& other) const {
  if (typeid(AsmOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const AsmOp*>(&other);
  ASSERT(po);

  return (m_instr == po->m_instr) && (m_dst == po->m_dst) && (m_src[0] == po->m_src[0]) &&
         (m_src[1] == po->m_src[1]) && (m_src[2] == po->m_src[2]);
}

bool AsmOp::is_sequence_point() const {
  return true;
}

RegisterAccess AsmOp::get_set_destination() const {
  throw std::runtime_error("AsmOp cannot be treated as a set! operation");
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

  if (m_instr.kind == InstructionKind::MOVN || m_instr.kind == InstructionKind::MOVZ) {
    // in the case that MOVN/MOVZ don't do the move, they effectively read the original value.
    m_read_regs.push_back(m_dst->reg());
  }

  if (m_instr.kind >= FIRST_COP2_MACRO && m_instr.kind <= LAST_COP2_MACRO) {
    switch (m_instr.kind) {
      case InstructionKind::VMSUBQ:
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_Q));
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;

      case InstructionKind::VMULAQ:
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_Q));
        m_write_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;

        // Read Q register
      case InstructionKind::VADDQ:
      case InstructionKind::VSUBQ:
      case InstructionKind::VMULQ:
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_Q));
        break;

        // Write ACC register
      case InstructionKind::VADDA:
      case InstructionKind::VADDA_BC:
      case InstructionKind::VMULA:
      case InstructionKind::VMULA_BC:
      case InstructionKind::VOPMULA:
        m_write_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;

        // Write Q register
      case InstructionKind::VDIV:
      case InstructionKind::VSQRT:
      case InstructionKind::VRSQRT:
        m_write_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_Q));
        break;

        // Read acc register
      case InstructionKind::VMADD:
      case InstructionKind::VMADD_BC:
      case InstructionKind::VMSUB:
      case InstructionKind::VMSUB_BC:
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;
      case InstructionKind::VOPMSUB:
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;

        // Read/Write acc register
      case InstructionKind::VMADDA:
      case InstructionKind::VMADDA_BC:
      case InstructionKind::VMSUBA_BC:
        m_write_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        m_read_regs.push_back(Register(Reg::SPECIAL, Reg::MACRO_ACC));
        break;

      case InstructionKind::VMOVE:
      case InstructionKind::VFTOI0:
      case InstructionKind::VFTOI4:
      case InstructionKind::VFTOI12:
      case InstructionKind::VFTOI15:
      case InstructionKind::VITOF0:
      case InstructionKind::VITOF12:
      case InstructionKind::VITOF15:
      case InstructionKind::VABS:
      case InstructionKind::VADD:
      case InstructionKind::VADD_BC:
      case InstructionKind::VSUB:
      case InstructionKind::VSUB_BC:
      case InstructionKind::VMUL:
      case InstructionKind::VMUL_BC:
      case InstructionKind::VMINI:
      case InstructionKind::VMINI_BC:
      case InstructionKind::VMAX:
      case InstructionKind::VMAX_BC:
      case InstructionKind::VCLIP:
      case InstructionKind::VNOP:

        // anything using one of these should be manually analyzed.
      case InstructionKind::VMTIR:
      case InstructionKind::VIAND:
      case InstructionKind::VLQI:
      case InstructionKind::VIADDI:
      case InstructionKind::VSQI:
      case InstructionKind::VRGET:
      case InstructionKind::VRXOR:
      case InstructionKind::VRNEXT:
      case InstructionKind::VWAITQ:  // okay if following vsqrt/vrsqrt/vdiv
      case InstructionKind::VCALLMS:

        // do not read/write acc/q
        break;

      default:
        ASSERT(false);
        break;
    }
  }
}

void AsmOp::collect_vars(RegAccessSet& vars) const {
  if (m_dst.has_value()) {
    vars.insert(*m_dst);
  }

  for (auto& x : m_src) {
    if (x.has_value()) {
      vars.insert(*x);
    }
  }

  if (m_instr.kind == InstructionKind::MOVN || m_instr.kind == InstructionKind::MOVZ) {
    // the conditional moves read their write register, but don't have it listed as a write
    // in the actual ASM.  We handle this difference for the variable naming system here.
    RegisterAccess ra(AccessMode::READ, m_dst->reg(), m_dst->idx());
    vars.insert(ra);
  }
}
/////////////////////////////
// Condition
/////////////////////////////

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
      ASSERT(false);
      return "";
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
      ASSERT(false);
      return -1;
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
      ASSERT(false);
      return IR2_Condition::Kind::INVALID;
  }
}

bool condition_uses_float(IR2_Condition::Kind kind) {
  switch (kind) {
    case IR2_Condition::Kind::FLOAT_LESS_THAN:
    case IR2_Condition::Kind::FLOAT_LEQ:
    case IR2_Condition::Kind::FLOAT_NOT_EQUAL:
    case IR2_Condition::Kind::FLOAT_EQUAL:
    case IR2_Condition::Kind::FLOAT_GEQ:
    case IR2_Condition::Kind::FLOAT_GREATER_THAN:
      return true;
    default:
      return false;
  }
}

IR2_Condition::IR2_Condition(Kind kind) : m_kind(kind) {
  ASSERT(get_condition_num_args(m_kind) == 0);
}

IR2_Condition::IR2_Condition(Kind kind, const SimpleAtom& src0) : m_kind(kind) {
  m_src[0] = src0;
  ASSERT(get_condition_num_args(m_kind) == 1);
}

IR2_Condition::IR2_Condition(Kind kind, const SimpleAtom& src0, const SimpleAtom& src1)
    : m_kind(kind) {
  m_src[0] = src0;
  m_src[1] = src1;
  ASSERT(get_condition_num_args(m_kind) == 2);
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
                                    const Env& env) const {
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

void IR2_Condition::collect_vars(RegAccessSet& vars) const {
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    m_src[i].collect_vars(vars);
  }
}

/////////////////////////////
// SetVarConditionOp
/////////////////////////////

SetVarConditionOp::SetVarConditionOp(RegisterAccess dst, IR2_Condition condition, int my_idx)
    : AtomicOp(my_idx), m_dst(dst), m_condition(std::move(condition)) {}

goos::Object SetVarConditionOp::to_form(const std::vector<DecompilerLabel>& labels,
                                        const Env& env) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"), m_dst.to_form(env),
                                  m_condition.to_form(labels, env));
}

bool SetVarConditionOp::operator==(const AtomicOp& other) const {
  if (typeid(SetVarConditionOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const SetVarConditionOp*>(&other);
  ASSERT(po);
  return m_dst == po->m_dst && m_condition == po->m_condition;
}

bool SetVarConditionOp::is_sequence_point() const {
  return true;
}

RegisterAccess SetVarConditionOp::get_set_destination() const {
  return m_dst;
}

void SetVarConditionOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_condition.get_regs(&m_read_regs);
}

void SetVarConditionOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_dst);
  m_condition.collect_vars(vars);
}

/////////////////////////////
// StoreOp
/////////////////////////////

StoreOp::StoreOp(int size, Kind kind, SimpleExpression addr, SimpleAtom value, int my_idx)
    : AtomicOp(my_idx),
      m_size(size),
      m_kind(kind),
      m_addr(std::move(addr)),
      m_value(std::move(value)) {}

goos::Object StoreOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  std::string store_name;
  switch (m_kind) {
    case Kind::INTEGER:
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
        case 16:
          store_name = "s.q!";
          break;
        default:
          ASSERT(false);
      }
      break;
    case Kind::FLOAT:
      ASSERT(m_size == 4);
      store_name = "s.f!";
      break;
    case Kind::VECTOR_FLOAT:
      ASSERT(m_size == 16);
      store_name = "s.vf!";
      break;
    default:
      ASSERT(false);
  }

  return pretty_print::build_list(pretty_print::to_symbol(store_name), m_addr.to_form(labels, env),
                                  m_value.to_form(labels, env));
}

bool StoreOp::operator==(const AtomicOp& other) const {
  if (typeid(StoreOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const StoreOp*>(&other);
  ASSERT(po);

  return m_addr == po->m_addr && m_value == po->m_value;
}

bool StoreOp::is_sequence_point() const {
  return true;
}

RegisterAccess StoreOp::get_set_destination() const {
  throw std::runtime_error("StoreOp cannot be treated as a set! operation");
}

void StoreOp::update_register_info() {
  m_addr.get_regs(&m_read_regs);
  m_value.get_regs(&m_read_regs);
}

void StoreOp::collect_vars(RegAccessSet& vars) const {
  m_addr.collect_vars(vars);
  m_value.collect_vars(vars);
}

/////////////////////////////
// LoadVarOp
/////////////////////////////

std::string load_kind_to_string(LoadVarOp::Kind kind) {
  switch (kind) {
    case LoadVarOp::Kind::FLOAT:
      return "float";
    case LoadVarOp::Kind::VECTOR_FLOAT:
      return "vector-float";
    case LoadVarOp::Kind::SIGNED:
      return "signed";
    case LoadVarOp::Kind::UNSIGNED:
      return "unsigned";
    default:
      ASSERT(false);
  }
}

LoadVarOp::LoadVarOp(Kind kind, int size, RegisterAccess dst, SimpleExpression src, int my_idx)
    : AtomicOp(my_idx), m_kind(kind), m_size(size), m_dst(dst), m_src(std::move(src)) {}

goos::Object LoadVarOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("set!"), m_dst.to_form(env)};

  switch (m_kind) {
    case Kind::FLOAT:
      ASSERT(m_size == 4);
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
        case 16:
          forms.push_back(pretty_print::build_list("l.q", m_src.to_form(labels, env)));
          break;
        default:
          ASSERT(false);
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
          ASSERT(false);
      }
      break;
    case Kind::VECTOR_FLOAT:
      ASSERT(m_size == 16);
      forms.push_back(pretty_print::build_list("l.vf", m_src.to_form(labels, env)));
      break;

    default:
      ASSERT(false);
  }
  return pretty_print::build_list(forms);
}

bool LoadVarOp::operator==(const AtomicOp& other) const {
  if (typeid(LoadVarOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const LoadVarOp*>(&other);
  ASSERT(po);
  return m_dst == po->m_dst && m_src == po->m_src;
}

bool LoadVarOp::is_sequence_point() const {
  return true;
}

RegisterAccess LoadVarOp::get_set_destination() const {
  return m_dst;
}

void LoadVarOp::update_register_info() {
  m_src.get_regs(&m_read_regs);
  m_write_regs.push_back(m_dst.reg());
}

void LoadVarOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_dst);
  m_src.collect_vars(vars);
}

/////////////////////////////
// IR2_BranchDelay
/////////////////////////////

IR2_BranchDelay::IR2_BranchDelay(Kind kind) : m_kind(kind) {
  ASSERT(m_kind == Kind::NOP || m_kind == Kind::NO_DELAY || m_kind == Kind::UNKNOWN);
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind, RegisterAccess var0) : m_kind(kind) {
  ASSERT(m_kind == Kind::SET_REG_FALSE || m_kind == Kind::SET_REG_TRUE ||
         m_kind == Kind::SET_BINTEGER || m_kind == Kind::SET_PAIR);
  ASSERT(var0.mode() == AccessMode::WRITE);
  m_var[0] = var0;
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind, RegisterAccess var0, RegisterAccess var1)
    : m_kind(kind) {
  ASSERT(m_kind == Kind::NEGATE || m_kind == Kind::SET_REG_REG);
  ASSERT(var0.mode() == AccessMode::WRITE);
  ASSERT(var1.mode() == AccessMode::READ);
  m_var[0] = var0;
  m_var[1] = var1;
}

IR2_BranchDelay::IR2_BranchDelay(Kind kind,
                                 RegisterAccess var0,
                                 RegisterAccess var1,
                                 RegisterAccess var2)
    : m_kind(kind) {
  ASSERT(m_kind == Kind::DSLLV);
  ASSERT(var0.mode() == AccessMode::WRITE);
  ASSERT(var1.mode() == AccessMode::READ);
  ASSERT(var2.mode() == AccessMode::READ);
  m_var[0] = var0;
  m_var[1] = var1;
  m_var[2] = var2;
}

goos::Object IR2_BranchDelay::to_form(const std::vector<DecompilerLabel>& labels,
                                      const Env& env) const {
  (void)labels;
  switch (m_kind) {
    case Kind::NOP:
      return pretty_print::build_list("nop!");
    case Kind::NO_DELAY:
      return pretty_print::build_list("no-delay!");
    case Kind::SET_REG_FALSE:
      ASSERT(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env), "#f");
    case Kind::SET_REG_TRUE:
      ASSERT(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env), "#t");
    case Kind::SET_REG_REG:
      ASSERT(m_var[0].has_value());
      ASSERT(m_var[1].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env), m_var[1]->to_form(env));
    case Kind::SET_BINTEGER:
      ASSERT(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env), "binteger");
    case Kind::SET_PAIR:
      ASSERT(m_var[0].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env), "pair");
    case Kind::DSLLV:
      ASSERT(m_var[0].has_value());
      ASSERT(m_var[1].has_value());
      ASSERT(m_var[2].has_value());
      return pretty_print::build_list(
          "set!", m_var[0]->to_form(env),
          pretty_print::build_list("sll", m_var[1]->to_form(env), m_var[2]->to_form(env)));
    case Kind::NEGATE:
      ASSERT(m_var[0].has_value());
      ASSERT(m_var[1].has_value());
      return pretty_print::build_list("set!", m_var[0]->to_form(env),
                                      pretty_print::build_list("-", m_var[1]->to_form(env)));
    case Kind::UNKNOWN:
      return pretty_print::build_list("unknown-branch-delay!");
    default:
      ASSERT(false);
      return {};
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
    case Kind::NO_DELAY:
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
      ASSERT(false);
  }
}

void IR2_BranchDelay::collect_vars(RegAccessSet& vars) const {
  for (auto& x : m_var) {
    if (x.has_value()) {
      vars.insert(*x);
    }
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

goos::Object BranchOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
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
  ASSERT(po);
  return m_likely == po->m_likely && m_condition == po->m_condition && m_label == po->m_label &&
         m_branch_delay == po->m_branch_delay;
}

bool BranchOp::is_sequence_point() const {
  return true;
}

RegisterAccess BranchOp::get_set_destination() const {
  throw std::runtime_error("BranchOp cannot be treated as a set! operation");
}

void BranchOp::update_register_info() {
  m_condition.get_regs(&m_read_regs);
  m_branch_delay.get_regs(&m_write_regs, &m_read_regs);
}

void BranchOp::collect_vars(RegAccessSet& vars) const {
  m_condition.collect_vars(vars);
  m_branch_delay.collect_vars(vars);
}

/////////////////////////////
// AsmBranchOp
/////////////////////////////

AsmBranchOp::AsmBranchOp(bool likely,
                         IR2_Condition condition,
                         int label,
                         AtomicOp* branch_delay,
                         int my_idx)
    : AtomicOp(my_idx),
      m_likely(likely),
      m_condition(std::move(condition)),
      m_label(label),
      m_branch_delay(branch_delay) {}

AsmBranchOp::AsmBranchOp(bool likely,
                         IR2_Condition condition,
                         int label,
                         std::shared_ptr<AtomicOp> branch_delay,
                         int my_idx)
    : AtomicOp(my_idx),
      m_likely(likely),
      m_condition(std::move(condition)),
      m_label(label),
      m_branch_delay_sp(branch_delay) {
  m_branch_delay = m_branch_delay_sp.get();
}

goos::Object AsmBranchOp::to_form(const std::vector<DecompilerLabel>& labels,
                                  const Env& env) const {
  std::vector<goos::Object> forms;

  if (m_likely) {
    forms.push_back(pretty_print::to_symbol("bl!"));
  } else {
    forms.push_back(pretty_print::to_symbol("b!"));
  }

  forms.push_back(m_condition.to_form(labels, env));
  forms.push_back(pretty_print::to_symbol(labels.at(m_label).name));

  if (m_branch_delay) {
    forms.push_back(m_branch_delay->to_form(labels, env));
  }

  return pretty_print::build_list(forms);
}

bool AsmBranchOp::operator==(const AtomicOp& other) const {
  if (typeid(BranchOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const AsmBranchOp*>(&other);
  ASSERT(po);
  return m_likely == po->m_likely && m_condition == po->m_condition && m_label == po->m_label &&
         m_branch_delay == po->m_branch_delay;
}

bool AsmBranchOp::is_sequence_point() const {
  return true;
}

RegisterAccess AsmBranchOp::get_set_destination() const {
  throw std::runtime_error("AsmBranchOp cannot be treated as a set! operation");
}

void AsmBranchOp::update_register_info() {
  m_condition.get_regs(&m_read_regs);
  if (m_branch_delay) {
    m_branch_delay->update_register_info();

    for (auto x : m_branch_delay->read_regs()) {
      m_read_regs.push_back(x);
    }

    for (auto x : m_branch_delay->write_regs()) {
      m_write_regs.push_back(x);
    }

    for (auto x : m_branch_delay->clobber_regs()) {
      m_clobber_regs.push_back(x);
    }
  }
}

void AsmBranchOp::collect_vars(RegAccessSet& vars) const {
  m_condition.collect_vars(vars);
  if (m_branch_delay) {
    m_branch_delay->collect_vars(vars);
  }
}

/////////////////////////////
// SpecialOp
/////////////////////////////

SpecialOp::SpecialOp(Kind kind, int my_idx) : AtomicOp(my_idx), m_kind(kind) {}

goos::Object SpecialOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
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
      ASSERT(false);
      return {};
  }
}

bool SpecialOp::operator==(const AtomicOp& other) const {
  if (typeid(SpecialOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const SpecialOp*>(&other);
  ASSERT(po);

  return m_kind == po->m_kind;
}

bool SpecialOp::is_sequence_point() const {
  return true;
}

RegisterAccess SpecialOp::get_set_destination() const {
  throw std::runtime_error("SpecialOp cannot be treated as a set! operation");
}

void SpecialOp::update_register_info() {
  switch (m_kind) {
    case Kind::NOP:
    case Kind::BREAK:
    case Kind::CRASH:
      return;
    case Kind::SUSPEND:
      // todo - confirm this is true.
      // the suspend operation is written in a way where it doesn't use temporaries to make the
      // call but the actual suspend operation doesn't seem to preserve temporaries. Maybe the
      // plan was to save temp registers at some point, but they later gave up on this?
      clobber_temps();
      return;
    default:
      ASSERT(false);
  }
}

void SpecialOp::collect_vars(RegAccessSet&) const {}

/////////////////////////////
// CallOp
/////////////////////////////

CallOp::CallOp(int my_idx)
    : AtomicOp(my_idx),
      m_function_var(AccessMode::READ, Register(Reg::GPR, Reg::T9), my_idx),
      m_return_var(AccessMode::WRITE, Register(Reg::GPR, Reg::V0), my_idx) {}

goos::Object CallOp::to_form(const std::vector<DecompilerLabel>& labels, const Env& env) const {
  (void)labels;
  (void)env;
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("call!"));
  for (auto& x : m_arg_vars) {
    forms.push_back(x.to_form(env));
  }
  return pretty_print::build_list(forms);
}

bool CallOp::operator==(const AtomicOp& other) const {
  if (typeid(CallOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const CallOp*>(&other);
  ASSERT(po);
  return true;
}

bool CallOp::is_sequence_point() const {
  return true;
}

RegisterAccess CallOp::get_set_destination() const {
  throw std::runtime_error("CallOp cannot be treated as a set! operation");
}

void CallOp::update_register_info() {
  // throw std::runtime_error("CallOp::update_register_info cannot be done until types are
  // known");
  m_read_regs.push_back(Register(Reg::GPR, Reg::T9));
  // previously, if the type analysis succeeds, it would remove this if the function doesn't
  // return a value. however, this turned out to be not quite right because GOAL internally thinks
  // that all functions return a value.
  m_write_regs.push_back(Register(Reg::GPR, Reg::V0));
  clobber_temps();
}

void CallOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_function_var);
  for (auto& e : m_arg_vars) {
    vars.insert(e);
  }

  // even if we don't actually return a value, GOAL pretends like we do.
  vars.insert(m_return_var);
}

/////////////////////////////
// ConditionalMoveFalseOp
/////////////////////////////

ConditionalMoveFalseOp::ConditionalMoveFalseOp(RegisterAccess dst,
                                               RegisterAccess src,
                                               RegisterAccess old_value,
                                               bool on_zero,
                                               int my_idx)
    : AtomicOp(my_idx), m_dst(dst), m_src(src), m_old_value(old_value), m_on_zero(on_zero) {}

goos::Object ConditionalMoveFalseOp::to_form(const std::vector<DecompilerLabel>& labels,
                                             const Env& env) const {
  (void)labels;
  return pretty_print::build_list(m_on_zero ? "cmove-#f-zero" : "cmove-#f-nonzero",
                                  m_dst.to_form(env), m_src.to_form(env), m_old_value.to_form(env));
}

bool ConditionalMoveFalseOp::operator==(const AtomicOp& other) const {
  if (typeid(ConditionalMoveFalseOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const ConditionalMoveFalseOp*>(&other);
  ASSERT(po);
  return m_dst == po->m_dst && m_src == po->m_src && m_on_zero == po->m_on_zero &&
         m_old_value == po->m_old_value;
}

bool ConditionalMoveFalseOp::is_sequence_point() const {
  return true;
}

RegisterAccess ConditionalMoveFalseOp::get_set_destination() const {
  throw std::runtime_error("ConditionalMoveFalseOp cannot be treated as a set! operation");
}

void ConditionalMoveFalseOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
  m_read_regs.push_back(m_src.reg());
  m_read_regs.push_back(m_old_value.reg());
}

void ConditionalMoveFalseOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_dst);
  vars.insert(m_src);
  vars.insert(m_old_value);
}

bool get_as_reg_offset(const SimpleExpression& expr, IR2_RegOffset* out) {
  if (expr.kind() == SimpleExpression::Kind::ADD && expr.get_arg(0).is_var() &&
      expr.get_arg(1).is_int()) {
    out->var = expr.get_arg(0).var();
    out->reg = expr.get_arg(0).var().reg();
    out->offset = expr.get_arg(1).get_int();
    return true;
  }

  if (expr.is_identity() && expr.get_arg(0).is_var()) {
    out->var = expr.get_arg(0).var();
    out->reg = expr.get_arg(0).var().reg();
    out->offset = 0;
    return true;
  }
  return false;
}

/////////////////////////////
// FunctionEndOp
/////////////////////////////

FunctionEndOp::FunctionEndOp(int my_idx)
    : AtomicOp(my_idx), m_return_reg(AccessMode::READ, Register(Reg::GPR, Reg::V0), my_idx) {}

goos::Object FunctionEndOp::to_form(const std::vector<DecompilerLabel>&, const Env& env) const {
  if (m_function_has_return_value) {
    return pretty_print::build_list("ret-value", m_return_reg.to_form(env));
  } else {
    return pretty_print::build_list("ret-none");
  }
}

bool FunctionEndOp::operator==(const AtomicOp& other) const {
  if (typeid(FunctionEndOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const FunctionEndOp*>(&other);
  ASSERT(po);
  return m_function_has_return_value == po->m_function_has_return_value;
}

bool FunctionEndOp::is_sequence_point() const {
  return true;
}

RegisterAccess FunctionEndOp::get_set_destination() const {
  throw std::runtime_error("FunctionEndOp cannot be treated as a set! operation");
}

void FunctionEndOp::update_register_info() {
  m_read_regs.push_back(Register(Reg::GPR, Reg::V0));
}

void FunctionEndOp::collect_vars(RegAccessSet& vars) const {
  if (m_function_has_return_value) {
    vars.insert(m_return_reg);
  }
}

/////////////////////////////
// StackSpillStoreOp
/////////////////////////////

StackSpillStoreOp::StackSpillStoreOp(const SimpleAtom& value, int size, int offset, int my_idx)
    : AtomicOp(my_idx), m_value(value), m_size(size), m_offset(offset) {
  if (m_value.is_var()) {
    ASSERT(m_value.var().mode() == AccessMode::READ);
  }
}

goos::Object StackSpillStoreOp::to_form(const std::vector<DecompilerLabel>&, const Env& env) const {
  return pretty_print::build_list(
      fmt::format("stack-store {} :offset {} :sz {}", m_value.to_string(env), m_offset, m_size));
}

bool StackSpillStoreOp::operator==(const AtomicOp& other) const {
  if (typeid(StackSpillStoreOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const StackSpillStoreOp*>(&other);
  ASSERT(po);
  return m_size == po->m_size && m_value == po->m_value && m_offset == po->m_offset;
}

bool StackSpillStoreOp::is_sequence_point() const {
  return true;  // this might not be totally true, but it seems kind of scary to allow it to
                // reorder.
}

void StackSpillStoreOp::update_register_info() {
  if (m_value.is_var()) {
    m_read_regs.push_back(m_value.var().reg());
  }
}

void StackSpillStoreOp::collect_vars(RegAccessSet& vars) const {
  m_value.collect_vars(vars);
}

RegisterAccess StackSpillStoreOp::get_set_destination() const {
  throw std::runtime_error("StackSpillStoreOp cannot be treated as a set! operation");
}

/////////////////////////////
// StackSpillLoadOp
/////////////////////////////

StackSpillLoadOp::StackSpillLoadOp(RegisterAccess dst,
                                   int size,
                                   int offset,
                                   bool is_signed,
                                   int my_idx)
    : AtomicOp(my_idx), m_dst(dst), m_size(size), m_offset(offset), m_is_signed(is_signed) {
  ASSERT(m_dst.mode() == AccessMode::WRITE);
}

goos::Object StackSpillLoadOp::to_form(const std::vector<DecompilerLabel>&, const Env& env) const {
  return pretty_print::build_list(fmt::format("stack-load {} :offset {} :sz {} :sext #{}",
                                              m_dst.to_string(env), m_offset, m_size,
                                              m_is_signed ? 't' : 'f'));
}

bool StackSpillLoadOp::operator==(const AtomicOp& other) const {
  if (typeid(StackSpillStoreOp) != typeid(other)) {
    return false;
  }

  auto po = dynamic_cast<const StackSpillLoadOp*>(&other);
  ASSERT(po);
  return m_size == po->m_size && m_dst == po->m_dst && m_offset == po->m_offset &&
         m_is_signed == po->m_is_signed;
}

bool StackSpillLoadOp::is_sequence_point() const {
  return true;  // this might not be totally true, but it seems kind of scary to allow it to
                // reorder.
}

void StackSpillLoadOp::update_register_info() {
  m_write_regs.push_back(m_dst.reg());
}

void StackSpillLoadOp::collect_vars(RegAccessSet& vars) const {
  vars.insert(m_dst);
}

RegisterAccess StackSpillLoadOp::get_set_destination() const {
  // todo!
  throw std::runtime_error("StackSpillLoadOp cannot be treated as a set! operation");
}

bool is_op_2(AtomicOp* op,
             MatchParam<SimpleExpression::Kind> kind,
             MatchParam<Register> dst,
             MatchParam<Register> src0,
             Register* dst_out,
             Register* src0_out) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = set->dst();
  if (dst != dest.reg()) {
    return false;
  }

  auto math = set->src();
  if (kind != math.kind()) {
    return false;
  }

  auto arg = math.get_arg(0);

  if (!arg.is_var() || src0 != arg.var().reg()) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest.reg();
  }

  if (src0_out) {
    *src0_out = arg.var().reg();
  }

  return true;
}
}  // namespace decompiler
