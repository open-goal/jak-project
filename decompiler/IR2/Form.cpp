#include "Form.h"

#include <algorithm>
#include <utility>

#include "common/goos/PrettyPrinter.h"
#include "common/type_system/TypeSystem.h"
#include "common/util/print_float.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/data_decompile.h"
#include "decompiler/util/sparticle_decompile.h"

namespace decompiler {

///////////////////
// FormPool
///////////////////

FormPool::~FormPool() {
  for (auto& x : m_forms) {
    delete x;
  }

  for (auto& x : m_elements) {
    delete x;
  }
}

///////////////////
// FormElement
///////////////////

std::string FormElement::to_string(const Env& env) const {
  return to_form(env).print();
}

void FormElement::push_to_stack(const Env& env, FormPool&, FormStack&) {
  throw std::runtime_error(fmt::format("push_to_stack not implemented for {}: {}", to_string(env),
                                       typeid(*this).name()));
}

goos::Object FormElement::to_form_as_condition_internal(const Env& env) const {
  return to_form(env);
}

bool FormElement::active() const {
  return true;
}

goos::Object FormElement::to_form(const Env& env) const {
  if (active()) {
    return to_form_internal(env);
  } else {
    return pretty_print::build_list("empty-form");
  }
}

///////////////////
// Form
//////////////////

goos::Object Form::to_form(const Env& env) const {
  ASSERT(!m_elements.empty());
  if (m_elements.size() == 1) {
    return m_elements.front()->to_form(env);
  } else {
    std::vector<goos::Object> forms;
    forms.push_back(pretty_print::to_symbol("begin"));
    for (auto& x : m_elements) {
      if (x->active()) {
        forms.push_back(x->to_form_internal(env));
      }
    }

    if (forms.size() == 2) {
      return forms.at(1);
    }
    return pretty_print::build_list(forms);
  }
}

goos::Object Form::to_form_as_condition(const Env& env) const {
  ASSERT(!m_elements.empty());
  if (m_elements.size() == 1) {
    return m_elements.front()->to_form_as_condition_internal(env);
  } else {
    std::vector<goos::Object> forms;
    forms.push_back(pretty_print::to_symbol("begin"));
    for (size_t i = 0; i < m_elements.size(); i++) {
      const auto& x = m_elements.at(i);
      if (i == m_elements.size() - 1) {
        forms.push_back(x->to_form_as_condition_internal(env));
      } else {
        if (x->active()) {
          forms.push_back(x->to_form_internal(env));
        }
      }
    }

    return pretty_print::build_list(forms);
  }
}

std::string Form::to_string(const Env& env) const {
  return to_form(env).print();
}

void Form::inline_forms(std::vector<goos::Object>& forms, const Env& env) const {
  for (auto& x : m_elements) {
    if (x->active()) {
      forms.push_back(x->to_form_internal(env));
    }
  }
}

void Form::apply(const std::function<void(FormElement*)>& f) {
  for (size_t i = 0; i < m_elements.size(); i++) {
    m_elements.at(i)->apply(f);
  }
}

void Form::apply_form(const std::function<void(Form*)>& f) {
  f(this);
  for (auto& x : m_elements) {
    x->apply_form(f);
  }
}

void Form::collect_vars(RegAccessSet& vars, bool recursive) const {
  for (auto e : m_elements) {
    e->collect_vars(vars, recursive);
  }
}

void Form::get_modified_regs(RegSet& regs) const {
  for (auto e : m_elements) {
    e->get_modified_regs(regs);
  }
}

/////////////////////////////
// SimpleExpressionElement
/////////////////////////////

SimpleExpressionElement::SimpleExpressionElement(SimpleExpression expr, int my_idx)
    : m_expr(std::move(expr)), m_my_idx(my_idx) {}

goos::Object SimpleExpressionElement::to_form_internal(const Env& env) const {
  return m_expr.to_form(env.file->labels, env);
}

void SimpleExpressionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void SimpleExpressionElement::apply_form(const std::function<void(Form*)>&) {}

bool SimpleExpressionElement::is_sequence_point() const {
  throw std::runtime_error("Should not check if a SimpleExpressionElement is a sequence point");
}

void SimpleExpressionElement::collect_vars(RegAccessSet& vars, bool) const {
  m_expr.collect_vars(vars);
}

void SimpleExpressionElement::get_modified_regs(RegSet& regs) const {
  (void)regs;
}

/////////////////////////////
// StoreElement
/////////////////////////////

StoreElement::StoreElement(const StoreOp* op) : m_op(op) {}

goos::Object StoreElement::to_form_internal(const Env& env) const {
  return m_op->to_form(env.file->labels, env);
}

void StoreElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StoreElement::apply_form(const std::function<void(Form*)>&) {}

void StoreElement::collect_vars(RegAccessSet& vars, bool) const {
  return m_op->collect_vars(vars);
}

void StoreElement::get_modified_regs(RegSet& regs) const {
  (void)regs;
}

/////////////////////////////
// LoadSourceElement
/////////////////////////////

LoadSourceElement::LoadSourceElement(Form* addr,
                                     int size,
                                     LoadVarOp::Kind kind,
                                     const std::optional<IR2_RegOffset>& load_source_ro,
                                     const TP_Type& ro_reg_type)
    : m_addr(addr),
      m_size(size),
      m_kind(kind),
      m_load_source_ro(load_source_ro),
      m_ro_reg_type(ro_reg_type) {
  m_addr->parent_element = this;
}

goos::Object LoadSourceElement::to_form_internal(const Env& env) const {
  switch (m_kind) {
    case LoadVarOp::Kind::FLOAT:
      ASSERT(m_size == 4);
      return pretty_print::build_list("l.f", m_addr->to_form(env));
    case LoadVarOp::Kind::UNSIGNED:
      switch (m_size) {
        case 1:
          return pretty_print::build_list("l.bu", m_addr->to_form(env));
        case 2:
          return pretty_print::build_list("l.hu", m_addr->to_form(env));
        case 4:
          return pretty_print::build_list("l.wu", m_addr->to_form(env));
        case 8:
          return pretty_print::build_list("l.d", m_addr->to_form(env));
        case 16:
          return pretty_print::build_list("l.q", m_addr->to_form(env));
        default:
          ASSERT(false);
          return {};
      }
      break;
    case LoadVarOp::Kind::SIGNED:
      switch (m_size) {
        case 1:
          return pretty_print::build_list("l.b", m_addr->to_form(env));
        case 2:
          return pretty_print::build_list("l.h", m_addr->to_form(env));
        case 4:
          return pretty_print::build_list("l.w", m_addr->to_form(env));
        default:
          ASSERT(false);
          return {};
      }
      break;
    case LoadVarOp::Kind::VECTOR_FLOAT:
      ASSERT(m_size == 16);
      return pretty_print::build_list("l.vf", m_addr->to_form(env));
      break;
    default:
      ASSERT(false);
      return {};
  }
}

void LoadSourceElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_addr->apply(f);
}

void LoadSourceElement::apply_form(const std::function<void(Form*)>& f) {
  m_addr->apply_form(f);
}

void LoadSourceElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_addr->collect_vars(vars, recursive);
  }
}

void LoadSourceElement::get_modified_regs(RegSet& regs) const {
  m_addr->get_modified_regs(regs);
}

/////////////////////////////
// SimpleAtomElement
/////////////////////////////

SimpleAtomElement::SimpleAtomElement(const SimpleAtom& atom, bool omit_var_cast)
    : m_atom(atom), m_omit_var_cast(omit_var_cast) {
  if (m_omit_var_cast) {
    ASSERT(atom.is_var());
  }
}

goos::Object SimpleAtomElement::to_form_internal(const Env& env) const {
  if (m_omit_var_cast) {
    return m_atom.var().to_form(env, RegisterAccess::Print::AS_VARIABLE_NO_CAST);
  }
  return m_atom.to_form(env.file->labels, env);
}

void SimpleAtomElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void SimpleAtomElement::apply_form(const std::function<void(Form*)>&) {}

void SimpleAtomElement::collect_vars(RegAccessSet& vars, bool) const {
  return m_atom.collect_vars(vars);
}

void SimpleAtomElement::get_modified_regs(RegSet& regs) const {
  (void)regs;
}

/////////////////////////////
// SetVarElement
/////////////////////////////

SetVarElement::SetVarElement(const RegisterAccess& var,
                             Form* value,
                             bool is_sequence_point,
                             TypeSpec src_type,
                             const SetVarInfo& info)
    : m_dst(var),
      m_src(value),
      m_is_sequence_point(is_sequence_point),
      m_src_type(std::move(src_type)),
      m_var_info(info) {
  value->parent_element = this;
}

goos::Object SetVarElement::to_form_internal(const Env& env) const {
  ASSERT(active());
  auto reg_kind = m_dst.reg().get_kind();
  if ((reg_kind == Reg::FPR || reg_kind == Reg::GPR) && env.has_type_analysis() &&
      env.has_local_vars()) {
    auto expected_type = env.get_variable_type(m_dst, true);
    if (!env.dts->ts.tc(expected_type, m_src_type)) {
      return pretty_print::build_list(
          "set!", m_dst.to_form(env, RegisterAccess::Print::AS_VARIABLE_NO_CAST),
          pretty_print::build_list("the-as", expected_type.print(), m_src->to_form(env)));
    }
  }

  return pretty_print::build_list(
      "set!", m_dst.to_form(env, RegisterAccess::Print::AS_VARIABLE_NO_CAST), m_src->to_form(env));
}

std::optional<TypeSpec> SetVarElement::required_cast(const Env& env) const {
  auto expected_type = env.get_variable_type(m_dst, true);
  if (!env.dts->ts.tc(expected_type, m_src_type)) {
    return expected_type;
  }
  return std::nullopt;
}

void SetVarElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_src->apply(f);
}

void SetVarElement::apply_form(const std::function<void(Form*)>& f) {
  m_src->apply_form(f);
}

bool SetVarElement::is_sequence_point() const {
  return m_is_sequence_point;
}

void SetVarElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (m_var_info.is_dead_set || m_var_info.is_dead_false) {
    return;
  }
  vars.insert(m_dst);
  if (recursive) {
    m_src->collect_vars(vars, recursive);
  }
}

void SetVarElement::get_modified_regs(RegSet& regs) const {
  regs.insert(m_dst.reg());
  m_src->get_modified_regs(regs);
}

bool SetVarElement::active() const {
  if (is_eliminated_coloring_move() || is_dead_false_set()) {
    return false;
  } else {
    return true;
  }
}

StoreInSymbolElement::StoreInSymbolElement(std::string sym_name,
                                           SimpleExpression value,
                                           std::optional<TypeSpec> cast_for_set,
                                           std::optional<TypeSpec> cast_for_define,
                                           int my_idx)
    : m_sym_name(std::move(sym_name)),
      m_value(std::move(value)),
      m_cast_for_set(std::move(cast_for_set)),
      m_cast_for_define(std::move(cast_for_define)),
      m_my_idx(my_idx) {}

goos::Object StoreInSymbolElement::to_form_internal(const Env& env) const {
  if (m_cast_for_set) {
    return pretty_print::build_list(
        "set!", m_sym_name,
        pretty_print::build_list(fmt::format("the-as {}", m_cast_for_set->print()),
                                 m_value.to_form(env.file->labels, env)));
  } else {
    return pretty_print::build_list("set!", m_sym_name, m_value.to_form(env.file->labels, env));
  }
}

void StoreInSymbolElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StoreInSymbolElement::apply_form(const std::function<void(Form*)>&) {}

void StoreInSymbolElement::collect_vars(RegAccessSet& vars, bool) const {
  m_value.collect_vars(vars);
}

void StoreInSymbolElement::get_modified_regs(RegSet&) const {}

StoreInPairElement::StoreInPairElement(bool is_car,
                                       RegisterAccess pair,
                                       SimpleExpression value,
                                       int my_idx)
    : m_is_car(is_car), m_pair(pair), m_value(value), m_my_idx(my_idx) {}

goos::Object StoreInPairElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(
      "set!", pretty_print::build_list(m_is_car ? "car" : "cdr", m_pair.to_form(env)),
      m_value.to_form(env.file->labels, env));
}

void StoreInPairElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StoreInPairElement::apply_form(const std::function<void(Form*)>&) {}

void StoreInPairElement::collect_vars(RegAccessSet& vars, bool) const {
  m_value.collect_vars(vars);
  vars.insert(m_pair);
}

void StoreInPairElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// SetFormFormElement
/////////////////////////////

SetFormFormElement::SetFormFormElement(Form* dst,
                                       Form* src,
                                       std::optional<TypeSpec> cast_for_set,
                                       std::optional<TypeSpec> cast_for_define)
    : m_dst(dst),
      m_src(src),
      m_cast_for_set(std::move(cast_for_set)),
      m_cast_for_define(std::move(cast_for_define)) {
  m_dst->parent_element = this;
  m_src->parent_element = this;
}

goos::Object SetFormFormElement::to_form_internal(const Env& env) const {
  if (m_cast_for_set) {
    return pretty_print::build_list(
        fmt::format("set!"), m_dst->to_form(env),
        pretty_print::build_list(fmt::format("the-as {}", m_cast_for_set->print()),
                                 m_src->to_form(env)));
  } else {
    std::vector<goos::Object> forms = {pretty_print::to_symbol("set!"), m_dst->to_form(env),
                                       m_src->to_form(env)};
    return pretty_print::build_list(forms);
  }
}

goos::Object SetFormFormElement::to_form_for_define(
    const Env& env,
    const std::optional<std::string>& docstring) const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("define"), m_dst->to_form(env)};
  if (docstring) {
    forms.push_back(pretty_print::to_symbol(fmt::format("\"{}\"", docstring.value())));
  }
  if (m_cast_for_define) {
    // for vu-function, we just put a 0. These aren't supported
    if (*m_cast_for_define == TypeSpec("vu-function")) {
      forms.push_back(pretty_print::build_list(fmt::format("the-as {}", m_cast_for_define->print()),
                                               pretty_print::to_symbol("0")));
      return pretty_print::build_list(forms);
    }
    forms.push_back(pretty_print::build_list(fmt::format("the-as {}", m_cast_for_define->print()),
                                             m_src->to_form(env)));
    return pretty_print::build_list(forms);
  } else {
    forms.push_back(m_src->to_form(env));
    return pretty_print::build_list(forms);
  }
}

void SetFormFormElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_src->apply(f);
  m_dst->apply(f);
}

void SetFormFormElement::apply_form(const std::function<void(Form*)>& f) {
  m_src->apply_form(f);
  m_dst->apply_form(f);
}

bool SetFormFormElement::is_sequence_point() const {
  return true;
}

void SetFormFormElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_src->collect_vars(vars, recursive);
    m_dst->collect_vars(vars, recursive);
  }
}

void SetFormFormElement::get_modified_regs(RegSet& regs) const {
  (void)regs;
}

/////////////////////////////
// AtomicOpElement
/////////////////////////////

AtomicOpElement::AtomicOpElement(AtomicOp* op) : m_op(op) {}

goos::Object AtomicOpElement::to_form_internal(const Env& env) const {
  return m_op->to_form(env.file->labels, env);
}

void AtomicOpElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AtomicOpElement::apply_form(const std::function<void(Form*)>&) {}

void AtomicOpElement::collect_vars(RegAccessSet& vars, bool) const {
  m_op->collect_vars(vars);
}

void AtomicOpElement::get_modified_regs(RegSet& regs) const {
  for (auto r : m_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// AsmBranchElement
/////////////////////////////

AsmBranchElement::AsmBranchElement(AsmBranchOp* branch_op, Form* branch_delay, bool likely)
    : m_branch_op(branch_op), m_branch_delay(branch_delay), m_likely(likely) {
  m_branch_delay->parent_element = this;
  for (auto& elt : m_branch_delay->elts()) {
    ASSERT(elt->parent_form == m_branch_delay);
  }
}

goos::Object AsmBranchElement::to_form_internal(const Env& env) const {
  auto f = m_branch_op->to_form(env.file->labels, env);
  return pretty_print::build_list(f, m_branch_delay->to_form(env));  // temp hack
}

void AsmBranchElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_branch_delay->apply(f);
}

void AsmBranchElement::apply_form(const std::function<void(Form*)>& f) {
  m_branch_delay->apply_form(f);
}

void AsmBranchElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_branch_delay->collect_vars(vars, recursive);
  }
  m_branch_op->collect_vars(vars);
}

void AsmBranchElement::get_modified_regs(RegSet& regs) const {
  m_branch_delay->get_modified_regs(regs);
  for (auto r : m_branch_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_branch_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// TranslatedAsmBranch
/////////////////////////////

TranslatedAsmBranch::TranslatedAsmBranch(Form* branch_condition,
                                         Form* branch_delay,
                                         int label_id,
                                         bool likely)
    : m_branch_condition(branch_condition),
      m_branch_delay(branch_delay),
      m_label_id(label_id),
      m_likely(likely) {
  if (m_branch_delay) {
    m_branch_delay->parent_element = this;
  }

  m_branch_condition->parent_element = this;
}

goos::Object TranslatedAsmBranch::to_form_internal(const Env& env) const {
  // auto& cfg = env.func->cfg;
  auto& label = env.file->labels.at(m_label_id);
  int instr_in_function = (label.offset / 4 - env.func->start_word);

  int block_id = -20;
  if (instr_in_function == env.func->basic_blocks.back().end_word) {
    block_id = env.func->basic_blocks.size() - 1;
  } else {
    int atomic_op_in_function =
        env.func->ir2.atomic_ops->instruction_to_atomic_op.at(instr_in_function);
    auto& ao = env.func->ir2.atomic_ops;

    for (int i = 0; i < (int)ao->block_id_to_first_atomic_op.size(); i++) {
      if (ao->block_id_to_first_atomic_op.at(i) == atomic_op_in_function) {
        block_id = i;
        break;
      }
    }
  }

  ASSERT(block_id >= 0);

  if (m_branch_delay) {
    if (m_branch_delay->parent_element != this) {
      ASSERT_MSG(false, fmt::format("bad ptr. Parent is {}",
                                    m_branch_delay->parent_element->to_string(env)));
    }

    ASSERT(m_branch_delay->parent_element->parent_form);
    std::vector<goos::Object> list = {
        pretty_print::to_symbol("b!"), m_branch_condition->to_form(env),
        pretty_print::to_symbol(fmt::format("cfg-{}", block_id)),
        pretty_print::to_symbol(m_likely ? ":likely-delay" : ":delay"),
        m_branch_delay->to_form(env)};

    return pretty_print::build_list(list);
  } else {
    std::vector<goos::Object> list = {pretty_print::to_symbol("b!"),
                                      m_branch_condition->to_form(env),
                                      pretty_print::to_symbol(fmt::format("cfg-{}", block_id))};

    return pretty_print::build_list(list);
  }
}

void TranslatedAsmBranch::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_branch_condition->apply(f);
  if (m_branch_delay) {
    m_branch_delay->apply(f);
  }
}

void TranslatedAsmBranch::apply_form(const std::function<void(Form*)>& f) {
  m_branch_condition->apply_form(f);
  if (m_branch_delay) {
    m_branch_delay->apply_form(f);
  }
}

void TranslatedAsmBranch::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_branch_condition->collect_vars(vars, recursive);
    if (m_branch_delay) {
      if (m_branch_delay->parent_element != this) {
        ASSERT_MSG(false,
                   fmt::format("bad ptr. Parent is {}", (void*)m_branch_delay->parent_element));
      }

      for (auto& elt : m_branch_delay->elts()) {
        ASSERT(elt->parent_form == m_branch_delay);
      }

      m_branch_delay->collect_vars(vars, recursive);
    }
  }
}

void TranslatedAsmBranch::get_modified_regs(RegSet& regs) const {
  m_branch_condition->get_modified_regs(regs);
  if (m_branch_delay) {
    m_branch_delay->get_modified_regs(regs);
  }
}

/////////////////////////////
// AsmOpElement
/////////////////////////////

AsmOpElement::AsmOpElement(const AsmOp* op) : m_op(op) {}

goos::Object AsmOpElement::to_form_internal(const Env& env) const {
  return m_op->to_form(env.file->labels, env);
}

void AsmOpElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AsmOpElement::apply_form(const std::function<void(Form*)>&) {}

void AsmOpElement::collect_vars(RegAccessSet& vars, bool) const {
  m_op->collect_vars(vars);
}

void AsmOpElement::get_modified_regs(RegSet& regs) const {
  for (auto r : m_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// OpenGoalAsmOpElement
/////////////////////////////

OpenGoalAsmOpElement::OpenGoalAsmOpElement(const AsmOp* op) : m_op(op) {}

goos::Object OpenGoalAsmOpElement::to_form_internal(const Env& env) const {
  return m_op->to_open_goal_form(env.file->labels, env);
}

void OpenGoalAsmOpElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void OpenGoalAsmOpElement::apply_form(const std::function<void(Form*)>&) {}

void OpenGoalAsmOpElement::collect_vars(RegAccessSet& vars, bool) const {
  m_op->collect_vars(vars);
}

void OpenGoalAsmOpElement::collect_vf_regs(RegSet& regs) const {
  for (auto r : m_op->read_regs()) {
    if (r.is_vu_float()) {
      regs.insert(r);
    }
  }

  for (auto r : m_op->write_regs()) {
    if (r.is_vu_float()) {
      regs.insert(r);
    }
  }

  for (auto r : m_op->clobber_regs()) {
    if (r.is_vu_float()) {
      regs.insert(r);
    }
  }
}

void OpenGoalAsmOpElement::get_modified_regs(RegSet& regs) const {
  for (auto r : m_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// ConditionElement
/////////////////////////////

ConditionElement::ConditionElement(IR2_Condition::Kind kind,
                                   std::optional<SimpleAtom> src0,
                                   std::optional<SimpleAtom> src1,
                                   RegSet consumed,
                                   bool flipped)
    : m_kind(kind), m_consumed(std::move(consumed)), m_flipped(flipped) {
  m_src[0] = src0;
  m_src[1] = src1;
}

goos::Object ConditionElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(get_condition_kind_name(m_kind)));
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    forms.push_back(m_src[i]->to_form(env.file->labels, env));
  }
  if (forms.size() > 1) {
    return pretty_print::build_list(forms);
  } else {
    return forms.front();
  }
}

goos::Object ConditionElement::to_form_as_condition_internal(const Env& env) const {
  if (m_kind == IR2_Condition::Kind::TRUTHY) {
    return m_src[0]->to_form(env.file->labels, env);
  } else {
    return to_form_internal(env);
  }
}

void ConditionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void ConditionElement::apply_form(const std::function<void(Form*)>&) {}

void ConditionElement::invert() {
  m_kind = get_condition_opposite(m_kind);
}

void ConditionElement::collect_vars(RegAccessSet& vars, bool) const {
  for (auto src : m_src) {
    if (src.has_value() && src->is_var()) {
      vars.insert(src->var());
    }
  }
}

void ConditionElement::get_modified_regs(RegSet& regs) const {
  (void)regs;
}

/////////////////////////////
// FunctionCallElement
/////////////////////////////

FunctionCallElement::FunctionCallElement(const CallOp* op) : m_op(op) {}

goos::Object FunctionCallElement::to_form_internal(const Env& env) const {
  return m_op->to_form(env.file->labels, env);
}

void FunctionCallElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void FunctionCallElement::apply_form(const std::function<void(Form*)>&) {}

void FunctionCallElement::collect_vars(RegAccessSet& vars, bool) const {
  return m_op->collect_vars(vars);
}

void FunctionCallElement::get_modified_regs(RegSet& regs) const {
  for (auto r : m_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// BranchElement
/////////////////////////////

BranchElement::BranchElement(const BranchOp* op) : m_op(op) {}

goos::Object BranchElement::to_form_internal(const Env& env) const {
  return m_op->to_form(env.file->labels, env);
}

void BranchElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void BranchElement::apply_form(const std::function<void(Form*)>&) {}

void BranchElement::collect_vars(RegAccessSet& vars, bool) const {
  return m_op->collect_vars(vars);
}

void BranchElement::get_modified_regs(RegSet& regs) const {
  for (auto r : m_op->write_regs()) {
    regs.insert(r);
  }

  for (auto r : m_op->clobber_regs()) {
    regs.insert(r);
  }
}

/////////////////////////////
// ReturnElement
/////////////////////////////

ReturnElement::ReturnElement(Form* _return_code, Form* _dead_code)
    : return_code(_return_code), dead_code(_dead_code) {
  return_code->parent_element = this;
  if (dead_code) {
    dead_code->parent_element = this;
  }
}

goos::Object ReturnElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("return"));
  forms.push_back(return_code->to_form(env));
  if (dead_code) {
    forms.push_back(dead_code->to_form(env));
  }
  return pretty_print::build_list(forms);
}

void ReturnElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  return_code->apply(f);
  if (dead_code) {
    dead_code->apply(f);
  }
}

void ReturnElement::apply_form(const std::function<void(Form*)>& f) {
  return_code->apply_form(f);
  if (dead_code) {
    dead_code->apply_form(f);
  }
}

void ReturnElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    return_code->collect_vars(vars, recursive);
    if (dead_code) {
      dead_code->collect_vars(vars, recursive);
    }
  }
}

void ReturnElement::get_modified_regs(RegSet& regs) const {
  return_code->get_modified_regs(regs);
  if (dead_code) {
    dead_code->get_modified_regs(regs);
  }
}

/////////////////////////////
// BreakElement
/////////////////////////////

BreakElement::BreakElement(Form* _return_code, Form* _dead_code, int _lid)
    : return_code(_return_code), dead_code(_dead_code), lid(_lid) {
  return_code->parent_element = this;
  if (dead_code) {
    dead_code->parent_element = this;
  }
}

goos::Object BreakElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  if (dead_code) {
    forms.push_back(pretty_print::to_symbol("break"));
    forms.push_back(pretty_print::build_list(return_code->to_form(env)));
    forms.push_back(pretty_print::build_list(dead_code->to_form(env)));
  } else {
    if (return_code->try_as_element<EmptyElement>()) {
      return pretty_print::build_list(fmt::format("goto cfg-{}", lid));
    } else {
      forms.push_back(pretty_print::to_symbol("begin"));
      return_code->inline_forms(forms, env);
      forms.push_back(pretty_print::build_list(fmt::format("goto cfg-{}", lid)));
    }
  }
  return pretty_print::build_list(forms);
}

void BreakElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  return_code->apply(f);
  if (dead_code) {
    dead_code->apply(f);
  }
}

void BreakElement::apply_form(const std::function<void(Form*)>& f) {
  return_code->apply_form(f);
  if (dead_code) {
    dead_code->apply_form(f);
  }
}

void BreakElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    return_code->collect_vars(vars, recursive);
    if (dead_code) {
      dead_code->collect_vars(vars, recursive);
    }
  }
}

void BreakElement::get_modified_regs(RegSet& regs) const {
  return_code->get_modified_regs(regs);
  if (dead_code) {
    dead_code->get_modified_regs(regs);
  }
}

/////////////////////////////
// CondWithElseElement
/////////////////////////////

CondWithElseElement::CondWithElseElement(std::vector<Entry> _entries, Form* _else_ir)
    : entries(std::move(_entries)), else_ir(_else_ir) {
  for (auto& e : entries) {
    e.condition->parent_element = this;
    e.body->parent_element = this;
  }
  else_ir->parent_element = this;
}

goos::Object CondWithElseElement::to_form_internal(const Env& env) const {
  // for now we only turn it into an if statement if both cases won't require a begin at the top
  // level. I think it is more common to write these as a two-case cond instead of an if with begin.
  if (entries.size() == 1 && entries.front().body->is_reasonable_for_if() &&
      else_ir->is_reasonable_for_if()) {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("if"));
    list.push_back(entries.front().condition->to_form_as_condition(env));
    list.push_back(entries.front().body->to_form(env));
    list.push_back(else_ir->to_form(env));
    return pretty_print::build_list(list);
  } else {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("cond"));
    for (auto& e : entries) {
      std::vector<goos::Object> entry;
      entry.push_back(e.condition->to_form_as_condition(env));
      e.body->inline_forms(entry, env);
      list.push_back(pretty_print::build_list(entry));
    }
    std::vector<goos::Object> else_form;
    else_form.push_back(pretty_print::to_symbol("else"));
    else_ir->inline_forms(else_form, env);
    list.push_back(pretty_print::build_list(else_form));
    return pretty_print::build_list(list);
  }
}

void CondWithElseElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& entry : entries) {
    entry.condition->apply(f);
    entry.body->apply(f);
  }
  else_ir->apply(f);
}

void CondWithElseElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& entry : entries) {
    entry.condition->apply_form(f);
    entry.body->apply_form(f);
  }
  else_ir->apply_form(f);
}

void CondWithElseElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    for (auto& entry : entries) {
      entry.condition->collect_vars(vars, recursive);
      entry.body->collect_vars(vars, recursive);
    }
    else_ir->collect_vars(vars, recursive);
  }
}

void CondWithElseElement::get_modified_regs(RegSet& regs) const {
  for (auto& e : entries) {
    e.condition->get_modified_regs(regs);
    e.body->get_modified_regs(regs);
  }
  else_ir->get_modified_regs(regs);
}

/////////////////////////////
// EmptyElement
/////////////////////////////

goos::Object EmptyElement::to_form_internal(const Env&) const {
  return pretty_print::build_list("empty");
}

void EmptyElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void EmptyElement::apply_form(const std::function<void(Form*)>&) {}
void EmptyElement::collect_vars(RegAccessSet&, bool) const {}
void EmptyElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// RLetElement
/////////////////////////////

bool cmp(Register x, Register y) {
  int comparison = x.to_string() > y.to_string();
  if (comparison <= 0)
    return true;
  return false;
}

RLetElement::RLetElement(Form* _body, RegSet _regs) : body(_body) {
  for (auto& reg : _regs) {
    sorted_regs.push_back(reg);
  }
  std::stable_sort(sorted_regs.begin(), sorted_regs.end(), cmp);
}

void RLetElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  body->apply(f);
}

goos::Object RLetElement::reg_list() const {
  std::vector<goos::Object> regs;
  for (auto& reg : sorted_regs) {
    if (reg.is_vu_float()) {
      std::string reg_name = reg.to_string() == "ACC" ? "acc" : reg.to_string();
      regs.push_back(
          pretty_print::build_list(pretty_print::to_symbol(fmt::format("{} :class vf", reg_name))));
    }
  }
  return pretty_print::build_list(regs);
}

bool RLetElement::needs_vf0_init() const {
  for (auto& reg : sorted_regs) {
    if (reg.get_kind() == Reg::RegisterKind::VF && reg.to_string() == "vf0") {
      return true;
    }
  }
  return false;
}

goos::Object RLetElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> rletForm;
  rletForm.push_back(pretty_print::to_symbol("rlet"));
  rletForm.push_back(reg_list());

  // NOTE - initialize any relevant registers in the body first
  if (needs_vf0_init()) {
    rletForm.push_back(pretty_print::to_symbol("(init-vf0-vector)"));  // Defined in vector-h.gc
  }

  body->inline_forms(rletForm, env);
  return pretty_print::build_list(rletForm);
}

void RLetElement::apply_form(const std::function<void(Form*)>& f) {
  body->apply_form(f);
}

void RLetElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    body->collect_vars(vars, recursive);
  }
}

void RLetElement::get_modified_regs(RegSet& regs) const {
  body->get_modified_regs(regs);
}

/////////////////////////////
// WhileElement
/////////////////////////////

WhileElement::WhileElement(Form* _condition, Form* _body) : condition(_condition), body(_body) {
  condition->parent_element = this;
  body->parent_element = this;
}

void WhileElement::apply(const std::function<void(FormElement*)>& f) {
  // note - this is done in program order, rather than print order. Not sure if this makes sense.
  f(this);
  body->apply(f);
  condition->apply(f);
}

goos::Object WhileElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> list;
  auto cond = condition->to_form_as_condition(env);
  if (cond == pretty_print::to_symbol("#t")) {
    list.push_back(pretty_print::to_symbol("loop"));
  } else {
    list.push_back(pretty_print::to_symbol("while"));
    list.push_back(condition->to_form_as_condition(env));
  }
  body->inline_forms(list, env);
  return pretty_print::build_list(list);
}

void WhileElement::apply_form(const std::function<void(Form*)>& f) {
  body->apply_form(f);
  condition->apply_form(f);
}

void WhileElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    body->collect_vars(vars, recursive);
    condition->collect_vars(vars, recursive);
  }
}

void WhileElement::get_modified_regs(RegSet& regs) const {
  condition->get_modified_regs(regs);
  body->get_modified_regs(regs);
}

/////////////////////////////
// UntilElement
/////////////////////////////

UntilElement::UntilElement(Form* _condition, Form* _body) : condition(_condition), body(_body) {
  condition->parent_element = this;
  body->parent_element = this;
}

void UntilElement::apply(const std::function<void(FormElement*)>& f) {
  // note - this is done in program order, rather than print order. Not sure if this makes sense.
  f(this);
  body->apply(f);
  condition->apply(f);
}

goos::Object UntilElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> list;
  list.push_back(pretty_print::to_symbol("until"));
  list.push_back(condition->to_form_as_condition(env));
  body->inline_forms(list, env);
  return pretty_print::build_list(list);
}

void UntilElement::apply_form(const std::function<void(Form*)>& f) {
  body->apply_form(f);
  condition->apply_form(f);
}

void UntilElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    body->collect_vars(vars, recursive);
    condition->collect_vars(vars, recursive);
  }
}

void UntilElement::get_modified_regs(RegSet& regs) const {
  condition->get_modified_regs(regs);
  body->get_modified_regs(regs);
}

/////////////////////////////
// ShortCircuitElement
/////////////////////////////

ShortCircuitElement::ShortCircuitElement(std::vector<Entry> _entries)
    : entries(std::move(_entries)) {
  for (auto& entry : entries) {
    entry.condition->parent_element = this;
  }
}

void ShortCircuitElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& x : entries) {
    x.condition->apply(f);
    //    if (x.output) {
    //      // not sure about this...
    //      x.output->apply(f);
    //    }
  }
}

void ShortCircuitElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& x : entries) {
    x.condition->apply_form(f);
    //    if (x.output) {
    //      // not sure about this...
    //      x.output->apply(f);
    //    }
  }
}

goos::Object ShortCircuitElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  switch (kind) {
    case UNKNOWN:
      forms.push_back(pretty_print::to_symbol("unknown-sc"));
      break;
    case AND:
      forms.push_back(pretty_print::to_symbol("and"));
      break;
    case OR:
      forms.push_back(pretty_print::to_symbol("or"));
      break;
    default:
      ASSERT(false);
  }
  for (auto& x : entries) {
    forms.push_back(x.condition->to_form_as_condition(env));
  }
  return pretty_print::build_list(forms);
}

void ShortCircuitElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  //  vars.insert(final_result);  // todo - this might be unused.
  if (recursive) {
    for (auto& entry : entries) {
      entry.condition->collect_vars(vars, recursive);
    }
  }
}

void ShortCircuitElement::get_modified_regs(RegSet& regs) const {
  for (auto& e : entries) {
    e.condition->get_modified_regs(regs);
  }
}

/////////////////////////////
// CondNoElseElement
/////////////////////////////

CondNoElseElement::CondNoElseElement(std::vector<Entry> _entries) : entries(std::move(_entries)) {
  for (auto& entry : entries) {
    entry.condition->parent_element = this;
    entry.body->parent_element = this;
  }
}

goos::Object CondNoElseElement::to_form_internal(const Env& env) const {
  if (entries.size() == 1 && entries.front().body->is_reasonable_for_if()) {
    // print as an if statement if we can put the body in a single form.
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("if"));
    list.push_back(entries.front().condition->to_form_as_condition(env));
    list.push_back(entries.front().body->to_form(env));
    return pretty_print::build_list(list);
  } else if (entries.size() == 1) {
    // turn into a when if the body requires multiple forms
    // todo check to see if the condition starts with a NOT and this can be simplified to an
    // unless.
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("when"));
    list.push_back(entries.front().condition->to_form_as_condition(env));
    entries.front().body->inline_forms(list, env);
    return pretty_print::build_list(list);
  } else {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("cond"));
    for (auto& e : entries) {
      std::vector<goos::Object> entry;
      entry.push_back(e.condition->to_form_as_condition(env));
      e.body->inline_forms(entry, env);
      list.push_back(pretty_print::build_list(entry));
    }
    return pretty_print::build_list(list);
  }
}

void CondNoElseElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& e : entries) {
    e.condition->apply(f);
    e.body->apply(f);
  }
}

void CondNoElseElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& e : entries) {
    e.condition->apply_form(f);
    e.body->apply_form(f);
  }
}

void CondNoElseElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    for (auto& e : entries) {
      e.condition->collect_vars(vars, recursive);
      e.body->collect_vars(vars, recursive);
    }
  }
}

void CondNoElseElement::get_modified_regs(RegSet& regs) const {
  for (auto& e : entries) {
    e.condition->get_modified_regs(regs);
    e.body->get_modified_regs(regs);
  }
}

CaseElement::CaseElement(Form* value, const std::vector<Entry>& entries, Form* else_body)
    : m_value(value), m_entries(entries), m_else_body(else_body) {
  m_value->parent_element = this;
  for (auto& entry : m_entries) {
    for (auto& val : entry.vals) {
      val->parent_element = this;
    }
    entry.body->parent_element = this;
  }
  if (m_else_body) {
    m_else_body->parent_element = this;
  }
}

goos::Object CaseElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> list;
  list.push_back(pretty_print::to_symbol("case"));
  list.push_back(m_value->to_form(env));
  for (auto& e : m_entries) {
    std::vector<goos::Object> entry;

    // cases
    std::vector<goos::Object> cases;
    for (auto& val : e.vals) {
      cases.push_back(val->to_form(env));
    }
    entry.push_back(pretty_print::build_list(cases));

    // body
    e.body->inline_forms(entry, env);
    list.push_back(pretty_print::build_list(entry));
  }

  if (m_else_body) {
    std::vector<goos::Object> entry;
    entry.push_back(pretty_print::to_symbol("else"));
    m_else_body->inline_forms(entry, env);
    list.push_back(pretty_print::build_list(entry));
  }
  return pretty_print::build_list(list);
}

void CaseElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_value->apply(f);
  for (auto& e : m_entries) {
    for (auto& val : e.vals) {
      val->apply(f);
    }
    e.body->apply(f);
  }

  if (m_else_body) {
    m_else_body->apply(f);
  }
}

void CaseElement::apply_form(const std::function<void(Form*)>& f) {
  m_value->apply_form(f);
  for (auto& e : m_entries) {
    for (auto& val : e.vals) {
      val->apply_form(f);
    }
    e.body->apply_form(f);
  }

  if (m_else_body) {
    m_else_body->apply_form(f);
  }
}

void CaseElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_value->collect_vars(vars, recursive);
    for (auto& e : m_entries) {
      for (auto& val : e.vals) {
        val->collect_vars(vars, recursive);
      }
      e.body->collect_vars(vars, recursive);
    }

    if (m_else_body) {
      m_else_body->collect_vars(vars, recursive);
    }
  }
}

void CaseElement::get_modified_regs(RegSet& regs) const {
  m_value->get_modified_regs(regs);
  for (auto& e : m_entries) {
    for (auto& val : e.vals) {
      val->get_modified_regs(regs);
    }
    e.body->get_modified_regs(regs);
  }

  if (m_else_body) {
    m_else_body->get_modified_regs(regs);
  }
}

/////////////////////////////
// AbsElement
/////////////////////////////

AbsElement::AbsElement(RegisterAccess _source, RegSet _consumed)
    : source(_source), consumed(std::move(_consumed)) {}

goos::Object AbsElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list("abs", source.to_form(env));
}

void AbsElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AbsElement::apply_form(const std::function<void(Form*)>&) {}

void AbsElement::collect_vars(RegAccessSet& vars, bool) const {
  vars.insert(source);
}

void AbsElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// AshElement
/////////////////////////////

AshElement::AshElement(RegisterAccess _shift_amount,
                       RegisterAccess _value,
                       std::optional<RegisterAccess> _clobber,
                       bool _is_signed,
                       RegSet _consumed)
    : shift_amount(_shift_amount),
      value(_value),
      clobber(_clobber),
      is_signed(_is_signed),
      consumed(_consumed) {}

goos::Object AshElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(pretty_print::to_symbol(is_signed ? "ash.si" : "ash.ui"),
                                  value.to_form(env), shift_amount.to_form(env));
}

void AshElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AshElement::apply_form(const std::function<void(Form*)>&) {}

void AshElement::collect_vars(RegAccessSet& vars, bool) const {
  vars.insert(value);
  vars.insert(shift_amount);
}

void AshElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// TypeOfElement
/////////////////////////////

TypeOfElement::TypeOfElement(Form* _value) : value(_value) {
  value->parent_element = this;
}

goos::Object TypeOfElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list("rtype-of", value->to_form(env));
}

void TypeOfElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  value->apply(f);
}

void TypeOfElement::apply_form(const std::function<void(Form*)>& f) {
  value->apply_form(f);
}

void TypeOfElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    value->collect_vars(vars, recursive);
  }
}

void TypeOfElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// ConditionalMoveFalseElement
/////////////////////////////

ConditionalMoveFalseElement::ConditionalMoveFalseElement(RegisterAccess _dest,
                                                         RegisterAccess _old_value,
                                                         RegisterAccess _source,
                                                         bool _on_zero)
    : dest(_dest), old_value(_old_value), source(_source), on_zero(_on_zero) {}

goos::Object ConditionalMoveFalseElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(on_zero ? "cmove-#f-zero" : "cmove-#f-nonzero", dest.to_form(env),
                                  source.to_form(env), old_value.to_form(env));
}

void ConditionalMoveFalseElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void ConditionalMoveFalseElement::apply_form(const std::function<void(Form*)>&) {}

void ConditionalMoveFalseElement::collect_vars(RegAccessSet& vars, bool) const {
  vars.insert(dest);
  vars.insert(old_value);
  vars.insert(source);
}

void ConditionalMoveFalseElement::get_modified_regs(RegSet& regs) const {
  regs.insert(dest.reg());
}

/////////////////////////////
// GenericElement
/////////////////////////////

GenericOperator GenericOperator::make_fixed(FixedOperatorKind kind) {
  GenericOperator op;
  op.m_kind = Kind::FIXED_OPERATOR;
  op.m_fixed_kind = kind;
  return op;
}

GenericOperator GenericOperator::make_function(Form* value) {
  GenericOperator op;
  op.m_kind = Kind::FUNCTION_EXPR;
  op.m_function = value;
  return op;
}

GenericOperator GenericOperator::make_compare(IR2_Condition::Kind kind) {
  GenericOperator op;
  op.m_kind = Kind::CONDITION_OPERATOR;
  op.m_condition_kind = kind;
  return op;
}

void GenericOperator::collect_vars(RegAccessSet& vars, bool recursive) const {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
    case Kind::CONDITION_OPERATOR:
      return;
    case Kind::FUNCTION_EXPR:
      if (recursive) {
        m_function->collect_vars(vars, recursive);
      }
      return;
    default:
      ASSERT(false);
  }
}

goos::Object GenericOperator::to_form(const Env& env) const {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
      return pretty_print::to_symbol(fixed_operator_to_string(m_fixed_kind));
    case Kind::CONDITION_OPERATOR:
      return pretty_print::to_symbol(get_condition_kind_name(m_condition_kind));
    case Kind::FUNCTION_EXPR:
      return m_function->to_form(env);
    default:
      ASSERT(false);
      return {};
  }
}

void GenericOperator::apply(const std::function<void(FormElement*)>& f) {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
    case Kind::CONDITION_OPERATOR:
      break;
    case Kind::FUNCTION_EXPR:
      m_function->apply(f);
      break;
    default:
      ASSERT(false);
  }
}

void GenericOperator::apply_form(const std::function<void(Form*)>& f) {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
    case Kind::CONDITION_OPERATOR:
      break;
    case Kind::FUNCTION_EXPR:
      m_function->apply_form(f);
      break;
    default:
      ASSERT(false);
  }
}

bool GenericOperator::operator==(const GenericOperator& other) const {
  if (m_kind != other.m_kind) {
    return false;
  }
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
      return m_fixed_kind == other.m_fixed_kind;
    case Kind::CONDITION_OPERATOR:
      return m_condition_kind == other.m_condition_kind;
    case Kind::FUNCTION_EXPR:
      return false;
    default:
      ASSERT(false);
      return false;
  }
}

bool GenericOperator::operator!=(const GenericOperator& other) const {
  return !((*this) == other);
}

void GenericOperator::get_modified_regs(RegSet& regs) const {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
    case Kind::CONDITION_OPERATOR:
      break;
    case Kind::FUNCTION_EXPR:
      m_function->get_modified_regs(regs);
      break;
    default:
      ASSERT(false);
  }
}

std::string fixed_operator_to_string(FixedOperatorKind kind) {
  switch (kind) {
    case FixedOperatorKind::GPR_TO_FPR:
      return "gpr->fpr";
    case FixedOperatorKind::DIVISION:
      return "/";
    case FixedOperatorKind::ADDITION:
      return "+";
    case FixedOperatorKind::ADDITION_IN_PLACE:
      return "+!";
    case FixedOperatorKind::ADDITION_PTR:
      return "&+";
    case FixedOperatorKind::ADDITION_PTR_IN_PLACE:
      return "&+!";
    case FixedOperatorKind::SUBTRACTION:
      return "-";
    case FixedOperatorKind::SUBTRACTION_PTR:
      return "&-";
    case FixedOperatorKind::MULTIPLICATION:
      return "*";
    case FixedOperatorKind::SQRT:
      return "sqrtf";
    case FixedOperatorKind::ARITH_SHIFT:
      return "ash";
    case FixedOperatorKind::MOD:
      return "mod";
    case FixedOperatorKind::ABS:
      return "abs";
    case FixedOperatorKind::MIN:
      return "min";
    case FixedOperatorKind::MAX:
      return "max";
    case FixedOperatorKind::FABS:
      return "fabs";
    case FixedOperatorKind::FMIN:
      return "fmin";
    case FixedOperatorKind::FMAX:
      return "fmax";
    case FixedOperatorKind::LOGAND:
      return "logand";
    case FixedOperatorKind::LOGAND_IN_PLACE:
      return "logand!";
    case FixedOperatorKind::LOGIOR:
      return "logior";
    case FixedOperatorKind::LOGIOR_IN_PLACE:
      return "logior!";
    case FixedOperatorKind::LOGXOR:
      return "logxor";
    case FixedOperatorKind::LOGXOR_IN_PLACE:
      return "logxor!";
    case FixedOperatorKind::LOGNOR:
      return "lognor";
    case FixedOperatorKind::LOGNOT:
      return "lognot";
    case FixedOperatorKind::LOGCLEAR:
      return "logclear";
    case FixedOperatorKind::LOGCLEAR_IN_PLACE:
      return "logclear!";
    case FixedOperatorKind::LOGTEST:
      return "logtest?";
    case FixedOperatorKind::LOGTESTA:
      return "logtesta?";
    case FixedOperatorKind::SHL:
      return "shl";
    case FixedOperatorKind::SHR:
      return "shr";
    case FixedOperatorKind::SAR:
      return "sar";
    case FixedOperatorKind::CAR:
      return "car";
    case FixedOperatorKind::CDR:
      return "cdr";
    case FixedOperatorKind::NEW:
      return "new";
    case FixedOperatorKind::OBJECT_NEW:
      return "object-new";
    case FixedOperatorKind::TYPE_NEW:
      return "type-new";
    case FixedOperatorKind::CONS:
      return "cons";
    case FixedOperatorKind::LT:
      return "<";
    case FixedOperatorKind::GT:
      return ">";
    case FixedOperatorKind::LEQ:
      return "<=";
    case FixedOperatorKind::GEQ:
      return ">=";
    case FixedOperatorKind::EQ:
      return "=";
    case FixedOperatorKind::NEQ:
      return "!=";
    case FixedOperatorKind::METHOD_OF_OBJECT:
      return "method-of-object";
    case FixedOperatorKind::METHOD_OF_TYPE:
      return "method-of-type";
    case FixedOperatorKind::NULLP:
      return "null?";
    case FixedOperatorKind::PAIRP:
      return "pair?";
    case FixedOperatorKind::NONE:
      return "none";
    case FixedOperatorKind::PCPYLD:
      return "make-u128";
    case FixedOperatorKind::SYMBOL_TO_STRING:
      return "symbol->string";
    case FixedOperatorKind::ADDRESS_OF:
      return "&";
    case FixedOperatorKind::ASM_SLLV_R0:
      return ".asm.sllv.r0";
    case FixedOperatorKind::ASM_MADDS:
      return ".asm.madd.s";
    case FixedOperatorKind::VECTOR_MINUS:
      return "vector-!";
    case FixedOperatorKind::VECTOR_PLUS:
      return "vector+!";
    case FixedOperatorKind::VECTOR_CROSS:
      return "vector-cross!";
    case FixedOperatorKind::VECTOR_FLOAT_PRODUCT:
      return "vector-float*!";
    case FixedOperatorKind::L32_NOT_FALSE_CBOOL:
      return "l32-false-check";
    case FixedOperatorKind::VECTOR_3_DOT:
      return "vector-dot";
    case FixedOperatorKind::VECTOR_4_DOT:
      return "vector4-dot";
    case FixedOperatorKind::PROCESS_TO_PPOINTER:
      return "process->ppointer";
    case FixedOperatorKind::PPOINTER_TO_HANDLE:
      return "ppointer->handle";
    case FixedOperatorKind::PROCESS_TO_HANDLE:
      return "process->handle";
    case FixedOperatorKind::PPOINTER_TO_PROCESS:
      return "ppointer->process";
    case FixedOperatorKind::SEND_EVENT:
      return "send-event";
    case FixedOperatorKind::CPAD_PRESSED_P:
      return "cpad-pressed?";
    case FixedOperatorKind::CPAD_HOLD_P:
      return "cpad-hold?";
    case FixedOperatorKind::MOUSE_PRESSED_P:
      return "mouse-pressed?";
    case FixedOperatorKind::MOUSE_HOLD_P:
      return "mouse-hold?";
    case FixedOperatorKind::VECTOR_LENGTH:
      return "vector-length";
    case FixedOperatorKind::VECTOR_PLUS_FLOAT_TIMES:
      return "vector+float*!";
    case FixedOperatorKind::FOCUS_TEST:
      return "focus-test?";
    default:
      ASSERT(false);
      return "";
  }
}

GenericElement::GenericElement(GenericOperator op) : m_head(op) {
  if (op.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
    op.m_function->parent_element = this;
  }
}

GenericElement::GenericElement(GenericOperator op, Form* arg) : m_head(op), m_elts({arg}) {
  if (op.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
    op.m_function->parent_element = this;
  }
  for (auto x : m_elts) {
    x->parent_element = this;
  }
}

GenericElement::GenericElement(GenericOperator op, Form* arg0, Form* arg1)
    : m_head(op), m_elts({arg0, arg1}) {
  if (op.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
    op.m_function->parent_element = this;
  }
  for (auto x : m_elts) {
    x->parent_element = this;
  }
}

GenericElement::GenericElement(GenericOperator op, std::vector<Form*> forms)
    : m_head(op), m_elts(std::move(forms)) {
  if (op.kind() == GenericOperator::Kind::FUNCTION_EXPR) {
    op.m_function->parent_element = this;
  }
  for (auto x : m_elts) {
    x->parent_element = this;
  }
}

goos::Object GenericElement::to_form_internal(const Env& env) const {
  if (m_head.kind() == GenericOperator::Kind::CONDITION_OPERATOR &&
      m_head.condition_kind() == IR2_Condition::Kind::TRUTHY) {
    ASSERT(m_elts.size() == 1);
    return m_elts.front()->to_form_as_condition(env);
  } else if (m_head.kind() == GenericOperator::Kind::CONDITION_OPERATOR &&
             m_head.condition_kind() == IR2_Condition::Kind::ALWAYS) {
    return pretty_print::to_symbol("#t");
  } else {
    std::vector<goos::Object> result;
    result.push_back(m_head.to_form(env));
    for (auto x : m_elts) {
      ASSERT(x->parent_element);
      result.push_back(x->to_form(env));
    }
    return pretty_print::build_list(result);
  }
}

void GenericElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_head.apply(f);
  for (auto x : m_elts) {
    x->apply(f);
  }
}

void GenericElement::apply_form(const std::function<void(Form*)>& f) {
  m_head.apply_form(f);
  for (auto x : m_elts) {
    x->apply_form(f);
  }
}

void GenericElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_head.collect_vars(vars, recursive);
    for (auto x : m_elts) {
      x->collect_vars(vars, recursive);
    }
  }
}

void GenericElement::get_modified_regs(RegSet& regs) const {
  m_head.get_modified_regs(regs);
  for (auto x : m_elts) {
    x->get_modified_regs(regs);
  }
}

/////////////////////////////
// CastElement
/////////////////////////////

CastElement::CastElement(TypeSpec type, Form* source, bool numeric)
    : m_type(std::move(type)), m_source(source), m_numeric(numeric) {
  m_source->parent_element = this;
}

goos::Object CastElement::to_form_internal(const Env& env) const {
  // ASSERT(m_source->parent_element == this);
  auto atom = form_as_atom(m_source);
  if (atom && atom->is_var()) {
    return pretty_print::build_list(
        m_numeric ? "the" : "the-as", m_type.print(),
        atom->var().to_form(env, RegisterAccess::Print::AS_VARIABLE_NO_CAST));
  }
  return pretty_print::build_list(m_numeric ? "the" : "the-as", m_type.print(),
                                  m_source->to_form(env));
}

void CastElement::apply(const std::function<void(FormElement*)>& f) {
  // ASSERT(m_source->parent_element == this);
  f(this);
  m_source->apply(f);
}

void CastElement::apply_form(const std::function<void(Form*)>& f) {
  // ASSERT(m_source->parent_element == this);
  m_source->apply_form(f);
}

void CastElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  // ASSERT(m_source->parent_element == this);
  if (recursive) {
    m_source->collect_vars(vars, recursive);
  }
}

void CastElement::get_modified_regs(RegSet& regs) const {
  ASSERT(m_source->parent_element == this);
  m_source->get_modified_regs(regs);
}

/////////////////////////////
// DerefElement
/////////////////////////////

DerefToken DerefToken::make_int_constant(s64 int_constant) {
  DerefToken x;
  x.m_kind = Kind::INTEGER_CONSTANT;
  x.m_int_constant = int_constant;
  return x;
}

DerefToken DerefToken::make_int_expr(Form* expr) {
  DerefToken x;
  x.m_kind = Kind::INTEGER_EXPRESSION;
  x.m_expr = expr;
  return x;
}

DerefToken DerefToken::make_field_name(const std::string& name) {
  DerefToken x;
  x.m_kind = Kind::FIELD_NAME;
  x.m_name = name;
  return x;
}

DerefToken DerefToken::make_expr_placeholder() {
  DerefToken x;
  x.m_kind = Kind::EXPRESSION_PLACEHOLDER;
  return x;
}

void DerefToken::collect_vars(RegAccessSet& vars, bool recursive) const {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
    case Kind::EXPRESSION_PLACEHOLDER:
      break;
    case Kind::INTEGER_EXPRESSION:
      if (recursive) {
        m_expr->collect_vars(vars, recursive);
      }
      break;
    default:
      ASSERT(false);
  }
}

goos::Object DerefToken::to_form(const Env& env) const {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
      return pretty_print::to_symbol(fmt::format("{}", m_int_constant));
    case Kind::INTEGER_EXPRESSION:
      return m_expr->to_form(env);
    case Kind::FIELD_NAME:
      return pretty_print::to_symbol(m_name);
    case Kind::EXPRESSION_PLACEHOLDER:
      return pretty_print::to_symbol("PLACEHOLDER");
    default:
      ASSERT(false);
      return {};
  }
}

void DerefToken::apply(const std::function<void(FormElement*)>& f) {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
    case Kind::EXPRESSION_PLACEHOLDER:
      break;
    case Kind::INTEGER_EXPRESSION:
      m_expr->apply(f);
      break;
    default:
      ASSERT(false);
  }
}

void DerefToken::apply_form(const std::function<void(Form*)>& f) {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
    case Kind::EXPRESSION_PLACEHOLDER:
      break;
    case Kind::INTEGER_EXPRESSION:
      m_expr->apply_form(f);
      break;
    default:
      ASSERT(false);
  }
}

void DerefToken::get_modified_regs(RegSet& regs) const {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
    case Kind::EXPRESSION_PLACEHOLDER:
      break;
    case Kind::INTEGER_EXPRESSION:
      m_expr->get_modified_regs(regs);
      break;
    default:
      ASSERT(false);
  }
}

DerefToken to_token(const FieldReverseLookupOutput::Token& in) {
  switch (in.kind) {
    case FieldReverseLookupOutput::Token::Kind::FIELD:
      return DerefToken::make_field_name(in.name);
    case FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX:
      return DerefToken::make_int_constant(in.idx);
    case FieldReverseLookupOutput::Token::Kind::VAR_IDX:
      return DerefToken::make_expr_placeholder();
    default:
      // temp
      throw std::runtime_error("Cannot convert rd lookup token to deref token");
  }
}

DerefElement::DerefElement(Form* base, bool is_addr_of, DerefToken token)
    : m_base(base), m_is_addr_of(is_addr_of), m_tokens({std::move(token)}) {
  m_base->parent_element = this;
  for (auto& x : m_tokens) {
    if (x.kind() == DerefToken::Kind::INTEGER_EXPRESSION) {
      x.expr()->parent_element = this;
    }
  }
  inline_nested();
}

DerefElement::DerefElement(Form* base, bool is_addr_of, std::vector<DerefToken> tokens)
    : m_base(base), m_is_addr_of(is_addr_of), m_tokens(std::move(tokens)) {
  m_base->parent_element = this;
  for (auto& x : m_tokens) {
    if (x.kind() == DerefToken::Kind::INTEGER_EXPRESSION) {
      x.expr()->parent_element = this;
    }
  }
  inline_nested();
}

goos::Object DerefElement::to_form_internal(const Env& env) const {
  ASSERT(m_base->parent_element);
  std::vector<goos::Object> forms = {pretty_print::to_symbol(m_is_addr_of ? "&->" : "->"),
                                     m_base->to_form(env)};
  for (auto& tok : m_tokens) {
    forms.push_back(tok.to_form(env));
  }
  return pretty_print::build_list(forms);
}

void DerefElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_base->apply(f);
  for (auto& tok : m_tokens) {
    tok.apply(f);
  }
}

void DerefElement::apply_form(const std::function<void(Form*)>& f) {
  m_base->apply_form(f);
  for (auto& tok : m_tokens) {
    tok.apply_form(f);
  }
}

void DerefElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_base->collect_vars(vars, recursive);
    for (auto& tok : m_tokens) {
      tok.collect_vars(vars, recursive);
    }
  }
}

void DerefElement::get_modified_regs(RegSet& regs) const {
  m_base->get_modified_regs(regs);
  for (auto& tok : m_tokens) {
    tok.get_modified_regs(regs);
  }
}

void DerefElement::set_base(Form* new_base) {
  m_base = new_base;
  m_base->parent_element = this;
}

/////////////////////////////
// DynamicMethodAccess
/////////////////////////////

DynamicMethodAccess::DynamicMethodAccess(RegisterAccess source) : m_source(source) {}

goos::Object DynamicMethodAccess::to_form_internal(const Env& env) const {
  return pretty_print::build_list("dyn-method-access", m_source.to_form(env));
}

void DynamicMethodAccess::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void DynamicMethodAccess::apply_form(const std::function<void(Form*)>&) {}

void DynamicMethodAccess::collect_vars(RegAccessSet& vars, bool) const {
  vars.insert(m_source);
}

void DynamicMethodAccess::get_modified_regs(RegSet&) const {}

/////////////////////////////
// ArrayFieldAccess
/////////////////////////////
ArrayFieldAccess::ArrayFieldAccess(RegisterAccess source,
                                   const std::vector<DerefToken>& deref_tokens,
                                   int expected_stride,
                                   int constant_offset,
                                   bool flipped)
    : m_source(source),
      m_deref_tokens(deref_tokens),
      m_expected_stride(expected_stride),
      m_constant_offset(constant_offset),
      m_flipped(flipped) {
  for (auto& token : m_deref_tokens) {
    if (token.kind() == DerefToken::Kind::INTEGER_EXPRESSION) {
      token.expr()->parent_element = this;
    }
  }
}

goos::Object ArrayFieldAccess::to_form_internal(const Env& env) const {
  std::vector<goos::Object> elts;
  elts.push_back(pretty_print::to_symbol("dynamic-array-field-access"));
  elts.push_back(m_source.to_form(env));
  for (auto& tok : m_deref_tokens) {
    elts.push_back(tok.to_form(env));
  }
  return pretty_print::build_list(elts);
}

void ArrayFieldAccess::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& tok : m_deref_tokens) {
    tok.apply(f);
  }
}

void ArrayFieldAccess::apply_form(const std::function<void(Form*)>& f) {
  for (auto& tok : m_deref_tokens) {
    tok.apply_form(f);
  }
}

void ArrayFieldAccess::collect_vars(RegAccessSet& vars, bool recursive) const {
  vars.insert(m_source);
  if (recursive) {
    for (auto& tok : m_deref_tokens) {
      tok.collect_vars(vars, recursive);
    }
  }
}

void ArrayFieldAccess::get_modified_regs(RegSet& regs) const {
  for (auto& tok : m_deref_tokens) {
    tok.get_modified_regs(regs);
  }
}

/////////////////////////////
// GetMethodElement
/////////////////////////////

GetMethodElement::GetMethodElement(Form* in, std::string name, bool is_object)
    : m_in(in), m_name(std::move(name)), m_is_object(is_object) {
  in->parent_element = this;
}

goos::Object GetMethodElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(m_is_object ? "method-of-object" : "method-of-type",
                                  m_in->to_form(env), m_name);
}

void GetMethodElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_in->apply(f);
}

void GetMethodElement::apply_form(const std::function<void(Form*)>& f) {
  m_in->apply_form(f);
}

void GetMethodElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_in->collect_vars(vars, recursive);
  }
}

void GetMethodElement::get_modified_regs(RegSet& regs) const {
  m_in->get_modified_regs(regs);
}

/////////////////////////////
// StringConstantElement
/////////////////////////////

StringConstantElement::StringConstantElement(const std::string& value) : m_value(value) {}

goos::Object StringConstantElement::to_form_internal(const Env&) const {
  return goos::StringObject::make_new(m_value);
}

void StringConstantElement::apply(const std::function<void(FormElement*)>&) {}
void StringConstantElement::apply_form(const std::function<void(Form*)>&) {}
void StringConstantElement::collect_vars(RegAccessSet&, bool) const {}
void StringConstantElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// ConstantTokenElement
/////////////////////////////
ConstantTokenElement::ConstantTokenElement(const std::string& value) : m_value(value) {}

goos::Object ConstantTokenElement::to_form_internal(const Env&) const {
  return pretty_print::to_symbol(m_value);
}

void ConstantTokenElement::apply(const std::function<void(FormElement*)>&) {}
void ConstantTokenElement::apply_form(const std::function<void(Form*)>&) {}
void ConstantTokenElement::collect_vars(RegAccessSet&, bool) const {}
void ConstantTokenElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// ConstantFloatElement
/////////////////////////////

ConstantFloatElement::ConstantFloatElement(float value) : m_value(value) {}

void ConstantFloatElement::apply(const std::function<void(FormElement*)>&) {}
void ConstantFloatElement::apply_form(const std::function<void(Form*)>&) {}
void ConstantFloatElement::collect_vars(RegAccessSet&, bool) const {}
void ConstantFloatElement::get_modified_regs(RegSet&) const {}

goos::Object ConstantFloatElement::to_form_internal(const Env&) const {
  return pretty_print::float_representation(m_value);
}

/////////////////////////////
// StorePlainDeref
/////////////////////////////

StorePlainDeref::StorePlainDeref(Form* dst,
                                 SimpleExpression expr,
                                 int my_idx,
                                 RegisterAccess base_var,
                                 std::optional<TypeSpec> dst_cast_type,
                                 std::optional<TypeSpec> src_cast_type,
                                 int size)
    : m_dst(dst),
      m_expr(std::move(expr)),
      m_my_idx(my_idx),
      m_base_var(base_var),
      m_dst_cast_type(std::move(dst_cast_type)),
      m_src_cast_type(std::move(src_cast_type)),
      m_size(size) {
  m_dst->parent_element = this;
}

goos::Object StorePlainDeref::to_form_internal(const Env& env) const {
  std::vector<goos::Object> lst = {pretty_print::to_symbol("set!")};

  if (m_dst_cast_type) {
    lst.push_back(
        pretty_print::build_list("the-as", m_dst_cast_type->print(), m_dst->to_form(env)));
  } else {
    lst.push_back(m_dst->to_form(env));
  }

  if (m_src_cast_type) {
    lst.push_back(pretty_print::build_list("the-as", m_src_cast_type->print(),
                                           m_expr.to_form(env.file->labels, env)));
  } else {
    lst.push_back(m_expr.to_form(env.file->labels, env));
  }

  return pretty_print::build_list(lst);
}

void StorePlainDeref::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_dst->apply(f);
}

void StorePlainDeref::apply_form(const std::function<void(Form*)>& f) {
  m_dst->apply_form(f);
}

void StorePlainDeref::collect_vars(RegAccessSet& vars, bool recursive) const {
  m_expr.collect_vars(vars);
  m_dst->collect_vars(vars, recursive);
}

void StorePlainDeref::get_modified_regs(RegSet& regs) const {
  m_dst->get_modified_regs(regs);
}

/////////////////////////////
// StoreArrayAccess
/////////////////////////////

StoreArrayAccess::StoreArrayAccess(ArrayFieldAccess* dst,
                                   SimpleExpression expr,
                                   int my_idx,
                                   RegisterAccess array_src,
                                   std::optional<TypeSpec> src_cast_type)
    : m_dst(dst),
      m_expr(expr),
      m_my_idx(my_idx),
      m_base_var(array_src),
      m_src_cast_type(src_cast_type) {}

goos::Object StoreArrayAccess::to_form_internal(const Env& env) const {
  return pretty_print::build_list("set!", m_dst->to_form(env),
                                  m_expr.to_form(env.file->labels, env));
}

void StoreArrayAccess::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_dst->apply(f);
}

void StoreArrayAccess::apply_form(const std::function<void(Form*)>& f) {
  m_dst->apply_form(f);
}

void StoreArrayAccess::collect_vars(RegAccessSet& vars, bool recursive) const {
  m_expr.collect_vars(vars);
  m_dst->collect_vars(vars, recursive);
}

void StoreArrayAccess::get_modified_regs(RegSet& regs) const {
  m_dst->get_modified_regs(regs);
}

/////////////////////////////
// DecompiledDataElement
/////////////////////////////

DecompiledDataElement::DecompiledDataElement(const DecompilerLabel& label,
                                             const std::optional<LabelInfo>& label_info)
    : m_label(label), m_label_info(label_info) {}

goos::Object DecompiledDataElement::to_form_internal(const Env&) const {
  if (m_decompiled) {
    return m_description;
  } else {
    return pretty_print::to_symbol(fmt::format("<static-data {}>", m_label.name));
  }
}

void DecompiledDataElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void DecompiledDataElement::apply_form(const std::function<void(Form*)>&) {}

void DecompiledDataElement::collect_vars(RegAccessSet&, bool) const {}

void DecompiledDataElement::get_modified_regs(RegSet&) const {}

void DecompiledDataElement::do_decomp(const Env& env, const LinkedObjectFile* file) {
  if (m_label_info) {
    m_description =
        decompile_at_label_with_hint(*m_label_info, m_label, env.file->labels,
                                     env.file->words_by_seg, env.dts->ts, file, env.version);
  } else {
    m_description = decompile_at_label_guess_type(m_label, env.file->labels, env.file->words_by_seg,
                                                  env.dts->ts, file, env.version);
  }
  m_decompiled = true;
}

/////////////////////////////
// LetElement
/////////////////////////////

LetElement::LetElement(Form* body, bool star) : m_body(body), m_star(star) {
  m_body->parent_element = this;
}

void LetElement::add_def(RegisterAccess dst, Form* value) {
  value->parent_element = this;
  m_entries.push_back({dst, value});
}

void LetElement::make_let_star() {
  m_star = true;
}

void LetElement::clear_let_star() {
  m_star = false;
}

goos::Object LetElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> outer = {pretty_print::to_symbol(m_star ? "let*" : "let")};

  std::vector<goos::Object> def_list;

  for (auto& entry : m_entries) {
    def_list.push_back(pretty_print::build_list(
        entry.dest.to_form(env, RegisterAccess::Print::AS_VARIABLE_NO_CAST),
        entry.src->to_form(env)));
  }

  outer.push_back(pretty_print::build_list(def_list));
  m_body->inline_forms(outer, env);
  return pretty_print::build_list(outer);
}

void LetElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& entry : m_entries) {
    entry.src->apply(f);
  }
  m_body->apply(f);
}

void LetElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& entry : m_entries) {
    entry.src->apply_form(f);
  }
  m_body->apply_form(f);
}

void LetElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  for (auto& entry : m_entries) {
    vars.insert(entry.dest);
    if (recursive) {
      entry.src->collect_vars(vars, recursive);
    }
  }
  m_body->collect_vars(vars, recursive);
}

void LetElement::get_modified_regs(RegSet& regs) const {
  for (auto& entry : m_entries) {
    regs.insert(entry.dest.reg());
  }
  m_body->get_modified_regs(regs);
}

void LetElement::add_entry(const Entry& e) {
  e.src->parent_element = this;
  m_entries.push_back(e);
}

void LetElement::set_body(Form* new_body) {
  m_body = new_body;
  m_body->parent_element = this;
}

/////////////////////////////
// CounterLoopElement
/////////////////////////////

CounterLoopElement::CounterLoopElement(Kind kind,
                                       RegisterAccess var_init,
                                       RegisterAccess var_check,
                                       RegisterAccess var_inc,
                                       Form* check_value,
                                       Form* body)
    : m_var_init(var_init),
      m_var_check(var_check),
      m_var_inc(var_inc),
      m_check_value(check_value),
      m_body(body),
      m_kind(kind) {
  m_body->parent_element = this;
  m_check_value->parent_element = this;
  ASSERT(m_var_inc.reg() == m_var_check.reg());
  ASSERT(m_var_init.reg() == m_var_inc.reg());
}

goos::Object CounterLoopElement::to_form_internal(const Env& env) const {
  std::string loop_name;
  switch (m_kind) {
    case Kind::DOTIMES:
      loop_name = "dotimes";
      break;
    case Kind::COUNTDOWN:
      loop_name = "countdown";
      break;
    default:
      ASSERT(false);
  }
  std::vector<goos::Object> outer = {
      pretty_print::to_symbol(loop_name),
      pretty_print::build_list(m_var_init.to_form(env), m_check_value->to_form(env))};
  m_body->inline_forms(outer, env);
  return pretty_print::build_list(outer);
}

void CounterLoopElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_check_value->apply(f);
  m_body->apply(f);
}

void CounterLoopElement::apply_form(const std::function<void(Form*)>& f) {
  m_check_value->apply_form(f);
  m_body->apply_form(f);
}

void CounterLoopElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  vars.insert(m_var_init);
  vars.insert(m_var_check);
  vars.insert(m_var_inc);
  if (recursive) {
    m_body->collect_vars(vars, recursive);
    m_check_value->collect_vars(vars, recursive);
  }
}

void CounterLoopElement::get_modified_regs(RegSet& regs) const {
  regs.insert(m_var_inc.reg());
  m_body->get_modified_regs(regs);
  m_check_value->get_modified_regs(regs);
}

/////////////////////////////
// LambdaDefinitionElement
/////////////////////////////

LambdaDefinitionElement::LambdaDefinitionElement(const goos::Object& def) : m_def(def) {}

goos::Object LambdaDefinitionElement::to_form_internal(const Env&) const {
  return m_def;
}

void LambdaDefinitionElement::apply_form(const std::function<void(Form*)>&) {}

void LambdaDefinitionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void LambdaDefinitionElement::collect_vars(RegAccessSet&, bool) const {}

void LambdaDefinitionElement::get_modified_regs(RegSet&) const {}

/////////////////////////////
// StackVarDefElement
/////////////////////////////

StackStructureDefElement::StackStructureDefElement(const StackStructureEntry& entry)
    : m_entry(entry) {}

goos::Object StackStructureDefElement::to_form_internal(const Env&) const {
  switch (m_entry.hint.container_type) {
    case StackStructureHint::ContainerType::NONE:
      return pretty_print::build_list(
          fmt::format("new 'stack-no-clear '{}", m_entry.ref_type.print()));
    case StackStructureHint::ContainerType::INLINE_ARRAY:
      return pretty_print::build_list(fmt::format("new 'stack-no-clear 'inline-array '{} {}",
                                                  m_entry.ref_type.get_single_arg().print(),
                                                  m_entry.hint.container_size));
    case StackStructureHint::ContainerType::ARRAY:
      return pretty_print::build_list(fmt::format("new 'stack-no-clear 'array '{} {}",
                                                  m_entry.ref_type.get_single_arg().print(),
                                                  m_entry.hint.container_size));
    default:
      ASSERT(false);
  }
}

void StackStructureDefElement::apply_form(const std::function<void(Form*)>&) {}

void StackStructureDefElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StackStructureDefElement::collect_vars(RegAccessSet&, bool) const {}

void StackStructureDefElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// VectorFloatLoadStoreElement
////////////////////////////////

VectorFloatLoadStoreElement::VectorFloatLoadStoreElement(Register vf_reg,
                                                         Form* location,
                                                         bool is_load,
                                                         int my_idx)
    : m_vf_reg(vf_reg), m_location(location), m_is_load(is_load), m_my_idx(my_idx) {
  location->parent_element = this;
}

goos::Object VectorFloatLoadStoreElement::to_form_internal(const Env& env) const {
  if (m_is_load) {
    return pretty_print::build_list(".lvf", pretty_print::to_symbol(m_vf_reg.to_charp()),
                                    m_location->to_form(env));
  } else {
    return pretty_print::build_list(".svf", m_location->to_form(env),
                                    pretty_print::to_symbol(m_vf_reg.to_charp()));
  }
}

void VectorFloatLoadStoreElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_location->apply(f);
}

void VectorFloatLoadStoreElement::apply_form(const std::function<void(Form*)>& f) {
  m_location->apply_form(f);
}

void VectorFloatLoadStoreElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_location->collect_vars(vars, recursive);
  }
}

void VectorFloatLoadStoreElement::get_modified_regs(RegSet&) const {
  // vf's dont count
}

void VectorFloatLoadStoreElement::collect_vf_regs(RegSet& regs) const {
  regs.insert(m_vf_reg);
}

////////////////////////////////
// StackSpillStoreElement
////////////////////////////////

StackSpillStoreElement::StackSpillStoreElement(SimpleAtom value,
                                               int size,
                                               int stack_offset,
                                               const std::optional<TypeSpec>& cast_type)
    : m_value(value), m_size(size), m_stack_offset(stack_offset), m_cast_type(cast_type) {}

goos::Object StackSpillStoreElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(
      fmt::format("set! {}", env.get_spill_slot_var_name(m_stack_offset)), m_value.to_form(env));
}

void StackSpillStoreElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StackSpillStoreElement::apply_form(const std::function<void(Form*)>&) {}

void StackSpillStoreElement::collect_vars(RegAccessSet& vars, bool) const {
  if (m_value.is_var()) {
    vars.insert(m_value.var());
  }
}

void StackSpillStoreElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// StackSpillValueElement
////////////////////////////////

StackSpillValueElement::StackSpillValueElement(int size, int stack_offset, bool is_signed)
    : m_size(size), m_stack_offset(stack_offset), m_is_signed(is_signed) {}

goos::Object StackSpillValueElement::to_form_internal(const Env& env) const {
  return pretty_print::to_symbol(env.get_spill_slot_var_name(m_stack_offset));
}

void StackSpillValueElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StackSpillValueElement::apply_form(const std::function<void(Form*)>&) {}
void StackSpillValueElement::collect_vars(RegAccessSet&, bool) const {}
void StackSpillValueElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// MethodOfTypeElement
///////////////////////////////

MethodOfTypeElement::MethodOfTypeElement(RegisterAccess type_reg,
                                         const TypeSpec& type_at_decompile,
                                         const MethodInfo& method_info)
    : m_type_reg(type_reg), m_type_at_decompile(type_at_decompile), m_method_info(method_info) {}

goos::Object MethodOfTypeElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list("method-of-type", m_type_reg.to_form(env), m_method_info.name);
}

void MethodOfTypeElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void MethodOfTypeElement::apply_form(const std::function<void(Form*)>&) {}

void MethodOfTypeElement::collect_vars(RegAccessSet& vars, bool) const {
  vars.insert(m_type_reg);
}

void MethodOfTypeElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// LabelElement
///////////////////////////////

LabelElement::LabelElement(int lid) : m_lid(lid) {}

goos::Object LabelElement::to_form_internal(const Env&) const {
  return pretty_print::build_list(fmt::format("label cfg-{}", m_lid));
}

void LabelElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void LabelElement::apply_form(const std::function<void(Form*)>&) {}
void LabelElement::collect_vars(RegAccessSet&, bool) const {}
void LabelElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// LabelDerefElement
///////////////////////////////

LabelDerefElement::LabelDerefElement(int lid,
                                     int size,
                                     LoadVarOp::Kind load_kind,
                                     RegisterAccess var)
    : m_lid(lid), m_size(size), m_load_kind(load_kind), m_var(var) {}

goos::Object LabelDerefElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(fmt::format("label-deref {} :label {} :size {} :kind {}",
                                              m_var.to_string(env), env.file->labels.at(m_lid).name,
                                              m_size, load_kind_to_string(m_load_kind)));
}

void LabelDerefElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void LabelDerefElement::apply_form(const std::function<void(Form*)>&) {}
void LabelDerefElement::collect_vars(RegAccessSet& regs, bool) const {
  regs.insert(m_var);
}
void LabelDerefElement::get_modified_regs(RegSet&) const {}

////////////////////////////////
// GetSymbolStringPointer
//////////////////////////////

GetSymbolStringPointer::GetSymbolStringPointer(Form* src) : m_src(src) {
  m_src->parent_element = this;
}

goos::Object GetSymbolStringPointer::to_form_internal(const Env& env) const {
  return pretty_print::build_list("sym->str-ptr", m_src->to_form(env));
}

void GetSymbolStringPointer::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_src->apply(f);
}

void GetSymbolStringPointer::apply_form(const std::function<void(Form*)>& f) {
  m_src->apply_form(f);
}

void GetSymbolStringPointer::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_src->collect_vars(vars, recursive);
  }
}

void GetSymbolStringPointer::get_modified_regs(RegSet& regs) const {
  return m_src->get_modified_regs(regs);
}

////////////////////////////////
// DefstateElement
////////////////////////////////

DefstateElement::DefstateElement(const std::string& process_type,
                                 const std::string& state_name,
                                 const std::vector<Entry>& entries,
                                 bool is_virtual,
                                 bool is_override)
    : m_process_type(process_type),
      m_state_name(state_name),
      m_entries(entries),
      m_is_virtual(is_virtual),
      m_is_override(is_override) {
  for (auto& e : m_entries) {
    e.val->parent_element = this;
  }
}

void DefstateElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& e : m_entries) {
    e.val->apply(f);
  }
}

void DefstateElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& e : m_entries) {
    e.val->apply_form(f);
  }
}

void DefstateElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    for (auto& e : m_entries) {
      e.val->collect_vars(vars, recursive);
    }
  }
}

void DefstateElement::get_modified_regs(RegSet& regs) const {
  for (auto& e : m_entries) {
    e.val->get_modified_regs(regs);
  }
}

goos::Object DefstateElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("defstate"));
  forms.push_back(pretty_print::to_symbol(m_state_name));
  forms.push_back(pretty_print::build_list(m_process_type));

  if (m_is_virtual) {
    if (!m_is_override) {
      forms.push_back(pretty_print::to_symbol(":virtual #t"));
    } else {
      forms.push_back(pretty_print::to_symbol(":virtual override"));
    }
  }

  for (const auto& e : m_entries) {
    forms.push_back(pretty_print::to_symbol(fmt::format(":{}", handler_kind_to_name(e.kind))));
    auto to_print = e.val;
    forms.push_back(to_print->to_form(env));
  }

  return pretty_print::build_list(forms);
}

////////////////////////////////
// DefskelgroupElement
////////////////////////////////

WithDmaBufferAddBucketElement::WithDmaBufferAddBucketElement(RegisterAccess dma_buf,
                                                             Form* dma_buf_val,
                                                             Form* bucket,
                                                             Form* body)
    : m_dma_buf(dma_buf), m_dma_buf_val(dma_buf_val), m_bucket(bucket), m_body(body) {
  m_dma_buf_val->parent_element = this;
  m_bucket->parent_element = this;
  m_body->parent_element = this;
}

void WithDmaBufferAddBucketElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_dma_buf_val->apply(f);
  m_bucket->apply(f);
  m_body->apply(f);
}

void WithDmaBufferAddBucketElement::apply_form(const std::function<void(Form*)>& f) {
  m_dma_buf_val->apply_form(f);
  m_bucket->apply_form(f);
  m_body->apply_form(f);
}

void WithDmaBufferAddBucketElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  m_dma_buf_val->collect_vars(vars, recursive);
  m_bucket->collect_vars(vars, recursive);
  m_body->collect_vars(vars, recursive);
}

void WithDmaBufferAddBucketElement::get_modified_regs(RegSet& regs) const {
  m_dma_buf_val->get_modified_regs(regs);
  m_bucket->get_modified_regs(regs);
  m_body->get_modified_regs(regs);
}

goos::Object WithDmaBufferAddBucketElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("with-dma-buffer-add-bucket"));
  forms.push_back(pretty_print::build_list(
      {pretty_print::build_list({pretty_print::to_symbol(env.get_variable_name(m_dma_buf)),
                                 m_dma_buf_val->to_form(env)}),
       m_bucket->to_form(env)}));
  m_body->inline_forms(forms, env);

  return pretty_print::build_list(forms);
}

////////////////////////////////
// DefskelgroupElement
////////////////////////////////

DefskelgroupElement::DefskelgroupElement(const std::string& name,
                                         const DefskelgroupElement::Info& info,
                                         const StaticInfo& data)
    : m_name(name), m_static_info(data), m_info(info) {
  for (auto& e : m_info.lods) {
    e.mgeo->parent_element = this;
    e.lod_dist->parent_element = this;
  }
  m_info.janim->parent_element = this;
  m_info.jgeo->parent_element = this;
}

void DefskelgroupElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (auto& e : m_info.lods) {
    e.mgeo->apply(f);
    e.lod_dist->apply(f);
  }
  m_info.janim->apply(f);
  m_info.jgeo->apply(f);
}

void DefskelgroupElement::apply_form(const std::function<void(Form*)>& f) {
  for (auto& e : m_info.lods) {
    e.mgeo->apply_form(f);
    e.lod_dist->apply_form(f);
  }
  m_info.janim->apply_form(f);
  m_info.jgeo->apply_form(f);
}

void DefskelgroupElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  for (auto& e : m_info.lods) {
    e.mgeo->collect_vars(vars, recursive);
    e.lod_dist->collect_vars(vars, recursive);
  }
  m_info.janim->collect_vars(vars, recursive);
  m_info.jgeo->collect_vars(vars, recursive);
}

void DefskelgroupElement::get_modified_regs(RegSet& regs) const {
  for (auto& e : m_info.lods) {
    e.mgeo->get_modified_regs(regs);
    e.lod_dist->get_modified_regs(regs);
  }
  m_info.janim->get_modified_regs(regs);
  m_info.jgeo->get_modified_regs(regs);
}

goos::Object DefskelgroupElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("defskelgroup"));
  forms.push_back(pretty_print::to_symbol(m_name));
  forms.push_back(pretty_print::to_symbol(m_static_info.art_group_name));
  const auto& art = env.dts->art_group_info.find(m_static_info.art_group_name + "-ag");
  bool has_art = art != env.dts->art_group_info.end();
  auto jg = m_info.jgeo->to_form(env);
  if (jg.is_int() && has_art && art->second.count(jg.as_int())) {
    forms.push_back(pretty_print::to_symbol(art->second.at(jg.as_int())));
  } else {
    forms.push_back(jg);
  }
  auto ja = m_info.janim->to_form(env);
  if (ja.is_int() && has_art && art->second.count(ja.as_int())) {
    forms.push_back(pretty_print::to_symbol(art->second.at(ja.as_int())));
  } else {
    forms.push_back(ja);
  }

  std::vector<goos::Object> lod_forms;
  for (const auto& e : m_info.lods) {
    auto f_dist = pretty_print::to_symbol(
        fmt::format("(meters {})", meters_to_string(e.lod_dist->to_form(env).as_float())));
    auto mg = e.mgeo->to_form(env);
    if (mg.is_int() && has_art && art->second.count(mg.as_int())) {
      lod_forms.push_back(
          pretty_print::build_list(pretty_print::to_symbol(art->second.at(mg.as_int())), f_dist));
    } else {
      lod_forms.push_back(pretty_print::build_list(mg, f_dist));
    }
  }
  forms.push_back(pretty_print::build_list(lod_forms));

  forms.push_back(pretty_print::to_symbol(fmt::format(
      ":bounds (static-spherem {} {} {} {})", meters_to_string(m_static_info.bounds.x()),
      meters_to_string(m_static_info.bounds.y()), meters_to_string(m_static_info.bounds.z()),
      meters_to_string(m_static_info.bounds.w()))));

  if (m_static_info.longest_edge != 0) {
    forms.push_back(pretty_print::to_symbol(
        fmt::format(":longest-edge (meters {})", meters_to_string(m_static_info.longest_edge))));
  }

  if (m_static_info.shadow != 0) {
    if (has_art && art->second.count(m_static_info.shadow)) {
      forms.push_back(
          pretty_print::to_symbol(fmt::format(":shadow {}", art->second.at(m_static_info.shadow))));
    } else {
      forms.push_back(pretty_print::to_symbol(fmt::format(":shadow {}", m_static_info.shadow)));
    }
  }
  if (m_static_info.tex_level != 0) {
    forms.push_back(
        pretty_print::to_symbol(fmt::format(":texture-level {}", m_static_info.tex_level)));
  }
  if (m_static_info.sort != 0) {
    forms.push_back(pretty_print::to_symbol(fmt::format(":sort {}", m_static_info.sort)));
  }
  // jak 2 skelgroups seem to be using version 7
  if (env.version != GameVersion::Jak1) {
    if (m_static_info.version != 7) {
      forms.push_back(pretty_print::to_symbol(fmt::format(":version {}", m_static_info.version)));
    }
  } else {
    if (m_static_info.version != 6) {
      forms.push_back(pretty_print::to_symbol(fmt::format(":version {}", m_static_info.version)));
    }
  }
  if (env.version != GameVersion::Jak1) {
    if (m_static_info.origin_joint_index != 0) {
      forms.push_back(pretty_print::to_symbol(
          fmt::format(":origin-joint-index {}", m_static_info.origin_joint_index)));
    }
    if (m_static_info.shadow_joint_index != 0) {
      forms.push_back(pretty_print::to_symbol(
          fmt::format(":shadow-joint-index {}", m_static_info.origin_joint_index)));
    }
    if (m_static_info.light_index != 0) {
      forms.push_back(pretty_print::to_symbol(
          fmt::format(":light-index {}", m_static_info.origin_joint_index)));
    }
  }

  return pretty_print::build_list(forms);
}

////////////////////////////////
// DefpartgroupElement
////////////////////////////////

DefpartgroupElement::DefpartgroupElement(const StaticInfo& data, int group_id)
    : m_static_info(data), m_group_id(group_id) {}

void DefpartgroupElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void DefpartgroupElement::apply_form(const std::function<void(Form*)>&) {}
void DefpartgroupElement::collect_vars(RegAccessSet&, bool) const {}
void DefpartgroupElement::get_modified_regs(RegSet&) const {}

goos::Object DefpartgroupElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(fmt::format("defpartgroup {}", name())));
  forms.push_back(pretty_print::to_symbol(fmt::format(":id {}", m_group_id)));
  if (m_static_info.duration != 3000) {
    forms.push_back(pretty_print::to_symbol(
        fmt::format(":duration (seconds {})", seconds_to_string(m_static_info.duration))));
  }
  if (m_static_info.linger != 1500) {
    // 5 seconds is default
    forms.push_back(pretty_print::to_symbol(
        fmt::format(":linger-duration (seconds {})", seconds_to_string(m_static_info.linger))));
  }
  if (m_static_info.flags != 0) {
    auto things = decompile_bitfield_enum_from_int(TypeSpec("sp-group-flag"), env.dts->ts,
                                                   m_static_info.flags);
    std::string result = ":flags (";
    for (auto& thing : things) {
      result += thing;
      result += ' ';
    }
    result.pop_back();
    result += ')';
    forms.push_back(pretty_print::to_symbol(result));
  }
  forms.push_back(pretty_print::to_symbol(fmt::format(
      ":bounds (static-bspherem {} {} {} {})", meters_to_string(m_static_info.bounds.x()),
      meters_to_string(m_static_info.bounds.y()), meters_to_string(m_static_info.bounds.z()),
      meters_to_string(m_static_info.bounds.w()))));

  if (env.version != GameVersion::Jak1) {
    // jak 2 stuff.
    if (m_static_info.rot != 0) {
      forms.push_back(pretty_print::to_symbol(fmt::format(
          ":rotate ((degrees {}) (degrees {}) (degrees {}))",
          degrees_to_string(m_static_info.rot.x()), degrees_to_string(m_static_info.rot.y()),
          degrees_to_string(m_static_info.rot.z()))));
    }
    if (m_static_info.scale != 1) {
      forms.push_back(pretty_print::to_symbol(fmt::format(
          ":scale ({} {} {})", float_to_string(m_static_info.scale.x()),
          float_to_string(m_static_info.scale.y()), float_to_string(m_static_info.scale.z()))));
    }
  }

  std::vector<goos::Object> item_forms;
  for (const auto& e : m_static_info.elts) {
    s32 launcher = e.part_id;
    u16 flags = e.flags;
    u16 period = e.period;
    u16 length = e.length;
    u16 offset = e.offset;
    u32 hour_mask = e.hour_mask;
    u32 binding = e.binding;

    std::string result =
        fmt::format("(sp-item {}", launcher);  // use decimal, so it matches array idx

    if (e.fade != 0.0) {
      result += fmt::format(" :fade-after (meters {})", meters_to_string(e.fade));
    }

    if (e.falloff != 0.0) {
      result += fmt::format(" :falloff-to (meters {})", meters_to_string(e.falloff));
    }

    if (flags) {
      auto things =
          decompile_bitfield_enum_from_int(TypeSpec("sp-group-item-flag"), env.dts->ts, flags);
      result += " :flags (";
      for (auto& thing : things) {
        result += thing;
        result += ' ';
      }
      result.pop_back();
      result += ')';
    }

    if (period) {
      result += fmt::format(" :period (seconds {})", seconds_to_string(period));
    }

    if (length) {
      result += fmt::format(" :length (seconds {})", seconds_to_string(length));
    }

    if (offset) {
      // jak2 has switched this field to a signed 16 bit number
      if (env.version == GameVersion::Jak2) {
        result += fmt::format(" :offset {}", (s16)offset);
      } else {
        result += fmt::format(" :offset {}", offset);
      }
    }

    if (hour_mask) {
      result += fmt::format(" :hour-mask #b{:b}", hour_mask);
    }

    if (binding) {
      result += fmt::format(" :binding {}", binding);
    }

    result += ')';

    item_forms.push_back(pretty_print::to_symbol(result));
  }
  if (!item_forms.empty()) {
    forms.push_back(pretty_print::to_symbol(":parts"));
    forms.push_back(pretty_print::build_list(item_forms));
  }

  return pretty_print::build_list(forms);
}

////////////////////////////////
// DefpartElement
////////////////////////////////

DefpartElement::DefpartElement(const StaticInfo& data, int id) : m_static_info(data), m_id(id) {}

void DefpartElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void DefpartElement::apply_form(const std::function<void(Form*)>&) {}
void DefpartElement::collect_vars(RegAccessSet&, bool) const {}
void DefpartElement::get_modified_regs(RegSet&) const {}

goos::Object DefpartElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("defpart"));
  forms.push_back(pretty_print::to_symbol(fmt::format("{}", m_id)));

  std::vector<goos::Object> item_forms;
  for (const auto& e : m_static_info.fields) {
    if (e.is_sp_end(env.version)) {
      // sp-end
      break;
    }
    item_forms.push_back(decompile_sparticle_field_init(e.data, e.field_id, e.flags, e.sound_spec,
                                                        e.userdata, env.dts->ts, env.version));
  }
  if (!item_forms.empty()) {
    forms.push_back(pretty_print::to_symbol(":init-specs"));
    forms.push_back(pretty_print::build_list(item_forms));
  }

  return pretty_print::build_list(forms);
}

////////////////////////////////
// ResLumpMacroElement
////////////////////////////////

ResLumpMacroElement::ResLumpMacroElement(Kind kind,
                                         Form* lump_object,
                                         Form* property_name,
                                         Form* default_arg,
                                         Form* tag_ptr,
                                         Form* time,
                                         const TypeSpec& result_type)
    : m_kind(kind),
      m_lump_object(lump_object),
      m_property_name(property_name),
      m_default_arg(default_arg),
      m_tag_ptr(tag_ptr),
      m_time(time),
      m_result_type(result_type) {
  m_lump_object->parent_element = this;
  m_property_name->parent_element = this;
  if (m_default_arg) {
    m_default_arg->parent_element = this;
  }
  if (m_tag_ptr) {
    m_tag_ptr->parent_element = this;
  }
  if (m_time) {
    m_time->parent_element = this;
  }
}

void ResLumpMacroElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_lump_object->apply(f);
  m_property_name->apply(f);
  if (m_default_arg) {
    m_default_arg->apply(f);
  }
  if (m_tag_ptr) {
    m_tag_ptr->apply(f);
  }
  if (m_time) {
    m_time->apply(f);
  }
}

void ResLumpMacroElement::apply_form(const std::function<void(Form*)>& f) {
  m_lump_object->apply_form(f);
  m_property_name->apply_form(f);
  if (m_default_arg) {
    m_default_arg->apply_form(f);
  }
  if (m_tag_ptr) {
    m_tag_ptr->apply_form(f);
  }
  if (m_time) {
    m_time->apply_form(f);
  }
}

void ResLumpMacroElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_lump_object->collect_vars(vars, recursive);
    m_property_name->collect_vars(vars, recursive);
    if (m_default_arg) {
      m_default_arg->collect_vars(vars, recursive);
    }
    if (m_tag_ptr) {
      m_tag_ptr->collect_vars(vars, recursive);
    }
    if (m_time) {
      m_time->collect_vars(vars, recursive);
    }
  }
}

void ResLumpMacroElement::get_modified_regs(RegSet& regs) const {
  m_lump_object->get_modified_regs(regs);
  m_property_name->get_modified_regs(regs);
  if (m_default_arg) {
    m_default_arg->get_modified_regs(regs);
  }
  if (m_tag_ptr) {
    m_tag_ptr->get_modified_regs(regs);
  }
  if (m_time) {
    m_time->get_modified_regs(regs);
  }
}

goos::Object ResLumpMacroElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;

  switch (m_kind) {
    case Kind::DATA:
      forms.push_back(pretty_print::to_symbol("res-lump-data"));
      break;
    case Kind::STRUCT:
      forms.push_back(pretty_print::to_symbol("res-lump-struct"));
      break;
    case Kind::VALUE:
      forms.push_back(pretty_print::to_symbol("res-lump-value"));
      break;
    default:
      ASSERT(false);
  }

  forms.push_back(m_lump_object->to_form(env));
  forms.push_back(m_property_name->to_form(env));

  forms.push_back(pretty_print::to_symbol(m_result_type.print()));

  if (m_default_arg) {
    forms.push_back(pretty_print::to_symbol(":default"));
    forms.push_back(m_default_arg->to_form(env));
  }

  if (m_tag_ptr) {
    forms.push_back(pretty_print::to_symbol(":tag-ptr"));
    forms.push_back(m_tag_ptr->to_form(env));
  }

  if (m_time) {
    forms.push_back(pretty_print::to_symbol(":time"));
    forms.push_back(m_time->to_form(env));
  }

  return pretty_print::build_list(forms);
}
////////////////////////////////
// Utilities
////////////////////////////////

std::optional<SimpleAtom> form_element_as_atom(const FormElement* f) {
  auto as_atom = dynamic_cast<const SimpleAtomElement*>(f);
  if (as_atom) {
    return as_atom->atom();
  }

  auto as_se = dynamic_cast<const SimpleExpressionElement*>(f);
  if (as_se && as_se->expr().is_identity()) {
    return as_se->expr().get_arg(0);
  }

  return {};
}

std::optional<SimpleAtom> form_as_atom(const Form* f) {
  auto as_single = f->try_as_single_element();
  return form_element_as_atom(as_single);
}

FormElement* make_cast_using_existing(FormElement* elt, const TypeSpec& type, FormPool& pool) {
  auto as_cast = dynamic_cast<CastElement*>(elt);
  if (as_cast) {
    if (as_cast->type() != type) {
      as_cast->set_type(type);
    }
    return as_cast;
  } else {
    return pool.alloc_element<CastElement>(type, pool.alloc_single_form(nullptr, elt));
  }
}

GenericElement* alloc_generic_token_op(const std::string& name,
                                       const std::vector<Form*>& args,
                                       FormPool& pool) {
  auto op = GenericOperator::make_function(
      pool.alloc_single_element_form<ConstantTokenElement>(nullptr, name));
  return pool.alloc_element<GenericElement>(op, args);
}

Form* alloc_var_form(const RegisterAccess& var, FormPool& pool) {
  return pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(var));
}

}  // namespace decompiler
