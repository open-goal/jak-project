#include "Form.h"

#include <utility>
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/goos/PrettyPrinter.h"
#include "common/type_system/TypeSystem.h"
#include <algorithm>

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
  throw std::runtime_error("push_to_stack not implemented for " + to_string(env));
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
  assert(!m_elements.empty());
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
  assert(!m_elements.empty());
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

LoadSourceElement::LoadSourceElement(Form* addr, int size, LoadVarOp::Kind kind)
    : m_addr(addr), m_size(size), m_kind(kind) {
  m_addr->parent_element = this;
}

goos::Object LoadSourceElement::to_form_internal(const Env& env) const {
  switch (m_kind) {
    case LoadVarOp::Kind::FLOAT:
      assert(m_size == 4);
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
        default:
          assert(false);
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
          assert(false);
          return {};
      }
      break;
    default:
      assert(false);
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

SimpleAtomElement::SimpleAtomElement(const SimpleAtom& atom) : m_atom(atom) {}

goos::Object SimpleAtomElement::to_form_internal(const Env& env) const {
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
                             const SetVarInfo& info)
    : m_dst(var), m_src(value), m_is_sequence_point(is_sequence_point), m_var_info(info) {
  value->parent_element = this;
}

goos::Object SetVarElement::to_form_internal(const Env& env) const {
  assert(active());
  return pretty_print::build_list("set!", m_dst.to_form(env), m_src->to_form(env));
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

goos::Object SetFormFormElement::to_form_for_define(const Env& env) const {
  if (m_cast_for_define) {
    return pretty_print::build_list(
        fmt::format("define"), m_dst->to_form(env),
        pretty_print::build_list(fmt::format("the-as {}", m_cast_for_define->print()),
                                 m_src->to_form(env)));
  } else {
    std::vector<goos::Object> forms = {pretty_print::to_symbol("define"), m_dst->to_form(env),
                                       m_src->to_form(env)};
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

AtomicOpElement::AtomicOpElement(const AtomicOp* op) : m_op(op) {}

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
    if (r.get_kind() == Reg::RegisterKind::VF ||
        r.get_kind() == Reg::RegisterKind::COP2_MACRO_SPECIAL) {
      regs.insert(r);
    }
  }

  for (auto r : m_op->write_regs()) {
    if (r.get_kind() == Reg::RegisterKind::VF ||
        r.get_kind() == Reg::RegisterKind::COP2_MACRO_SPECIAL) {
      regs.insert(r);
    }
  }

  for (auto r : m_op->clobber_regs()) {
    if (r.get_kind() == Reg::RegisterKind::VF ||
        r.get_kind() == Reg::RegisterKind::COP2_MACRO_SPECIAL) {
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

BreakElement::BreakElement(Form* _return_code, Form* _dead_code)
    : return_code(_return_code), dead_code(_dead_code) {
  return_code->parent_element = this;
  dead_code->parent_element = this;
}

goos::Object BreakElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("break"));
  forms.push_back(pretty_print::build_list(return_code->to_form(env)));
  forms.push_back(pretty_print::build_list(dead_code->to_form(env)));
  return pretty_print::build_list(forms);
}

void BreakElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  return_code->apply(f);
  dead_code->apply(f);
}

void BreakElement::apply_form(const std::function<void(Form*)>& f) {
  return_code->apply_form(f);
  dead_code->apply_form(f);
}

void BreakElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    return_code->collect_vars(vars, recursive);
    dead_code->collect_vars(vars, recursive);
  }
}

void BreakElement::get_modified_regs(RegSet& regs) const {
  for (auto x : {return_code, dead_code}) {
    x->get_modified_regs(regs);
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
  if (entries.size() == 1 && entries.front().body->is_single_element() &&
      else_ir->is_single_element()) {
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
  int comparison = x.to_string().compare(y.to_string());
  if (comparison <= 0)
    return true;
  return false;
}

RLetElement::RLetElement(Form* _body, RegSet _regs) : body(_body) {
  for (auto& reg : _regs) {
    sorted_regs.push_back(reg);
  }
  std::sort(sorted_regs.begin(), sorted_regs.end(), cmp);
}

void RLetElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  body->apply(f);
}

goos::Object RLetElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> regs;
  for (auto& reg : sorted_regs) {
    if (reg.get_kind() == Reg::RegisterKind::VF ||
        reg.get_kind() == Reg::RegisterKind::COP2_MACRO_SPECIAL) {
      std::string reg_name = reg.to_string() == "ACC" ? "acc" : reg.to_string();
      regs.push_back(
          pretty_print::build_list(pretty_print::to_symbol(fmt::format("{} :class vf", reg_name))));
    }
  }

  std::vector<goos::Object> rletForm;
  rletForm.push_back(pretty_print::to_symbol("rlet"));
  rletForm.push_back(pretty_print::build_list(regs));

  // NOTE - initialize any relevant registers in the body first
  for (auto& reg : sorted_regs) {
    if (reg.get_kind() == Reg::RegisterKind::VF && reg.to_string() == "vf0") {
      // TODO - a good idea to move this to a macro like initialize-constant-vector! or something.
      // There could be some clever way to do this initialization that's faster that a normal static
      // load.
      rletForm.push_back(
          pretty_print::to_symbol("(.lvf vf0 (new 'static 'vector :x 0.0 :y 0.0 :z 0.0 :w 1.0))"));
    }
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
  list.push_back(pretty_print::to_symbol("while"));
  list.push_back(condition->to_form_as_condition(env));
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
      assert(false);
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
  if (entries.size() == 1 && entries.front().body->is_single_element()) {
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

TypeOfElement::TypeOfElement(Form* _value, std::optional<RegisterAccess> _clobber)
    : value(_value), clobber(_clobber) {
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
    case FixedOperatorKind::MULTIPLICATION:
      return "*";
    case FixedOperatorKind::SQRT:
      return "sqrt";
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
    case FixedOperatorKind::LOGIOR:
      return "logior";
    case FixedOperatorKind::LOGXOR:
      return "logxor";
    case FixedOperatorKind::LOGNOR:
      return "lognor";
    case FixedOperatorKind::LOGNOT:
      return "lognot";
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
    case FixedOperatorKind::NULLP:
      return "null?";
    case FixedOperatorKind::PAIRP:
      return "pair?";
    default:
      assert(false);
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
    assert(m_elts.size() == 1);
    return m_elts.front()->to_form_as_condition(env);
  } else {
    std::vector<goos::Object> result;
    result.push_back(m_head.to_form(env));
    for (auto x : m_elts) {
      assert(x->parent_element);
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
  source->parent_element = this;
}

goos::Object CastElement::to_form_internal(const Env& env) const {
  return pretty_print::build_list(m_numeric ? "the" : "the-as", m_type.print(),
                                  m_source->to_form(env));
}

void CastElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_source->apply(f);
}

void CastElement::apply_form(const std::function<void(Form*)>& f) {
  m_source->apply_form(f);
}

void CastElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  if (recursive) {
    m_source->collect_vars(vars, recursive);
  }
}

void CastElement::get_modified_regs(RegSet& regs) const {
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
      assert(false);
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
}

DerefElement::DerefElement(Form* base, bool is_addr_of, std::vector<DerefToken> tokens)
    : m_base(base), m_is_addr_of(is_addr_of), m_tokens(std::move(tokens)) {
  m_base->parent_element = this;
  for (auto& x : m_tokens) {
    if (x.kind() == DerefToken::Kind::INTEGER_EXPRESSION) {
      x.expr()->parent_element = this;
    }
  }
}

goos::Object DerefElement::to_form_internal(const Env& env) const {
  assert(m_base->parent_element);
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
                                   int constant_offset)
    : m_source(source),
      m_deref_tokens(deref_tokens),
      m_expected_stride(expected_stride),
      m_constant_offset(constant_offset) {
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

StorePlainDeref::StorePlainDeref(DerefElement* dst,
                                 SimpleExpression expr,
                                 int my_idx,
                                 RegisterAccess base_var,
                                 std::optional<TypeSpec> cast_type)
    : m_dst(dst),
      m_expr(std::move(expr)),
      m_my_idx(my_idx),
      m_base_var(std::move(base_var)),
      m_cast_type(cast_type) {}
goos::Object StorePlainDeref::to_form_internal(const Env& env) const {
  if (!m_cast_type.has_value()) {
    return pretty_print::build_list("set!", m_dst->to_form(env),
                                    m_expr.to_form(env.file->labels, env));
  } else {
    return pretty_print::build_list(
        "set!", pretty_print::build_list("the-as", m_cast_type->print(), m_dst->to_form(env)),
        m_expr.to_form(env.file->labels, env));
  }
}
void StorePlainDeref::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_dst->apply(f);
}

void StorePlainDeref::apply_form(const std::function<void(Form*)>&) {}

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
                                   RegisterAccess array_src)
    : m_dst(dst), m_expr(expr), m_my_idx(my_idx), m_base_var(array_src) {}

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

DecompiledDataElement::DecompiledDataElement(goos::Object description)
    : m_description(std::move(description)) {}

goos::Object DecompiledDataElement::to_form_internal(const Env&) const {
  return m_description;
}

void DecompiledDataElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void DecompiledDataElement::apply_form(const std::function<void(Form*)>&) {}

void DecompiledDataElement::collect_vars(RegAccessSet&, bool) const {}

void DecompiledDataElement::get_modified_regs(RegSet&) const {}

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

goos::Object LetElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> outer = {pretty_print::to_symbol(m_star ? "let*" : "let")};

  std::vector<goos::Object> def_list;

  for (auto& entry : m_entries) {
    def_list.push_back(pretty_print::build_list(entry.dest.to_form(env), entry.src->to_form(env)));
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
// DoTimesElement
/////////////////////////////

DoTimesElement::DoTimesElement(RegisterAccess var_init,
                               RegisterAccess var_check,
                               RegisterAccess var_inc,
                               Form* check_value,
                               Form* body)
    : m_var_init(var_init),
      m_var_check(var_check),
      m_var_inc(var_inc),
      m_check_value(check_value),
      m_body(body) {
  m_body->parent_element = this;
  m_check_value->parent_element = this;
  assert(m_var_inc.reg() == m_var_check.reg());
  assert(m_var_init.reg() == m_var_inc.reg());
}

goos::Object DoTimesElement::to_form_internal(const Env& env) const {
  std::vector<goos::Object> outer = {
      pretty_print::to_symbol("dotimes"),
      pretty_print::build_list(m_var_init.to_form(env), m_check_value->to_form(env))};
  m_body->inline_forms(outer, env);
  return pretty_print::build_list(outer);
}

void DoTimesElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_check_value->apply(f);
  m_body->apply(f);
}

void DoTimesElement::apply_form(const std::function<void(Form*)>& f) {
  m_check_value->apply_form(f);
  m_body->apply_form(f);
}

void DoTimesElement::collect_vars(RegAccessSet& vars, bool recursive) const {
  vars.insert(m_var_init);
  vars.insert(m_var_check);
  vars.insert(m_var_inc);
  if (recursive) {
    m_body->collect_vars(vars, recursive);
    m_check_value->collect_vars(vars, recursive);
  }
}

void DoTimesElement::get_modified_regs(RegSet& regs) const {
  regs.insert(m_var_inc.reg());
  m_body->get_modified_regs(regs);
  m_check_value->get_modified_regs(regs);
}

std::optional<SimpleAtom> form_as_atom(const Form* f) {
  auto as_single = f->try_as_single_element();
  auto as_atom = dynamic_cast<SimpleAtomElement*>(as_single);
  if (as_atom) {
    return as_atom->atom();
  }

  auto as_se = dynamic_cast<SimpleExpressionElement*>(as_single);
  if (as_se && as_se->expr().is_identity()) {
    return as_se->expr().get_arg(0);
  }

  return {};
}

}  // namespace decompiler
