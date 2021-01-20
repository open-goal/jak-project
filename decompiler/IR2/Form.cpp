#include "Form.h"

#include <utility>
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/goos/PrettyPrinter.h"

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
      forms.push_back(x->to_form(env));
    }
    return pretty_print::build_list(forms);
  }
}

void Form::inline_forms(std::vector<goos::Object>& forms, const Env& env) const {
  for (auto& x : m_elements) {
    forms.push_back(x->to_form(env));
  }
}

void Form::apply(const std::function<void(FormElement*)>& f) {
  for (auto& x : m_elements) {
    x->apply(f);
  }
}

void Form::apply_form(const std::function<void(Form*)>& f) {
  f(this);
  for (auto& x : m_elements) {
    x->apply_form(f);
  }
}

void Form::collect_vars(VariableSet& vars) const {
  for (auto e : m_elements) {
    e->collect_vars(vars);
  }
}

/////////////////////////////
// SimpleExpressionElement
/////////////////////////////

SimpleExpressionElement::SimpleExpressionElement(SimpleExpression expr) : m_expr(std::move(expr)) {}

goos::Object SimpleExpressionElement::to_form(const Env& env) const {
  return m_expr.to_form(env.file->labels, &env);
}

void SimpleExpressionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void SimpleExpressionElement::apply_form(const std::function<void(Form*)>&) {}

bool SimpleExpressionElement::is_sequence_point() const {
  throw std::runtime_error("Should not check if a SimpleExpressionElement is a sequence point");
}

void SimpleExpressionElement::collect_vars(VariableSet& vars) const {
  m_expr.collect_vars(vars);
}

/////////////////////////////
// StoreElement
/////////////////////////////

StoreElement::StoreElement(const StoreOp* op) : m_op(op) {}

goos::Object StoreElement::to_form(const Env& env) const {
  return m_op->to_form(env.file->labels, &env);
}

void StoreElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void StoreElement::apply_form(const std::function<void(Form*)>&) {}

void StoreElement::collect_vars(VariableSet& vars) const {
  return m_op->collect_vars(vars);
}

/////////////////////////////
// LoadSourceElement
/////////////////////////////

LoadSourceElement::LoadSourceElement(Form* addr, int size, LoadVarOp::Kind kind)
    : m_addr(addr), m_size(size), m_kind(kind) {
  m_addr->parent_element = this;
}

goos::Object LoadSourceElement::to_form(const Env& env) const {
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
      }
      break;
    default:
      assert(false);
  }
}

void LoadSourceElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_addr->apply(f);
}

void LoadSourceElement::apply_form(const std::function<void(Form*)>& f) {
  m_addr->apply_form(f);
}

void LoadSourceElement::collect_vars(VariableSet& vars) const {
  m_addr->collect_vars(vars);
}

/////////////////////////////
// SimpleAtomElement
/////////////////////////////

SimpleAtomElement::SimpleAtomElement(const SimpleAtom& atom) : m_atom(atom) {}

goos::Object SimpleAtomElement::to_form(const Env& env) const {
  return m_atom.to_form(env.file->labels, &env);
}

void SimpleAtomElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void SimpleAtomElement::apply_form(const std::function<void(Form*)>&) {}

void SimpleAtomElement::collect_vars(VariableSet& vars) const {
  return m_atom.collect_vars(vars);
}

/////////////////////////////
// SetVarElement
/////////////////////////////

SetVarElement::SetVarElement(const Variable& var, Form* value, bool is_sequence_point)
    : m_dst(var), m_src(value), m_is_sequence_point(is_sequence_point) {
  value->parent_element = this;
}

goos::Object SetVarElement::to_form(const Env& env) const {
  return pretty_print::build_list("set!", m_dst.to_string(&env), m_src->to_form(env));
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

void SetVarElement::collect_vars(VariableSet& vars) const {
  vars.insert(m_dst);
  m_src->collect_vars(vars);
}

/////////////////////////////
// AtomicOpElement
/////////////////////////////

AtomicOpElement::AtomicOpElement(const AtomicOp* op) : m_op(op) {}

goos::Object AtomicOpElement::to_form(const Env& env) const {
  return m_op->to_form(env.file->labels, &env);
}

void AtomicOpElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AtomicOpElement::apply_form(const std::function<void(Form*)>&) {}

void AtomicOpElement::collect_vars(VariableSet& vars) const {
  m_op->collect_vars(vars);
}

/////////////////////////////
// ConditionElement
/////////////////////////////

ConditionElement::ConditionElement(IR2_Condition::Kind kind, Form* src0, Form* src1)
    : m_kind(kind) {
  m_src[0] = src0;
  m_src[1] = src1;
  for (int i = 0; i < 2; i++) {
    if (m_src[i]) {
      m_src[i]->parent_element = this;
    }
  }
}

goos::Object ConditionElement::to_form(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(get_condition_kind_name(m_kind)));
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    forms.push_back(m_src[i]->to_form(env));
  }
  if (forms.size() > 1) {
    return pretty_print::build_list(forms);
  } else {
    return forms.front();
  }
}

void ConditionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  for (int i = 0; i < 2; i++) {
    if (m_src[i]) {
      m_src[i]->apply(f);
    }
  }
}

void ConditionElement::apply_form(const std::function<void(Form*)>& f) {
  for (int i = 0; i < 2; i++) {
    if (m_src[i]) {
      m_src[i]->apply_form(f);
    }
  }
}

void ConditionElement::invert() {
  m_kind = get_condition_opposite(m_kind);
}

void ConditionElement::collect_vars(VariableSet& vars) const {
  for (auto src : m_src) {
    if (src) {
      src->collect_vars(vars);
    }
  }
}

/////////////////////////////
// FunctionCallElement
/////////////////////////////

FunctionCallElement::FunctionCallElement(const CallOp* op) : m_op(op) {}

goos::Object FunctionCallElement::to_form(const Env& env) const {
  return m_op->to_form(env.file->labels, &env);
}

void FunctionCallElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void FunctionCallElement::apply_form(const std::function<void(Form*)>&) {}

void FunctionCallElement::collect_vars(VariableSet& vars) const {
  return m_op->collect_vars(vars);
}

/////////////////////////////
// BranchElement
/////////////////////////////

BranchElement::BranchElement(const BranchOp* op) : m_op(op) {}

goos::Object BranchElement::to_form(const Env& env) const {
  return m_op->to_form(env.file->labels, &env);
}

void BranchElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void BranchElement::apply_form(const std::function<void(Form*)>&) {}

void BranchElement::collect_vars(VariableSet& vars) const {
  return m_op->collect_vars(vars);
}

/////////////////////////////
// ReturnElement
/////////////////////////////

goos::Object ReturnElement::to_form(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("return"));
  forms.push_back(pretty_print::build_list(return_code->to_form(env)));
  forms.push_back(pretty_print::build_list(dead_code->to_form(env)));
  return pretty_print::build_list(forms);
}

void ReturnElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  return_code->apply(f);
  dead_code->apply(f);
}

void ReturnElement::apply_form(const std::function<void(Form*)>& f) {
  return_code->apply_form(f);
  dead_code->apply_form(f);
}

void ReturnElement::collect_vars(VariableSet& vars) const {
  return_code->collect_vars(vars);
  dead_code->collect_vars(vars);
}

/////////////////////////////
// BreakElement
/////////////////////////////

goos::Object BreakElement::to_form(const Env& env) const {
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

void BreakElement::collect_vars(VariableSet& vars) const {
  return_code->collect_vars(vars);
  dead_code->collect_vars(vars);
}

/////////////////////////////
// CondWithElseElement
/////////////////////////////

goos::Object CondWithElseElement::to_form(const Env& env) const {
  // for now we only turn it into an if statement if both cases won't require a begin at the top
  // level. I think it is more common to write these as a two-case cond instead of an if with begin.
  if (entries.size() == 1 && entries.front().body->is_single_element() &&
      else_ir->is_single_element()) {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("if"));
    list.push_back(entries.front().condition->to_form(env));
    list.push_back(entries.front().body->to_form(env));
    list.push_back(else_ir->to_form(env));
    return pretty_print::build_list(list);
  } else {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("cond"));
    for (auto& e : entries) {
      std::vector<goos::Object> entry;
      entry.push_back(e.condition->to_form(env));
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

void CondWithElseElement::collect_vars(VariableSet& vars) const {
  for (auto& entry : entries) {
    entry.condition->collect_vars(vars);
    entry.body->collect_vars(vars);
  }
  else_ir->collect_vars(vars);
}

/////////////////////////////
// EmptyElement
/////////////////////////////

goos::Object EmptyElement::to_form(const Env&) const {
  return pretty_print::build_list("empty");
}

void EmptyElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void EmptyElement::apply_form(const std::function<void(Form*)>&) {}

void EmptyElement::collect_vars(VariableSet&) const {}

/////////////////////////////
// WhileElement
/////////////////////////////

void WhileElement::apply(const std::function<void(FormElement*)>& f) {
  // note - this is done in program order, rather than print order. Not sure if this makes sense.
  f(this);
  body->apply(f);
  condition->apply(f);
}

goos::Object WhileElement::to_form(const Env& env) const {
  std::vector<goos::Object> list;
  list.push_back(pretty_print::to_symbol("while"));
  list.push_back(condition->to_form(env));
  body->inline_forms(list, env);
  return pretty_print::build_list(list);
}

void WhileElement::apply_form(const std::function<void(Form*)>& f) {
  body->apply_form(f);
  condition->apply_form(f);
}

void WhileElement::collect_vars(VariableSet& vars) const {
  body->collect_vars(vars);
  condition->collect_vars(vars);
}

/////////////////////////////
// UntilElement
/////////////////////////////

void UntilElement::apply(const std::function<void(FormElement*)>& f) {
  // note - this is done in program order, rather than print order. Not sure if this makes sense.
  f(this);
  body->apply(f);
  condition->apply(f);
}

goos::Object UntilElement::to_form(const Env& env) const {
  std::vector<goos::Object> list;
  list.push_back(pretty_print::to_symbol("until"));
  list.push_back(condition->to_form(env));
  body->inline_forms(list, env);
  return pretty_print::build_list(list);
}

void UntilElement::apply_form(const std::function<void(Form*)>& f) {
  body->apply_form(f);
  condition->apply_form(f);
}

void UntilElement::collect_vars(VariableSet& vars) const {
  body->collect_vars(vars);
  condition->collect_vars(vars);
}

/////////////////////////////
// ShortCircuitElement
/////////////////////////////

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

goos::Object ShortCircuitElement::to_form(const Env& env) const {
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
    forms.push_back(x.condition->to_form(env));
  }
  return pretty_print::build_list(forms);
}

void ShortCircuitElement::collect_vars(VariableSet& vars) const {
  vars.insert(final_result);  // todo - this might be unused.
  for (auto& entry : entries) {
    entry.condition->collect_vars(vars);
  }
}

/////////////////////////////
// CondNoElseElement
/////////////////////////////

goos::Object CondNoElseElement::to_form(const Env& env) const {
  if (entries.size() == 1 && entries.front().body->is_single_element()) {
    // print as an if statement if we can put the body in a single form.
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("if"));
    list.push_back(entries.front().condition->to_form(env));
    list.push_back(entries.front().body->to_form(env));
    return pretty_print::build_list(list);
  } else if (entries.size() == 1) {
    // turn into a when if the body requires multiple forms
    // todo check to see if the condition starts with a NOT and this can be simplified to an
    // unless.
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("when"));
    list.push_back(entries.front().condition->to_form(env));
    entries.front().body->inline_forms(list, env);
    return pretty_print::build_list(list);
  } else {
    std::vector<goos::Object> list;
    list.push_back(pretty_print::to_symbol("cond"));
    for (auto& e : entries) {
      std::vector<goos::Object> entry;
      entry.push_back(e.condition->to_form(env));
      entries.front().body->inline_forms(list, env);
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

void CondNoElseElement::collect_vars(VariableSet& vars) const {
  for (auto& e : entries) {
    e.condition->collect_vars(vars);
    e.body->collect_vars(vars);
    if (e.false_destination.has_value()) {
      vars.insert(*e.false_destination);
    }
  }
}
/////////////////////////////
// AbsElement
/////////////////////////////

AbsElement::AbsElement(Form* _source) : source(_source) {
  source->parent_element = this;
}

goos::Object AbsElement::to_form(const Env& env) const {
  return pretty_print::build_list("abs", source->to_form(env));
}

void AbsElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  source->apply(f);
}

void AbsElement::apply_form(const std::function<void(Form*)>& f) {
  source->apply_form(f);
}

void AbsElement::collect_vars(VariableSet& vars) const {
  source->collect_vars(vars);
}

/////////////////////////////
// AshElement
/////////////////////////////

AshElement::AshElement(Form* _shift_amount,
                       Form* _value,
                       std::optional<Variable> _clobber,
                       bool _is_signed)
    : shift_amount(_shift_amount), value(_value), clobber(_clobber), is_signed(_is_signed) {
  _shift_amount->parent_element = this;
  _value->parent_element = this;
}

goos::Object AshElement::to_form(const Env& env) const {
  return pretty_print::build_list(pretty_print::to_symbol(is_signed ? "ash.si" : "ash.ui"),
                                  value->to_form(env), shift_amount->to_form(env));
}

void AshElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  shift_amount->apply(f);
  value->apply(f);
}

void AshElement::apply_form(const std::function<void(Form*)>& f) {
  shift_amount->apply_form(f);
  value->apply_form(f);
}

void AshElement::collect_vars(VariableSet& vars) const {
  shift_amount->collect_vars(vars);
  value->collect_vars(vars);
}

/////////////////////////////
// TypeOfElement
/////////////////////////////

TypeOfElement::TypeOfElement(Form* _value, std::optional<Variable> _clobber)
    : value(_value), clobber(_clobber) {
  value->parent_element = this;
}

goos::Object TypeOfElement::to_form(const Env& env) const {
  return pretty_print::build_list("type-of", value->to_form(env));
}

void TypeOfElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  value->apply(f);
}

void TypeOfElement::apply_form(const std::function<void(Form*)>& f) {
  value->apply_form(f);
}

void TypeOfElement::collect_vars(VariableSet& vars) const {
  value->collect_vars(vars);
}

/////////////////////////////
// ConditionalMoveFalseElement
/////////////////////////////

ConditionalMoveFalseElement::ConditionalMoveFalseElement(Variable _dest,
                                                         Form* _source,
                                                         bool _on_zero)
    : dest(_dest), source(_source), on_zero(_on_zero) {
  source->parent_element = this;
}

goos::Object ConditionalMoveFalseElement::to_form(const Env& env) const {
  return pretty_print::build_list(on_zero ? "cmove-#f-zero" : "cmove-#f-nonzero",
                                  dest.to_string(&env), source->to_form(env));
}

void ConditionalMoveFalseElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  source->apply(f);
}

void ConditionalMoveFalseElement::apply_form(const std::function<void(Form*)>& f) {
  source->apply_form(f);
}

void ConditionalMoveFalseElement::collect_vars(VariableSet& vars) const {
  vars.insert(dest);
  source->collect_vars(vars);
}
}  // namespace decompiler
