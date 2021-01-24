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
// FormElement
///////////////////

std::string FormElement::to_string(const Env& env) const {
  return to_form(env).print();
}

void FormElement::push_to_stack(const Env& env, FormPool&, FormStack&) {
  throw std::runtime_error("push_to_stack not implemented for " + to_string(env));
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

std::string Form::to_string(const Env& env) const {
  return to_form(env).print();
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

SimpleExpressionElement::SimpleExpressionElement(SimpleExpression expr, int my_idx)
    : m_expr(std::move(expr)), m_my_idx(my_idx) {}

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

ConditionElement::ConditionElement(IR2_Condition::Kind kind,
                                   std::optional<SimpleAtom> src0,
                                   std::optional<SimpleAtom> src1,
                                   RegSet consumed)
    : m_kind(kind), m_consumed(std::move(consumed)) {
  m_src[0] = src0;
  m_src[1] = src1;
}

goos::Object ConditionElement::to_form(const Env& env) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(get_condition_kind_name(m_kind)));
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    forms.push_back(m_src[i]->to_form(env.file->labels, &env));
  }
  if (forms.size() > 1) {
    return pretty_print::build_list(forms);
  } else {
    return forms.front();
  }
}

void ConditionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void ConditionElement::apply_form(const std::function<void(Form*)>&) {}

void ConditionElement::invert() {
  m_kind = get_condition_opposite(m_kind);
}

void ConditionElement::collect_vars(VariableSet& vars) const {
  for (auto src : m_src) {
    if (src.has_value() && src->is_var()) {
      vars.insert(src->var());
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

AbsElement::AbsElement(Variable _source, RegSet _consumed)
    : source(_source), consumed(std::move(_consumed)) {}

goos::Object AbsElement::to_form(const Env& env) const {
  return pretty_print::build_list("abs", source.to_string(&env));
}

void AbsElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AbsElement::apply_form(const std::function<void(Form*)>&) {}

void AbsElement::collect_vars(VariableSet& vars) const {
  vars.insert(source);
}

/////////////////////////////
// AshElement
/////////////////////////////

AshElement::AshElement(Variable _shift_amount,
                       Variable _value,
                       std::optional<Variable> _clobber,
                       bool _is_signed,
                       RegSet _consumed)
    : shift_amount(_shift_amount),
      value(_value),
      clobber(_clobber),
      is_signed(_is_signed),
      consumed(_consumed) {}

goos::Object AshElement::to_form(const Env& env) const {
  return pretty_print::build_list(pretty_print::to_symbol(is_signed ? "ash.si" : "ash.ui"),
                                  value.to_string(&env), shift_amount.to_string(&env));
}

void AshElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

void AshElement::apply_form(const std::function<void(Form*)>&) {}

void AshElement::collect_vars(VariableSet& vars) const {
  vars.insert(value);
  vars.insert(shift_amount);
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

void GenericOperator::collect_vars(VariableSet& vars) const {
  switch (m_kind) {
    case Kind::FIXED_OPERATOR:
    case Kind::CONDITION_OPERATOR:
      return;
    case Kind::FUNCTION_EXPR:
      m_function->collect_vars(vars);
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

std::string fixed_operator_to_string(FixedOperatorKind kind) {
  switch (kind) {
    case FixedOperatorKind::GPR_TO_FPR:
      return "gpr->fpr";
    case FixedOperatorKind::DIVISION:
      return "/";
    case FixedOperatorKind::ADDITION:
      return "+";
    case FixedOperatorKind::SUBTRACTION:
      return "-";
    case FixedOperatorKind::MULTIPLICATION:
      return "*";
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
    case FixedOperatorKind::SLL:
      return "sll";
    default:
      assert(false);
  }
}

GenericElement::GenericElement(GenericOperator op) : m_head(op) {}
GenericElement::GenericElement(GenericOperator op, Form* arg) : m_head(op), m_elts({arg}) {}
GenericElement::GenericElement(GenericOperator op, Form* arg0, Form* arg1)
    : m_head(op), m_elts({arg0, arg1}) {}
GenericElement::GenericElement(GenericOperator op, std::vector<Form*> forms)
    : m_head(op), m_elts(std::move(forms)) {}

goos::Object GenericElement::to_form(const Env& env) const {
  std::vector<goos::Object> result;
  result.push_back(m_head.to_form(env));
  for (auto x : m_elts) {
    result.push_back(x->to_form(env));
  }
  return pretty_print::build_list(result);
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

void GenericElement::collect_vars(VariableSet& vars) const {
  m_head.collect_vars(vars);
  for (auto x : m_elts) {
    x->collect_vars(vars);
  }
}

/////////////////////////////
// CastElement
/////////////////////////////

CastElement::CastElement(TypeSpec type, Form* source) : m_type(std::move(type)), m_source(source) {}

goos::Object CastElement::to_form(const Env& env) const {
  return pretty_print::build_list("the-as", m_type.print(), m_source->to_form(env));
}

void CastElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
  m_source->apply(f);
}

void CastElement::apply_form(const std::function<void(Form*)>& f) {
  m_source->apply_form(f);
}

void CastElement::collect_vars(VariableSet& vars) const {
  m_source->collect_vars(vars);
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

void DerefToken::collect_vars(VariableSet& vars) const {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
      break;
    case Kind::INTEGER_EXPRESSION:
      m_expr->collect_vars(vars);
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
    default:
      assert(false);
  }
}

void DerefToken::apply(const std::function<void(FormElement*)>& f) {
  switch (m_kind) {
    case Kind::INTEGER_CONSTANT:
    case Kind::FIELD_NAME:
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
      break;
    case Kind::INTEGER_EXPRESSION:
      m_expr->apply_form(f);
      break;
    default:
      assert(false);
  }
}

DerefElement::DerefElement(Form* base, bool is_addr_of, DerefToken token)
    : m_base(base), m_is_addr_of(is_addr_of), m_tokens({std::move(token)}) {}

DerefElement::DerefElement(Form* base, bool is_addr_of, std::vector<DerefToken> tokens)
    : m_base(base), m_is_addr_of(is_addr_of), m_tokens(std::move(tokens)) {}

goos::Object DerefElement::to_form(const Env& env) const {
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

void DerefElement::collect_vars(VariableSet& vars) const {
  m_base->collect_vars(vars);
  for (auto& tok : m_tokens) {
    tok.collect_vars(vars);
  }
}

}  // namespace decompiler
