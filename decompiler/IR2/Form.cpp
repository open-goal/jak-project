#include "Form.h"
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

void Form::apply(const std::function<void(FormElement*)>& f) const {
  for (auto& x : m_elements) {
    x->apply(f);
  }
}

/////////////////////////////
// SimpleExpressionElement
/////////////////////////////

SimpleExpressionElement::SimpleExpressionElement(const SimpleExpression& expr) : m_expr(expr) {}

goos::Object SimpleExpressionElement::to_form(const Env& env) const {
  return m_expr.to_form(env.file->labels, &env);
}

void SimpleExpressionElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

bool SimpleExpressionElement::is_sequence_point() const {
  throw std::runtime_error("Should not check if a SimpleExpressionElement is a sequence point");
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

bool SetVarElement::is_sequence_point() const {
  return m_is_sequence_point;
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
}

void ConditionElement::invert() {
  m_kind = get_condition_opposite(m_kind);
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

/////////////////////////////
// EmptyElement
/////////////////////////////

goos::Object EmptyElement::to_form(const Env& env) const {
  return pretty_print::build_list("empty");
}

void EmptyElement::apply(const std::function<void(FormElement*)>& f) {
  f(this);
}

}  // namespace decompiler
