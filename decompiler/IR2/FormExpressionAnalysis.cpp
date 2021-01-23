#include "Form.h"
#include "FormStack.h"

namespace decompiler {

namespace {

void update_var_from_stack_helper(int my_idx,
                                  Variable input,
                                  const Env& env,
                                  FormPool& pool,
                                  FormStack& stack,
                                  std::vector<FormElement*>* result) {
  auto& ri = env.reg_use().op.at(my_idx);
  if (ri.consumes.find(input.reg()) != ri.consumes.end()) {
    // is consumed.
    auto stack_val = stack.pop_reg(input);
    if (stack_val) {
      for (auto x : stack_val->elts()) {
        result->push_back(x);
      }
      return;
    }
  }
  auto elt =
      pool.alloc_element<SimpleExpressionElement>(SimpleAtom::make_var(input).as_expr(), my_idx);
  result->push_back(elt);
}
}  // namespace

void Form::update_children_from_stack(const Env& env, FormPool& pool, FormStack& stack) {
  std::vector<FormElement*> new_elts;
  for (auto& elt : m_elements) {
    elt->update_from_stack(env, pool, stack, &new_elts);
  }
  m_elements = new_elts;
}

void FormElement::update_from_stack(const Env& env,
                                    FormPool&,
                                    FormStack&,
                                    std::vector<FormElement*>*) {
  throw std::runtime_error(fmt::format("update_from_stack NYI for {}", to_string(env)));
}

void LoadSourceElement::update_from_stack(const Env& env,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result) {
  m_addr->update_children_from_stack(env, pool, stack);
  result->push_back(this);
}

void SimpleExpressionElement::update_from_stack(const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                std::vector<FormElement*>* result) {
  if (m_expr.kind() == SimpleExpression::Kind::IDENTITY) {
    auto& arg = m_expr.get_arg(0);
    if (arg.is_var()) {
      update_var_from_stack_helper(m_my_idx, arg.var(), env, pool, stack, result);
    } else if (arg.is_static_addr()) {
      // for now, do nothing.
    } else {
      throw std::runtime_error(
          fmt::format("SimpleExpressionElement::update_from_stack NYI for {}", to_string(env)));
    }
  } else {
    throw std::runtime_error(
        fmt::format("SimpleExpressionElement::update_from_stack NYI for {}", to_string(env)));
  }
}

///////////////////
// SetVarElement
///////////////////

void SetVarElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  m_src->update_children_from_stack(env, pool, stack);
  stack.push_value_to_reg(m_dst, m_src, false);
}
}  // namespace decompiler