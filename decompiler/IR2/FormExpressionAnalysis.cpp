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

Form* update_var_from_stack_to_form(int my_idx,
                                    Variable input,
                                    const Env& env,
                                    FormPool& pool,
                                    FormStack& stack) {
  std::vector<FormElement*> elts;
  update_var_from_stack_helper(my_idx, input, env, pool, stack, &elts);
  return pool.alloc_sequence_form(nullptr, elts);
}

bool is_float_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("float");
}

bool is_int_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("int");
}

bool is_uint_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("uint");
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

void SimpleExpressionElement::update_from_stack_identity(const Env& env,
                                                         FormPool& pool,
                                                         FormStack& stack,
                                                         std::vector<FormElement*>* result) {
  auto& arg = m_expr.get_arg(0);
  if (arg.is_var()) {
    update_var_from_stack_helper(m_my_idx, arg.var(), env, pool, stack, result);
  } else if (arg.is_static_addr()) {
    // for now, do nothing.
    result->push_back(this);
  } else {
    throw std::runtime_error(
        fmt::format("SimpleExpressionElement::update_from_stack NYI for {}", to_string(env)));
  }
}

void SimpleExpressionElement::update_from_stack_gpr_to_fpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(m_my_idx).get(src.var().reg());
  if (src_type.typespec() == TypeSpec("float")) {
    // set ourself to identity.
    m_expr = src.as_expr();
    // then go again.
    update_from_stack(env, pool, stack, result);
  } else {
    throw std::runtime_error(fmt::format("GPR -> FPR applied to a {}", src_type.print()));
  }
}

void SimpleExpressionElement::update_from_stack_fpr_to_gpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(m_my_idx).get(src.var().reg());
  if (src_type.typespec() == TypeSpec("float")) {
    // set ourself to identity.
    m_expr = src.as_expr();
    // then go again.
    update_from_stack(env, pool, stack, result);
  } else {
    throw std::runtime_error(fmt::format("FPR -> GPR applied to a {}", src_type.print()));
  }
}

void SimpleExpressionElement::update_from_stack_div_s(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    // todo - check the order here
    auto arg0 = update_var_from_stack_to_form(m_my_idx, m_expr.get_arg(0).var(), env, pool, stack);
    auto arg1 = update_var_from_stack_to_form(m_my_idx, m_expr.get_arg(1).var(), env, pool, stack);
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::DIVISION), arg0, arg1);
    result->push_back(new_form);
  } else {
    throw std::runtime_error(fmt::format("Floating point division attempted on invalid types."));
  }
}

void SimpleExpressionElement::update_from_stack_add_i(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());
  auto arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());

  auto arg0 = update_var_from_stack_to_form(m_my_idx, m_expr.get_arg(0).var(), env, pool, stack);
  auto arg1 = update_var_from_stack_to_form(m_my_idx, m_expr.get_arg(1).var(), env, pool, stack);

  if ((arg0_i && arg1_i) || (arg0_u && arg1_u)) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION), arg0, arg1);
    result->push_back(new_form);
  } else {
    auto cast = pool.alloc_single_element_form<CastElement>(
        nullptr, TypeSpec(arg0_i ? "int" : "uint"), arg1);
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION), arg0, cast);
    result->push_back(new_form);
  }
}

void SimpleExpressionElement::update_from_stack(const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                std::vector<FormElement*>* result) {
  switch (m_expr.kind()) {
    case SimpleExpression::Kind::IDENTITY:
      update_from_stack_identity(env, pool, stack, result);
      break;
    case SimpleExpression::Kind::GPR_TO_FPR:
      update_from_stack_gpr_to_fpr(env, pool, stack, result);
      break;
    case SimpleExpression::Kind::FPR_TO_GPR:
      update_from_stack_fpr_to_gpr(env, pool, stack, result);
      break;
    case SimpleExpression::Kind::DIV_S:
      update_from_stack_div_s(env, pool, stack, result);
      break;
    case SimpleExpression::Kind::ADD:
      update_from_stack_add_i(env, pool, stack, result);
      break;
    default:
      throw std::runtime_error(
          fmt::format("SimpleExpressionElement::update_from_stack NYI for {}", to_string(env)));
  }
}

///////////////////
// SetVarElement
///////////////////

void SetVarElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  m_src->update_children_from_stack(env, pool, stack);
  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        stack.push_value_to_reg(m_dst, m_src, false);
        return;
      }
    }
  }

  stack.push_value_to_reg(m_dst, m_src, true);
}
}  // namespace decompiler