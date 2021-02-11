#include "Form.h"
#include "FormStack.h"
#include "GenericElementMatcher.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

/*
 * TODO
 * - use var_to_form over expressions for vars
 * - check out if we can push/pop variables instead of registers?
 */

/*!
 * Basic idea: push partial expressions to the stack and pop them off as they are used.
 * Leftovers are left on the stack and flushed out as set!
 *  But the challenge is knowing it's safe to pop something off of the stack.
 *  If the value is used after the read again, then it's not.
 *
 * The tricky situation is to accidentally generate this
 *  [simplified, we would never group like this, but similar things are possible]
 * (+ s5 (begin (set! s5 z) (+ x y)))
 * when the value of s5 used is after the (set! s5 z).
 *
 * To avoid that case, we make sure that anything after s5 in the expression cannot modify s5. If it
 * does, we just don't do the expression building and leave it as smaller expressions. To accomplish
 * this, we submit a batch of registers to pop, and the stack takes care of making sure this
 * property will hold.
 *
 * But what about
 * (+ (* s5 s4) (begin (set! s5 z) (+ x y)))
 *  Luckily this isn't a problem. The actual (* s5 s4) will only be inserted if the multiply
 *  instruction actually occurs before the second term in the outer addition.
 *  The issue only occurs when a pop fails and we just insert a variable name.
 *  In other words, this variable "insert" is the only that lets us bypass ordering.
 *    (which makes me wonder if I should have solved this by being more careful with that...
 *     but I have no immediate ideas unless we allow backtracking in popping which is bad)
 *
 * Now sometimes it's too hard to figure out exactly all of the variables we might pop and do
 * it all in one batch. We pop one thing, but we know that there are registers it shouldn't modify.
 * Instead of the barrier register approach, we do something easier: explicitly forbid
 * "outside of expression register variable side effects".
 * This means that you modify a register that's visible outside of the expression. Confusingly,
 * memory access doesn't count as a side effect and register read/writes that are part of a
 * chained together expression do not count.
 * This is kind of a hacky workaround that I'd really like to remove eventually.
 * I think it can basically be solved by being strategic in when we update from stack and we can
 * avoid the highly general "update some unknown vector of unknown things from the stack"
 */

namespace decompiler {

bool Form::has_side_effects() {
  bool has_side_effect = false;
  apply([&](FormElement* elt) {
    if (dynamic_cast<SetVarElement*>(elt)) {
      has_side_effect = true;
    }
  });
  return has_side_effect;
}

bool FormElement::has_side_effects() {
  bool has_side_effect = false;
  apply([&](FormElement* elt) {
    if (dynamic_cast<SetVarElement*>(elt)) {
      has_side_effect = true;
    }
  });
  return has_side_effect;
}

namespace {

/*!
 * Create a form which represents a variable.
 */
Form* var_to_form(const Variable& var, FormPool& pool) {
  return pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(var));
}

/*!
 * Pop values of off the expression stack
 * @param vars     : list of variables to pop. In order of source code evaluation.
 * @param env      : the decompilation environment
 * @param pool     : form allocation pool
 * @param stack    : stack to pop from
 * @param output   : list of locations to push results.
 * @param consumes : if you have a different list of variables that are consumed by this operation.
 */
void pop_helper(const std::vector<Variable>& vars,
                const Env& env,
                FormPool& pool,
                FormStack& stack,
                const std::vector<std::vector<FormElement*>*>& output,
                bool allow_side_effects,
                const std::optional<RegSet>& consumes = std::nullopt) {
  // to submit to stack to attempt popping
  std::vector<Register> submit_regs;
  // submit_reg[i] is for var submit_reg_to_var[i]
  std::vector<size_t> submit_reg_to_var;

  // build submission for stack
  for (size_t var_idx = 0; var_idx < vars.size(); var_idx++) {
    const auto& var = vars.at(var_idx);
    auto& ri = env.reg_use().op.at(var.idx());
    RegSet consumes_to_use = consumes.value_or(ri.consumes);
    if (consumes_to_use.find(var.reg()) != consumes_to_use.end()) {
      // we consume the register, so it's safe to try popping.
      submit_reg_to_var.push_back(var_idx);
      submit_regs.push_back(var.reg());
    }
  }

  // submit and get a result! If the stack has nothing to pop, the result here may be nullptr.
  std::vector<Form*> pop_result;
  // loop in reverse (later vals first)
  for (size_t i = submit_regs.size(); i-- > 0;) {
    // figure out what var we are:
    auto var_idx = submit_reg_to_var.at(i);

    // anything _less_ than this should be unmodified by the pop
    // it's fine to modify yourself in your pop.
    RegSet pop_barrier_regs;
    for (size_t j = 0; j < var_idx; j++) {
      pop_barrier_regs.insert(vars.at(j).reg());
    }

    // do the pop, with the barrier to prevent out-of-sequence popping.
    pop_result.push_back(
        stack.pop_reg(submit_regs.at(i), pop_barrier_regs, env, allow_side_effects));
  }
  // now flip back to the source order for making the final result
  std::reverse(pop_result.begin(), pop_result.end());

  // final result forms. Will be nullptr if: we didn't try popping OR popping from stack failed.
  std::vector<Form*> forms;
  forms.resize(vars.size(), nullptr);
  if (!pop_result.empty()) {
    // success!
    for (size_t i = 0; i < submit_regs.size(); i++) {
      // fill out vars from our submission
      forms.at(submit_reg_to_var.at(i)) = pop_result.at(i);
    }
  }

  // write the output
  for (size_t i = 0; i < forms.size(); i++) {
    if (forms.at(i)) {
      // we got a form. inline these in the result
      for (auto x : forms.at(i)->elts()) {
        output.at(i)->push_back(x);
      }
    } else {
      // we got nothing, just insert the variable name.
      output.at(i)->push_back(pool.alloc_element<SimpleExpressionElement>(
          SimpleAtom::make_var(vars.at(i)).as_expr(), vars.at(i).idx()));
    }
  }
}

/*!
 * Pop each variable in the input list into a form. The variables should be given in the order
 * they are evaluated in the source. It is safe to put the result of these in the same expression.
 * This uses the barrier register approach, but it is only effective if you put all registers
 * appearing at the same level.
 */
std::vector<Form*> pop_to_forms(const std::vector<Variable>& vars,
                                const Env& env,
                                FormPool& pool,
                                FormStack& stack,
                                bool allow_side_effects,
                                const std::optional<RegSet>& consumes = std::nullopt) {
  std::vector<Form*> forms;
  std::vector<std::vector<FormElement*>> forms_out;
  std::vector<std::vector<FormElement*>*> form_ptrs;
  forms_out.resize(vars.size());
  form_ptrs.reserve(vars.size());
  forms.reserve(vars.size());
  for (auto& x : forms_out) {
    form_ptrs.push_back(&x);
  }

  pop_helper(vars, env, pool, stack, form_ptrs, allow_side_effects, consumes);

  for (auto& x : forms_out) {
    forms.push_back(pool.alloc_sequence_form(nullptr, x));
  }
  return forms;
}

// TODO - if we start using child classes of float/int/uint for things like degrees/meters
// we may need to adjust these.

/*!
 * type == float (exactly)?
 */
bool is_float_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("float");
}

/*!
 * type == int (exactly)?
 */
bool is_int_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("int");
}

/*!
 * type == uint (exactly)?
 */
bool is_uint_type(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec();
  return type == TypeSpec("uint");
}
}  // namespace

/*!
 * Update a form to use values from the stack. Won't push to the stack.
 * This should be used to update a Form that immediately follows something being pushed.
 * Will only change the first element of the form - anything after that will jump sequencing
 */
void Form::update_children_from_stack(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      bool allow_side_effects) {
  assert(!m_elements.empty());

  std::vector<FormElement*> new_elts;

  for (size_t i = 0; i < m_elements.size(); i++) {
    if (i == 0) {
      // only bother doing the first one.
      m_elements[i]->update_from_stack(env, pool, stack, &new_elts, allow_side_effects);
    } else {
      new_elts.push_back(m_elements[i]);
    }
  }

  for (auto& x : new_elts) {
    x->parent_form = this;
  }

  m_elements = new_elts;
}

/*!
 * Default update_from_stack for an element if no specific one is provided.
 */
void FormElement::update_from_stack(const Env& env,
                                    FormPool&,
                                    FormStack&,
                                    std::vector<FormElement*>*,
                                    bool) {
  throw std::runtime_error(fmt::format("update_from_stack NYI for {}", to_string(env)));
}

/*!
 * Update a LoadSourceElement from the stack.
 */
void LoadSourceElement::update_from_stack(const Env& env,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects) {
  m_addr->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}

void SimpleExpressionElement::update_from_stack_identity(const Env& env,
                                                         FormPool& pool,
                                                         FormStack& stack,
                                                         std::vector<FormElement*>* result,
                                                         bool allow_side_effects) {
  auto& arg = m_expr.get_arg(0);
  if (arg.is_var()) {
    pop_helper({arg.var()}, env, pool, stack, {result}, allow_side_effects);
  } else if (arg.is_static_addr()) {
    auto lab = env.file->labels.at(arg.label());
    if (env.file->is_string(lab.target_segment, lab.offset)) {
      auto str = env.file->get_goal_string(lab.target_segment, lab.offset / 4 - 1, false);
      result->push_back(pool.alloc_element<StringConstantElement>(str));
    } else {
      result->push_back(this);
    }

  } else if (arg.is_sym_ptr() || arg.is_sym_val() || arg.is_int() || arg.is_empty_list()) {
    result->push_back(this);
  } else {
    throw std::runtime_error(fmt::format(
        "SimpleExpressionElement::update_from_stack_identity NYI for {}", to_string(env)));
  }
}

void SimpleExpressionElement::update_from_stack_gpr_to_fpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(src.var().idx()).get(src.var().reg());
  std::vector<FormElement*> src_fes;
  if (src.is_var()) {
    pop_helper({src.var()}, env, pool, stack, {&src_fes}, allow_side_effects);
  } else {
    src_fes = {this};
  }

  // set ourself to identity.
  m_expr = src.as_expr();

  if (src_type.typespec() == TypeSpec("float")) {
    // got a float as an input, we can convert it to an FPR with no effect.
    for (auto x : src_fes) {
      result->push_back(x);
    }
  } else {
    // converting something else to an FPR, put an expression around it.
    result->push_back(pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::GPR_TO_FPR),
        pool.alloc_sequence_form(nullptr, src_fes)));
  }
}

void SimpleExpressionElement::update_from_stack_fpr_to_gpr(const Env& env,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto src = m_expr.get_arg(0);
  auto src_type = env.get_types_before_op(m_my_idx).get(src.var().reg());
  if (src_type.typespec() == TypeSpec("float")) {
    // set ourself to identity.
    m_expr = src.as_expr();
    // then go again.
    update_from_stack(env, pool, stack, result, allow_side_effects);
  } else {
    throw std::runtime_error(fmt::format("FPR -> GPR applied to a {}", src_type.print()));
  }
}

void SimpleExpressionElement::update_from_stack_div_s(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result,
                                                      bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    // todo - check the order here

    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::DIVISION), args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    throw std::runtime_error(fmt::format("Floating point division attempted on invalid types."));
  }
}

void SimpleExpressionElement::update_from_stack_float_2(const Env& env,
                                                        FixedOperatorKind kind,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var()) &&
      is_float_type(env, m_my_idx, m_expr.get_arg(1).var())) {
    // todo - check the order here

    auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                             allow_side_effects);
    auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                       args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    throw std::runtime_error(fmt::format("Floating point math attempted on invalid types."));
  }
}

void SimpleExpressionElement::update_from_stack_float_1(const Env& env,
                                                        FixedOperatorKind kind,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  if (is_float_type(env, m_my_idx, m_expr.get_arg(0).var())) {
    auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    auto new_form =
        pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0));
    result->push_back(new_form);
  } else {
    throw std::runtime_error(fmt::format("Floating point division attempted on invalid types."));
  }
}

void SimpleExpressionElement::update_from_stack_add_i(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result,
                                                      bool allow_side_effects) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());

  bool arg1_reg = m_expr.get_arg(1).is_var();
  bool arg1_i = true;
  bool arg1_u = true;
  if (arg1_reg) {
    arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());
    arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());
  }

  std::vector<Form*> args;

  if (arg1_reg) {
    args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                        allow_side_effects);
  } else {
    args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    args.push_back(pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_expr.get_arg(1)));
  }

  if ((arg0_i && arg1_i) || (arg0_u && arg1_u)) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION), args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    auto cast = pool.alloc_single_element_form<CastElement>(
        nullptr, TypeSpec(arg0_i ? "int" : "uint"), args.at(1));
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION), args.at(0), cast);
    result->push_back(new_form);
  }
}

void SimpleExpressionElement::update_from_stack_mult_si(const Env& env,
                                                        FormPool& pool,
                                                        FormStack& stack,
                                                        std::vector<FormElement*>* result,
                                                        bool allow_side_effects) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());

  auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                           allow_side_effects);

  if (!arg0_i) {
    args.at(0) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("int"), args.at(0));
  }

  if (!arg1_i) {
    args.at(1) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("int"), args.at(1));
  }

  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::MULTIPLICATION), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_force_si_2(const Env& env,
                                                           FixedOperatorKind kind,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  bool arg1_i = true;
  bool arg1_reg = m_expr.get_arg(1).is_var();
  if (arg1_reg) {
    arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());
  } else {
    assert(m_expr.get_arg(1).is_int());
  }

  std::vector<Form*> args;
  if (arg1_reg) {
    args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                        allow_side_effects);
  } else {
    args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    args.push_back(pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_expr.get_arg(1)));
  }

  if (!arg0_i) {
    args.at(0) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("int"), args.at(0));
  }

  if (!arg1_i) {
    args.at(1) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("int"), args.at(1));
  }

  auto new_form =
      pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_force_ui_2(const Env& env,
                                                           FixedOperatorKind kind,
                                                           FormPool& pool,
                                                           FormStack& stack,
                                                           std::vector<FormElement*>* result,
                                                           bool allow_side_effects) {
  auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
  bool arg1_u = true;
  bool arg1_reg = m_expr.get_arg(1).is_var();
  if (arg1_reg) {
    arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());
  } else {
    assert(m_expr.get_arg(1).is_int());
  }

  std::vector<Form*> args;
  if (arg1_reg) {
    args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                        allow_side_effects);
  } else {
    args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
    args.push_back(pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_expr.get_arg(1)));
  }

  if (!arg0_u) {
    args.at(0) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("uint"), args.at(0));
  }

  if (!arg1_u) {
    args.at(1) = pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("uint"), args.at(1));
  }

  auto new_form =
      pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), args.at(1));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_copy_first_int_2(const Env& env,
                                                                 FixedOperatorKind kind,
                                                                 FormPool& pool,
                                                                 FormStack& stack,
                                                                 std::vector<FormElement*>* result,
                                                                 bool allow_side_effects) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
  if (!m_expr.get_arg(1).is_var()) {
    auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);

    if (!arg0_i && !arg0_u) {
      auto new_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(kind),
          pool.alloc_single_element_form<CastElement>(nullptr, TypeSpec("int"), args.at(0)),
          pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_expr.get_arg(1)));
      result->push_back(new_form);
    } else {
      auto new_form = pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(kind), args.at(0),
          pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_expr.get_arg(1)));
      result->push_back(new_form);
    }

    return;
  }
  auto arg1_i = is_int_type(env, m_my_idx, m_expr.get_arg(1).var());
  auto arg1_u = is_uint_type(env, m_my_idx, m_expr.get_arg(1).var());

  auto args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                           allow_side_effects);

  if ((arg0_i && arg1_i) || (arg0_u && arg1_u)) {
    auto new_form = pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind),
                                                       args.at(0), args.at(1));
    result->push_back(new_form);
  } else {
    auto cast = pool.alloc_single_element_form<CastElement>(
        nullptr, TypeSpec(arg0_i ? "int" : "uint"), args.at(1));
    auto new_form =
        pool.alloc_element<GenericElement>(GenericOperator::make_fixed(kind), args.at(0), cast);
    result->push_back(new_form);
  }
}

void SimpleExpressionElement::update_from_stack_lognot(const Env& env,
                                                       FormPool& pool,
                                                       FormStack& stack,
                                                       std::vector<FormElement*>* result,
                                                       bool allow_side_effects) {
  auto args = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::LOGNOT), args.at(0));
  result->push_back(new_form);
}

void SimpleExpressionElement::update_from_stack_int_to_float(const Env& env,
                                                             FormPool& pool,
                                                             FormStack& stack,
                                                             std::vector<FormElement*>* result,
                                                             bool allow_side_effects) {
  auto var = m_expr.get_arg(0).var();
  auto arg = pop_to_forms({var}, env, pool, stack, allow_side_effects).at(0);
  // if we convert from a GPR to FPR, then immediately to int to float, we can strip away the
  // the gpr->fpr operation beacuse it doesn't matter.
  auto fpr_convert_matcher =
      Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::GPR_TO_FPR), {Matcher::any(0)});
  auto type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
  if (type == TypeSpec("int") || type == TypeSpec("uint")) {
    auto mr = match(fpr_convert_matcher, arg);
    if (mr.matched) {
      arg = mr.maps.forms.at(0);
    }
    result->push_back(pool.alloc_element<CastElement>(TypeSpec("float"), arg, true));
  } else {
    throw std::runtime_error("Used int to float on a " + type.print());
  }
}

void SimpleExpressionElement::update_from_stack(const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                std::vector<FormElement*>* result,
                                                bool allow_side_effects) {
  switch (m_expr.kind()) {
    case SimpleExpression::Kind::IDENTITY:
      update_from_stack_identity(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::GPR_TO_FPR:
      update_from_stack_gpr_to_fpr(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::FPR_TO_GPR:
      update_from_stack_fpr_to_gpr(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::DIV_S:
      update_from_stack_div_s(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::SUB_S:
      update_from_stack_float_2(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_S:
      update_from_stack_float_2(env, FixedOperatorKind::MULTIPLICATION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ADD_S:
      update_from_stack_float_2(env, FixedOperatorKind::ADDITION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::SQRT_S:
      update_from_stack_float_1(env, FixedOperatorKind::SQRT, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ABS_S:
      update_from_stack_float_1(env, FixedOperatorKind::ABS, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::NEG_S:
      update_from_stack_float_1(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ADD:
      update_from_stack_add_i(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::SUB:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_SIGNED:
      update_from_stack_mult_si(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::DIV_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::DIVISION, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::MOD_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MOD, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::MIN_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MIN, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::MAX_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MAX, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::AND:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGAND, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::OR:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGIOR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::NOR:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGNOR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::XOR:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::LOGXOR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::LOGNOT:
      update_from_stack_lognot(env, pool, stack, result, allow_side_effects);
      break;
    case SimpleExpression::Kind::LEFT_SHIFT:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SHL, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_LOGIC:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SHR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::RIGHT_SHIFT_ARITH:
      update_from_stack_copy_first_int_2(env, FixedOperatorKind::SAR, pool, stack, result,
                                         allow_side_effects);
      break;
    case SimpleExpression::Kind::MUL_UNSIGNED:
      update_from_stack_force_ui_2(env, FixedOperatorKind::MULTIPLICATION, pool, stack, result,
                                   allow_side_effects);
      break;
    case SimpleExpression::Kind::INT_TO_FLOAT:
      update_from_stack_int_to_float(env, pool, stack, result, allow_side_effects);
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
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
  assert(m_src->parent_element == this);
  m_src->update_children_from_stack(env, pool, stack, true);
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        auto var = src_as_se->expr().get_arg(0).var();
        auto& info = env.reg_use().op.at(var.idx());
        if (info.consumes.find(var.reg()) != info.consumes.end()) {
          stack.push_non_seq_reg_to_reg(m_dst, src_as_se->expr().get_arg(0).var(), m_src,
                                        m_is_eliminated_coloring_move);
          return;
        }
      }
    }
  }

  stack.push_value_to_reg(m_dst, m_src, true, m_is_eliminated_coloring_move);
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
}

void SetVarElement::update_from_stack(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      std::vector<FormElement*>* result,
                                      bool allow_side_effects) {
  m_src->update_children_from_stack(env, pool, stack, allow_side_effects);
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
  result->push_back(this);
}

void SetFormFormElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  // todo - is the order here right?
  m_src->update_children_from_stack(env, pool, stack, false);
  m_dst->update_children_from_stack(env, pool, stack, false);
  stack.push_form_element(this, true);
}

///////////////////
// AshElement
///////////////////

void AshElement::update_from_stack(const Env& env,
                                   FormPool& pool,
                                   FormStack& stack,
                                   std::vector<FormElement*>* result,
                                   bool allow_side_effects) {
  auto forms = pop_to_forms({value, shift_amount}, env, pool, stack, allow_side_effects, consumed);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::ARITH_SHIFT), forms.at(0), forms.at(1));
  result->push_back(new_form);
}

///////////////////
// AbsElement
///////////////////

void AbsElement::update_from_stack(const Env& env,
                                   FormPool& pool,
                                   FormStack& stack,
                                   std::vector<FormElement*>* result,
                                   bool allow_side_effects) {
  auto forms = pop_to_forms({source}, env, pool, stack, allow_side_effects, consumed);
  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(FixedOperatorKind::ABS), forms.at(0));
  result->push_back(new_form);
}

///////////////////
// FunctionCallElement
///////////////////

void FunctionCallElement::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool allow_side_effects) {
  std::vector<Form*> args;
  auto nargs = m_op->arg_vars().size();
  args.resize(nargs, nullptr);

  std::vector<Variable> all_pop_vars = {m_op->function_var()};
  for (size_t i = 0; i < nargs; i++) {
    all_pop_vars.push_back(m_op->arg_vars().at(i));
  }
  auto unstacked = pop_to_forms(all_pop_vars, env, pool, stack, allow_side_effects);
  std::vector<Form*> arg_forms;
  TypeSpec function_type;
  if (env.has_type_analysis()) {
    function_type =
        env.get_types_before_op(all_pop_vars.at(0).idx()).get(all_pop_vars.at(0).reg()).typespec();
  }

  for (size_t arg_id = 0; arg_id < nargs; arg_id++) {
    auto val = unstacked.at(arg_id + 1);  // first is the function itself.
    auto& var = all_pop_vars.at(arg_id + 1);
    if (env.has_type_analysis() && function_type.arg_count() == nargs + 1) {
      auto actual_arg_type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
      auto desired_arg_type = function_type.get_arg(arg_id);
      if (!env.dts->ts.typecheck(desired_arg_type, actual_arg_type, "", false, false)) {
        arg_forms.push_back(
            pool.alloc_single_element_form<CastElement>(nullptr, desired_arg_type, val));
      } else {
        arg_forms.push_back(val);
      }
    } else {
      arg_forms.push_back(val);
    }
  }

  auto new_form = pool.alloc_element<GenericElement>(
      GenericOperator::make_function(unstacked.at(0)), arg_forms);

  {
    // detect method calls:
    // ex: ((-> pair methods-by-name new) (quote global) pair gp-0 a3-0)
    constexpr int type_for_method = 0;
    constexpr int method_name = 1;

    auto deref_matcher = Matcher::deref(
        Matcher::any_symbol(type_for_method), false,
        {DerefTokenMatcher::string("methods-by-name"), DerefTokenMatcher::any_string(method_name)});

    auto matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher), {});
    auto temp_form = pool.alloc_single_form(nullptr, new_form);
    auto match_result = match(matcher, temp_form);
    if (match_result.matched) {
      auto type_1 = match_result.maps.strings.at(type_for_method);
      auto name = match_result.maps.strings.at(method_name);

      if (name == "new" && type_1 == "object") {
        // calling the new method of object. This is a special case that turns into an (object-new
        // macro. The arguments are allocation type-to-make and size of type
        // symbol, type, int.
        std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();

        // if needed, cast to to correct type.
        std::vector<TypeSpec> expected_arg_types = {TypeSpec("symbol"), TypeSpec("type"),
                                                    TypeSpec("int")};
        assert(new_args.size() >= 3);
        for (size_t i = 0; i < 3; i++) {
          auto& var = all_pop_vars.at(i + 1);  // 0 is the function itself.
          auto arg_type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
          if (!env.dts->ts.typecheck(expected_arg_types.at(i), arg_type, "", false, false)) {
            new_args.at(i) = pool.alloc_single_element_form<CastElement>(
                nullptr, expected_arg_types.at(i), new_args.at(i));
          }
        }

        auto new_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::OBJECT_NEW), new_args);
        result->push_back(new_op);
        return;
      }
      if (name == "new" && type_1 == "type") {
        std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();
        auto new_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::TYPE_NEW), new_args);
        result->push_back(new_op);
        return;
      } else if (name == "new") {
        constexpr int allocation = 2;
        constexpr int type_for_arg = 3;
        auto alloc_matcher = Matcher::any_quoted_symbol(allocation);
        auto type_arg_matcher = Matcher::any_symbol(type_for_arg);
        matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher),
                                        {alloc_matcher, type_arg_matcher});
        match_result = match(matcher, temp_form);
        if (match_result.matched) {
          auto alloc = match_result.maps.strings.at(allocation);
          if (alloc != "global" && alloc != "debug") {
            throw std::runtime_error("Unrecognized heap symbol for new: " + alloc);
          }
          auto type_2 = match_result.maps.strings.at(type_for_arg);
          if (type_1 != type_2) {
            throw std::runtime_error(
                fmt::format("Inconsistent types in method call: {} and {}", type_1, type_2));
          }

          auto quoted_type = pool.alloc_single_element_form<SimpleAtomElement>(
              nullptr, SimpleAtom::make_sym_ptr(type_2));

          std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();
          new_args.at(1) = quoted_type;

          auto new_op = pool.alloc_element<GenericElement>(
              GenericOperator::make_fixed(FixedOperatorKind::NEW), new_args);
          result->push_back(new_op);
          return;
        } else {
          throw std::runtime_error("Failed to match new method");
        }
      } else {
        throw std::runtime_error("Method call detected, not yet implemented");
      }
    }
  }

  {
    // detect method calls:
    // ex: ((-> XXX methods-by-name new) (quote global) pair gp-0 a3-0)
    constexpr int method_name = 0;
    constexpr int type_source = 1;

    auto deref_matcher = Matcher::deref(
        Matcher::any(type_source), false,
        {DerefTokenMatcher::string("methods-by-name"), DerefTokenMatcher::any_string(method_name)});

    auto matcher = Matcher::op_with_rest(GenericOpMatcher::func(deref_matcher), {});
    auto temp_form = pool.alloc_single_form(nullptr, new_form);
    auto match_result = match(matcher, temp_form);
    if (match_result.matched) {
      auto name = match_result.maps.strings.at(method_name);
      auto type_source_form = match_result.maps.forms.at(type_source);
      auto method_op =
          pool.alloc_single_element_form<GetMethodElement>(nullptr, type_source_form, name, false);
      auto gop = GenericOperator::make_function(method_op);

      result->push_back(pool.alloc_element<GenericElement>(gop, arg_forms));
      return;
    }
  }

  result->push_back(new_form);
}

void FunctionCallElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  std::vector<FormElement*> rewritten;
  update_from_stack(env, pool, stack, &rewritten, true);
  for (auto x : rewritten) {
    stack.push_form_element(x, true);
  }
}

///////////////////
// DerefElement
///////////////////
void DerefElement::update_from_stack(const Env& env,
                                     FormPool& pool,
                                     FormStack& stack,
                                     std::vector<FormElement*>* result,
                                     bool allow_side_effects) {
  // todo - update var tokens from stack?
  m_base->update_children_from_stack(env, pool, stack, allow_side_effects);
  auto as_deref = dynamic_cast<DerefElement*>(m_base->try_as_single_element());
  if (as_deref) {
    if (!m_is_addr_of && !as_deref->is_addr_of()) {
      m_tokens.insert(m_tokens.begin(), as_deref->tokens().begin(), as_deref->tokens().end());
      m_base = as_deref->m_base;
    }
  }
  result->push_back(this);
}

///////////////////
// UntilElement
///////////////////

void UntilElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  for (auto form : {condition, body}) {
    FormStack temp_stack;
    for (auto& entry : form->elts()) {
      entry->push_to_stack(env, pool, temp_stack);
    }
    auto new_entries = temp_stack.rewrite(pool);
    form->clear();
    for (auto e : new_entries) {
      form->push_back(e);
    }
  }

  stack.push_form_element(this, true);
}

void WhileElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  for (auto form : {condition, body}) {
    FormStack temp_stack;
    for (auto& entry : form->elts()) {
      entry->push_to_stack(env, pool, temp_stack);
    }
    auto new_entries = temp_stack.rewrite(pool);
    form->clear();
    for (auto e : new_entries) {
      form->push_back(e);
    }
  }
  stack.push_form_element(this, true);
}

///////////////////
// CondNoElseElement
///////////////////
void CondNoElseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  if (already_rewritten) {
    stack.push_form_element(this, true);
  }
  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      FormStack temp_stack;
      for (auto& elt : form->elts()) {
        elt->push_to_stack(env, pool, temp_stack);
      }

      std::vector<FormElement*> new_entries;
      if (form == entry.body && used_as_value) {
        new_entries = temp_stack.rewrite_to_get_var(pool, final_destination, env);
      } else {
        new_entries = temp_stack.rewrite(pool);
      }

      form->clear();
      for (auto e : new_entries) {
        form->push_back(e);
      }
    }
  }

  if (used_as_value) {
    stack.push_value_to_reg(final_destination, pool.alloc_single_form(nullptr, this), true);
  } else {
    stack.push_form_element(this, true);
  }
  already_rewritten = true;
}

void CondWithElseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  if (already_rewritten) {
    stack.push_form_element(this, true);
  }
  // first, let's try to detect if all bodies write the same value
  std::optional<Variable> last_var;
  bool rewrite_as_set = true;

  // collect all forms which should write the output.
  std::vector<Form*> write_output_forms;
  for (const auto& entry : entries) {
    write_output_forms.push_back(entry.body);
  }
  write_output_forms.push_back(else_ir);

  // check all to see if they write the value.
  for (auto form : write_output_forms) {
    auto last_in_body = dynamic_cast<SetVarElement*>(form->elts().back());
    if (last_in_body) {
      if (last_var.has_value()) {
        if (last_var->reg() != last_in_body->dst().reg()) {
          rewrite_as_set = false;
          break;
        }
      }
      last_var = last_in_body->dst();
    }
  }

  if (!last_var.has_value()) {
    rewrite_as_set = false;
  }

  // determine if set destination is used
  bool set_unused = false;
  if (rewrite_as_set) {
    auto& info = env.reg_use().op.at(last_var->idx());
    if (info.written_and_unused.find(last_var->reg()) != info.written_and_unused.end()) {
      set_unused = true;
    }
  }

  // process everything.
  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      FormStack temp_stack;
      for (auto& elt : form->elts()) {
        elt->push_to_stack(env, pool, temp_stack);
      }

      std::vector<FormElement*> new_entries;
      if (form == entry.body && rewrite_as_set) {
        new_entries = temp_stack.rewrite_to_get_var(pool, *last_var, env);
      } else {
        new_entries = temp_stack.rewrite(pool);
      }

      form->clear();
      for (auto e : new_entries) {
        form->push_back(e);
      }
    }
  }

  FormStack temp_stack;
  for (auto& elt : else_ir->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  if (rewrite_as_set) {
    new_entries = temp_stack.rewrite_to_get_var(pool, *last_var, env);
  } else {
    new_entries = temp_stack.rewrite(pool);
  }

  else_ir->clear();
  for (auto e : new_entries) {
    else_ir->push_back(e);
  }

  auto top_condition = entries.front().condition;
  if (!top_condition->is_single_element()) {
    auto real_condition = top_condition->back();
    top_condition->pop_back();
    for (auto x : top_condition->elts()) {
      x->push_to_stack(env, pool, stack);
    }
    top_condition->elts() = {real_condition};
  }

  if (rewrite_as_set) {
    if (set_unused) {
      stack.push_form_element(this, true);
    } else {
      stack.push_value_to_reg(*last_var, pool.alloc_single_form(nullptr, this), true);
    }
  } else {
    stack.push_form_element(this, true);
  }
  already_rewritten = true;
}

///////////////////
// ShortCircuitElement
///////////////////

void ShortCircuitElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  if (!used_as_value.value_or(false)) {
    throw std::runtime_error(
        "ShortCircuitElement::push_to_stack not implemented for result not used case.");

    stack.push_form_element(this, true);
  } else {
    if (already_rewritten) {
      stack.push_form_element(this, true);
    }
    for (int i = 0; i < int(entries.size()); i++) {
      auto& entry = entries.at(i);
      FormStack temp_stack;
      for (auto& elt : entry.condition->elts()) {
        elt->push_to_stack(env, pool, temp_stack);
      }

      std::vector<FormElement*> new_entries;
      if (i == int(entries.size()) - 1) {
        new_entries = temp_stack.rewrite_to_get_var(pool, final_result, env);
      } else {
        new_entries = temp_stack.rewrite(pool);
      }

      entry.condition->clear();
      for (auto e : new_entries) {
        entry.condition->push_back(e);
      }
    }
    assert(used_as_value.has_value());
    stack.push_value_to_reg(final_result, pool.alloc_single_form(nullptr, this), true);
    already_rewritten = true;
  }
}

void ShortCircuitElement::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool) {
  (void)stack;
  if (already_rewritten) {
    result->push_back(this);
    return;
  }
  for (int i = 0; i < int(entries.size()); i++) {
    auto& entry = entries.at(i);
    FormStack temp_stack;
    for (auto& elt : entry.condition->elts()) {
      elt->push_to_stack(env, pool, temp_stack);
    }

    std::vector<FormElement*> new_entries;
    if (i == int(entries.size()) - 1) {
      new_entries = temp_stack.rewrite_to_get_var(pool, final_result, env);
    } else {
      new_entries = temp_stack.rewrite(pool);
    }

    entry.condition->clear();
    for (auto e : new_entries) {
      entry.condition->push_back(e);
    }
  }
  result->push_back(this);
  already_rewritten = true;
}

///////////////////
// ConditionElement
///////////////////

namespace {
Form* make_cast(Form* in, const TypeSpec& in_type, const TypeSpec& out_type, FormPool& pool) {
  if (in_type == out_type) {
    return in;
  }
  return pool.alloc_single_element_form<CastElement>(nullptr, out_type, in);
}

std::vector<Form*> make_cast(const std::vector<Form*>& in,
                             const std::vector<TypeSpec>& in_types,
                             const TypeSpec& out_type,
                             FormPool& pool) {
  std::vector<Form*> out;
  assert(in.size() == in_types.size());
  for (size_t i = 0; i < in_types.size(); i++) {
    out.push_back(make_cast(in.at(i), in_types.at(i), out_type, pool));
  }
  return out;
}
}  // namespace

FormElement* ConditionElement::make_generic(const Env&,
                                            FormPool& pool,
                                            const std::vector<Form*>& source_forms,
                                            const std::vector<TypeSpec>& types) {
  switch (m_kind) {
    case IR2_Condition::Kind::TRUTHY:
    case IR2_Condition::Kind::ZERO:
    case IR2_Condition::Kind::NONZERO:
    case IR2_Condition::Kind::FALSE:
    case IR2_Condition::Kind::IS_PAIR:
    case IR2_Condition::Kind::IS_NOT_PAIR:
      // kind of a hack, we fall back to the old condition operator which is special cased
      // to print the truthy condition in a nice way. and we use it for other things that don't
      // require fancy renaming.
      return pool.alloc_element<GenericElement>(GenericOperator::make_compare(m_kind),
                                                source_forms);
    case IR2_Condition::Kind::EQUAL:
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::EQ),
                                                source_forms);
    case IR2_Condition::Kind::NOT_EQUAL:
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NEQ),
                                                source_forms);

    case IR2_Condition::Kind::LESS_THAN_SIGNED:
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::LT),
          make_cast(source_forms, types, TypeSpec("int"), pool));
    case IR2_Condition::Kind::LESS_THAN_UNSIGNED:
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::LT),
          make_cast(source_forms, types, TypeSpec("uint"), pool));

    case IR2_Condition::Kind::GEQ_UNSIGNED:
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GEQ),
          make_cast(source_forms, types, TypeSpec("uint"), pool));

    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("int"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                                casted);
    }

    case IR2_Condition::Kind::GEQ_ZERO_SIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("int"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GEQ),
                                                casted);
    }

    case IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("int"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GT),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_NOT_EQUAL: {
      auto casted = make_cast(source_forms, types, TypeSpec("float"), pool);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NEQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_EQUAL: {
      auto casted = make_cast(source_forms, types, TypeSpec("float"), pool);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::EQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_LEQ: {
      auto casted = make_cast(source_forms, types, TypeSpec("float"), pool);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LEQ),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_LESS_THAN: {
      auto casted = make_cast(source_forms, types, TypeSpec("float"), pool);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                                casted);
    }

    case IR2_Condition::Kind::FLOAT_GEQ: {
      auto casted = make_cast(source_forms, types, TypeSpec("float"), pool);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GEQ),
                                                casted);
    }

    default:
      throw std::runtime_error("ConditionElement::make_generic NYI for kind " +
                               get_condition_kind_name(m_kind));
  }
}

void ConditionElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  std::vector<Form*> source_forms;
  std::vector<TypeSpec> source_types;
  std::vector<Variable> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    auto& var = m_src[i]->var();
    vars.push_back(var);
    source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
  }
  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }

  source_forms = pop_to_forms(vars, env, pool, stack, true, m_consumed);
  if (m_flipped) {
    std::reverse(source_forms.begin(), source_forms.end());
  }

  stack.push_form_element(make_generic(env, pool, source_forms, source_types), true);
}

void ConditionElement::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  std::vector<Form*> source_forms;
  std::vector<TypeSpec> source_types;
  std::vector<Variable> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    auto& var = m_src[i]->var();
    vars.push_back(var);
    source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
  }

  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }
  source_forms = pop_to_forms(vars, env, pool, stack, allow_side_effects, m_consumed);
  if (m_flipped) {
    std::reverse(source_forms.begin(), source_forms.end());
  }

  result->push_back(make_generic(env, pool, source_forms, source_types));
}

void ReturnElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  FormStack temp_stack;
  for (auto& elt : return_code->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  new_entries = temp_stack.rewrite_to_get_var(pool, env.end_var(), env);

  return_code->clear();
  for (auto e : new_entries) {
    return_code->push_back(e);
  }
  stack.push_form_element(this, true);
}

void AtomicOpElement::push_to_stack(const Env& env, FormPool&, FormStack& stack) {
  auto as_end = dynamic_cast<const FunctionEndOp*>(m_op);
  if (as_end) {
    // we don't want to push this to the stack (for now at least)
    return;
  }

  auto as_special = dynamic_cast<const SpecialOp*>(m_op);
  if (as_special) {
    if (as_special->kind() == SpecialOp::Kind::NOP) {
      stack.push_form_element(this, true);
      return;
    }
  }

  auto as_asm = dynamic_cast<const AsmOp*>(m_op);
  if (as_asm) {
    stack.push_form_element(this, true);
    return;
  }
  throw std::runtime_error("Can't push atomic op to stack: " + m_op->to_string(env));
}

void AsmOpElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  stack.push_form_element(this, true);
}

void GenericElement::update_from_stack(const Env& env,
                                       FormPool& pool,
                                       FormStack& stack,
                                       std::vector<FormElement*>* result,
                                       bool) {
  for (auto it = m_elts.rbegin(); it != m_elts.rend(); it++) {
    (*it)->update_children_from_stack(env, pool, stack, false);
  }

  if (m_head.m_kind == GenericOperator::Kind::FUNCTION_EXPR) {
    m_head.m_function->update_children_from_stack(env, pool, stack, false);
  }
  result->push_back(this);
}

void GenericElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  (void)env;
  (void)pool;
  stack.push_form_element(this, true);
}

////////////////////////
// DynamicMethodAccess
////////////////////////

void DynamicMethodAccess::update_from_stack(const Env& env,
                                            FormPool& pool,
                                            FormStack& stack,
                                            std::vector<FormElement*>* result,
                                            bool allow_side_effects) {
  auto new_val = stack.pop_reg(m_source, {}, env, allow_side_effects);
  auto reg0_matcher =
      Matcher::match_or({Matcher::any_reg(0), Matcher::cast("uint", Matcher::any_reg(0))});
  auto reg1_matcher =
      Matcher::match_or({Matcher::any_reg(1), Matcher::cast("int", Matcher::any_reg(1))});

  // (+ (sll (the-as uint a1-0) 2) (the-as int a0-0))
  auto sll_matcher = Matcher::fixed_op(FixedOperatorKind::SHL, {reg0_matcher, Matcher::integer(2)});
  auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {sll_matcher, reg1_matcher});
  auto match_result = match(matcher, new_val);
  if (!match_result.matched) {
    throw std::runtime_error("Couldn't match DynamicMethodAccess values: " +
                             new_val->to_string(env));
  }

  auto idx = match_result.maps.regs.at(0);
  auto base = match_result.maps.regs.at(1);
  assert(idx.has_value() && base.has_value());

  auto deref = pool.alloc_element<DerefElement>(
      var_to_form(base.value(), pool), false,
      std::vector<DerefToken>{DerefToken::make_field_name("method-table"),
                              DerefToken::make_int_expr(var_to_form(idx.value(), pool))});
  result->push_back(deref);
}

////////////////////////
// ArrayFieldAccess
////////////////////////

namespace {
bool is_power_of_two(int in, int* out) {
  int x = 1;
  for (int i = 0; i < 32; i++) {
    if (x == in) {
      *out = i;
      return true;
    }
    x = x * 2;
  }
  return false;
}
}  // namespace

void ArrayFieldAccess::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  auto new_val = stack.pop_reg(m_source, {}, env, allow_side_effects);
  int power_of_two = 0;

  if (m_constant_offset == 0) {
    if (m_expected_stride == 1) {
      throw std::runtime_error("One case, not yet implemented (no offset)");
    } else if (is_power_of_two(m_expected_stride, &power_of_two)) {
      // reg0 is base
      // reg1 is idx

      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(1)), Matcher::any(1)});
      auto sll_matcher =
          Matcher::fixed_op(FixedOperatorKind::SHL, {reg1_matcher, Matcher::integer(power_of_two)});
      sll_matcher = Matcher::match_or({Matcher::cast("uint", sll_matcher), sll_matcher});
      auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {reg0_matcher, sll_matcher});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        fmt::print("power {}\n", power_of_two);
        throw std::runtime_error(
            "Couldn't match ArrayFieldAccess (stride power of 2, 0 offset) values: " +
            new_val->to_string(env));
      }

      auto idx = match_result.maps.forms.at(1);
      auto base = match_result.maps.forms.at(0);
      assert(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      //      tokens.push_back(DerefToken::make_int_expr(idx));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else {
      throw std::runtime_error("Not power of two case, not yet implemented (no offset)");
    }
  } else {
    if (m_expected_stride == 1) {
      // reg0 is idx
      auto reg0_matcher =
          Matcher::match_or({Matcher::any_reg(0), Matcher::cast("int", Matcher::any_reg(0))});
      // reg1 is base
      auto reg1_matcher =
          Matcher::match_or({Matcher::any_reg(1), Matcher::cast("int", Matcher::any_reg(1))});
      auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {reg0_matcher, reg1_matcher});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        throw std::runtime_error("Couldn't match ArrayFieldAccess (stride 1) values: " +
                                 new_val->to_string(env));
      }
      auto idx = match_result.maps.regs.at(0);
      auto base = match_result.maps.regs.at(1);
      assert(idx.has_value() && base.has_value());

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(var_to_form(idx.value(), pool));
        }
      }
      // tokens.push_back(DerefToken::make_int_expr(var_to_form(idx.value(), pool)));

      auto deref = pool.alloc_element<DerefElement>(var_to_form(base.value(), pool), false, tokens);
      result->push_back(deref);
    } else if (is_power_of_two(m_expected_stride, &power_of_two)) {
      // (+ (sll (the-as uint a1-0) 2) (the-as int a0-0))
      // (+ gp-0 (the-as uint (shl (the-as uint (shl (the-as uint s4-0) 2)) 2)))
      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(1)),
                             Matcher::cast("int", Matcher::any(1)), Matcher::any(1)});
      auto sll_matcher =
          Matcher::fixed_op(FixedOperatorKind::SHL, {reg0_matcher, Matcher::integer(power_of_two)});
      sll_matcher = Matcher::match_or({Matcher::cast("uint", sll_matcher), sll_matcher});
      auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {sll_matcher, reg1_matcher});
      auto match_result = match(matcher, new_val);
      // TODO - figure out why it sometimes happens the other way.
      if (!match_result.matched) {
        matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {reg1_matcher, sll_matcher});
        match_result = match(matcher, new_val);
        if (!match_result.matched) {
          throw std::runtime_error("Couldn't match ArrayFieldAccess (stride power of 2) values: " +
                                   new_val->to_string(env));
        }
      }
      auto idx = match_result.maps.forms.at(0);
      auto base = match_result.maps.forms.at(1);

      assert(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }
      // tokens.push_back(DerefToken::make_int_expr(var_to_form(idx.value(), pool)));

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    } else {
      throw std::runtime_error("Not power of two case, not yet implemented (offset)");
    }
  }
}

////////////////////////
// CastElement
////////////////////////

void CastElement::update_from_stack(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects) {
  m_source->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}

////////////////////////
// TypeOfElement
////////////////////////

void TypeOfElement::update_from_stack(const Env& env,
                                      FormPool& pool,
                                      FormStack& stack,
                                      std::vector<FormElement*>* result,
                                      bool allow_side_effects) {
  value->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}

////////////////////////
// EmptyElement
////////////////////////

void EmptyElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  stack.push_form_element(this, true);
}

void ConditionalMoveFalseElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  stack.push_form_element(this, true);
}

}  // namespace decompiler
