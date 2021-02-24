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
  std::unordered_map<Register, int, Register::hash> reg_counts;
  for (auto& v : vars) {
    reg_counts[v.reg()]++;
  }

  for (size_t var_idx = 0; var_idx < vars.size(); var_idx++) {
    const auto& var = vars.at(var_idx);
    auto& ri = env.reg_use().op.at(var.idx());
    RegSet consumes_to_use = consumes.value_or(ri.consumes);
    if (consumes_to_use.find(var.reg()) != consumes_to_use.end()) {
      if (reg_counts.at(var.reg()) == 1) {
        // we consume the register, so it's safe to try popping.
        submit_reg_to_var.push_back(var_idx);
        submit_regs.push_back(var.reg());
      }
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

bool is_ptr_or_child(const Env& env, int my_idx, Variable var) {
  auto type = env.get_types_before_op(my_idx).get(var.reg()).typespec().base_type();
  return type == "pointer";
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
      if (!m_elements[i]->is_popped()) {
        m_elements[i]->update_from_stack(env, pool, stack, &new_elts, allow_side_effects);
      } else {
        new_elts.push_back(m_elements[i]);
      }

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

/*!
 * Update a LoadSourceElement from the stack.
 */
void LoadSourceElement::update_from_stack(const Env& env,
                                          FormPool& pool,
                                          FormStack& stack,
                                          std::vector<FormElement*>* result,
                                          bool allow_side_effects) {
  mark_popped();
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
  if (src_type.typespec() == TypeSpec("float") || src_type.typespec() == TypeSpec("int")) {
    // set ourself to identity.
    m_expr = src.as_expr();
    // then go again.
    assert(m_popped);
    m_popped = false;
    update_from_stack(env, pool, stack, result, allow_side_effects);
  } else {
    throw std::runtime_error(
        fmt::format("FPR -> GPR applied to a {} in {}", src_type.print(), to_string(env)));
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
    auto type0 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg());
    auto type1 = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg());
    throw std::runtime_error(
        fmt::format("Floating point math attempted on invalid types: {} and {} in op {}.",
                    type0.print(), type1.print(), to_string(env)));
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

void SimpleExpressionElement::update_from_stack_si_1(const Env& env,
                                                     FixedOperatorKind kind,
                                                     FormPool& pool,
                                                     FormStack& stack,
                                                     std::vector<FormElement*>* result,
                                                     bool allow_side_effects) {
  auto in_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg()).typespec();
  auto arg = pop_to_forms({m_expr.get_arg(0).var()}, env, pool, stack, allow_side_effects).at(0);
  result->push_back(pool.alloc_element<GenericElement>(
      GenericOperator::make_fixed(kind), make_cast(arg, in_type, TypeSpec("int"), pool)));
}

void SimpleExpressionElement::update_from_stack_add_i(const Env& env,
                                                      FormPool& pool,
                                                      FormStack& stack,
                                                      std::vector<FormElement*>* result,
                                                      bool allow_side_effects) {
  auto arg0_i = is_int_type(env, m_my_idx, m_expr.get_arg(0).var());
  auto arg0_u = is_uint_type(env, m_my_idx, m_expr.get_arg(0).var());
  bool arg0_ptr = is_ptr_or_child(env, m_my_idx, m_expr.get_arg(0).var());

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

  // Look for getting an address inside of an object.
  // (+ <integer 108 + int> process). array style access with a stride of 1.
  // in the case, both are vars.
  if (arg1_reg) {
    // lookup types.
    auto arg1_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(1).var().reg());
    auto arg0_type = env.get_types_before_op(m_my_idx).get(m_expr.get_arg(0).var().reg());
    if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR) {
      // try to see if this is valid, from the type system.
      FieldReverseLookupInput input;
      input.offset = arg0_type.get_integer_constant();
      input.stride = 1;
      input.base_type = arg1_type.typespec();
      auto out = env.dts->ts.reverse_field_lookup(input);
      if (out.success) {
        // it is. now we have to modify things
        // first, look for the index
        auto arg0_matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                        {Matcher::any(0), Matcher::integer(input.offset)});
        auto match_result = match(arg0_matcher, args.at(0));
        if (match_result.matched) {
          bool used_index = false;
          std::vector<DerefToken> tokens;
          for (auto& tok : out.tokens) {
            if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
              assert(!used_index);
              used_index = true;
              tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
            } else {
              tokens.push_back(to_token(tok));
            }
          }
          result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
          return;
        } else {
          throw std::runtime_error("Failed to match for stride 1 address access with add.");
        }
      }
    } else if (arg0_type.kind == TP_Type::Kind::INTEGER_CONSTANT_PLUS_VAR_MULT) {
      // try to see if this is valid, from the type system.
      FieldReverseLookupInput input;
      input.offset = arg0_type.get_add_int_constant();
      input.stride = arg0_type.get_mult_int_constant();
      input.base_type = arg1_type.typespec();
      auto out = env.dts->ts.reverse_field_lookup(input);
      if (out.success) {
        // it is. now we have to modify things
        // first, look for the index
        int p2;
        if (is_power_of_two(input.stride, &p2)) {
          // (+ (shl (-> a0-0 reg-count) 3) 28)
          auto arg0_matcher =
              Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                          {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::SHL),
                                       {Matcher::any(0), Matcher::integer(p2)}),
                           Matcher::integer(input.offset)});
          auto match_result = match(arg0_matcher, args.at(0));
          if (match_result.matched) {
            bool used_index = false;
            std::vector<DerefToken> tokens;
            for (auto& tok : out.tokens) {
              if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
                assert(!used_index);
                used_index = true;
                tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
              } else {
                tokens.push_back(to_token(tok));
              }
            }
            result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
            return;
          } else {
            throw std::runtime_error(
                fmt::format("Failed to match for stride (power 2 {}) with add: {}", input.stride,
                            args.at(0)->to_string(env)));
          }
        } else {
          auto arg0_matcher =
              Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                          {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                                       {Matcher::integer(input.stride), Matcher::any(0)}),
                           Matcher::integer(input.offset)});
          auto match_result = match(arg0_matcher, args.at(0));
          if (match_result.matched) {
            bool used_index = false;
            std::vector<DerefToken> tokens;
            for (auto& tok : out.tokens) {
              if (tok.kind == FieldReverseLookupOutput::Token::Kind::VAR_IDX) {
                assert(!used_index);
                used_index = true;
                tokens.push_back(DerefToken::make_int_expr(match_result.maps.forms.at(0)));
              } else {
                tokens.push_back(to_token(tok));
              }
            }
            result->push_back(pool.alloc_element<DerefElement>(args.at(1), out.addr_of, tokens));
            return;
          } else {
            throw std::runtime_error(
                fmt::format("Failed to match for stride (non power 2 {}) with add: {}",
                            input.stride, args.at(0)->to_string(env)));
          }
        }
      }
    }
  }

  if ((arg0_i && arg1_i) || (arg0_u && arg1_u)) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION), args.at(0), args.at(1));
    result->push_back(new_form);
  } else if (arg0_ptr) {
    auto new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::ADDITION_PTR), args.at(0), args.at(1));
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
                                                           bool allow_side_effects,
                                                           bool reverse) {
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
    if (reverse) {
      args = pop_to_forms({m_expr.get_arg(1).var(), m_expr.get_arg(0).var()}, env, pool, stack,
                          allow_side_effects);
      auto temp = args.at(1);
      args.at(1) = args.at(0);
      args.at(0) = temp;
    } else {
      args = pop_to_forms({m_expr.get_arg(0).var(), m_expr.get_arg(1).var()}, env, pool, stack,
                          allow_side_effects);
    }

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
    throw std::runtime_error(fmt::format("Used int to float on a {} from {}: {}", type.print(),
                                         var.to_form(env).print(), arg->to_string(env)));
  }
}

void SimpleExpressionElement::update_from_stack_float_to_int(const Env& env,
                                                             FormPool& pool,
                                                             FormStack& stack,
                                                             std::vector<FormElement*>* result,
                                                             bool allow_side_effects) {
  auto var = m_expr.get_arg(0).var();
  auto arg = pop_to_forms({var}, env, pool, stack, allow_side_effects).at(0);
  auto type = env.get_types_before_op(var.idx()).get(var.reg()).typespec();
  if (type == TypeSpec("float")) {
    result->push_back(pool.alloc_element<CastElement>(TypeSpec("int"), arg, true));
  } else {
    throw std::runtime_error("Used float to int on a " + type.print());
  }
}

void SimpleExpressionElement::update_from_stack(const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                std::vector<FormElement*>* result,
                                                bool allow_side_effects) {
  mark_popped();
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
    case SimpleExpression::Kind::MAX_S:
      update_from_stack_float_2(env, FixedOperatorKind::FMAX, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::MIN_S:
      update_from_stack_float_2(env, FixedOperatorKind::FMIN, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::SQRT_S:
      update_from_stack_float_1(env, FixedOperatorKind::SQRT, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::ABS_S:
      update_from_stack_float_1(env, FixedOperatorKind::FABS, pool, stack, result,
                                allow_side_effects);
      break;
    case SimpleExpression::Kind::NEG:
      update_from_stack_si_1(env, FixedOperatorKind::SUBTRACTION, pool, stack, result,
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
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::MOD_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MOD, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::MIN_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MIN, pool, stack, result,
                                   allow_side_effects, false);
      break;
    case SimpleExpression::Kind::MAX_SIGNED:
      update_from_stack_force_si_2(env, FixedOperatorKind::MAX, pool, stack, result,
                                   allow_side_effects, false);
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
    case SimpleExpression::Kind::FLOAT_TO_INT:
      update_from_stack_float_to_int(env, pool, stack, result, allow_side_effects);
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
  mark_popped();
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
  assert(m_src->parent_element == this);

  // hack for method stuff
  if (is_dead_set()) {
    stack.push_value_to_reg_dead(m_dst, m_src, true, m_var_info);
    return;
  }

  // if we are a reg-reg move that consumes the original, push it without popping from stack.
  // it is the Stack's responsibility to untangle these later on.
  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        // this can happen late in the case of coloring moves which are also gpr -> fpr's
        // so they don't get caught by SetVarOp::get_as_form's check.
        if (env.op_id_is_eliminated_coloring_move(src_as_se->expr().get_arg(0).var().idx())) {
          m_var_info.is_eliminated_coloring_move = true;
        }

        auto var = src_as_se->expr().get_arg(0).var();
        auto& info = env.reg_use().op.at(var.idx());
        if (info.consumes.find(var.reg()) != info.consumes.end()) {
          stack.push_non_seq_reg_to_reg(m_dst, src_as_se->expr().get_arg(0).var(), m_src,
                                        m_var_info);
          return;
        }
      }
    }
  }

  // we aren't a reg-reg move, so update our source
  m_src->update_children_from_stack(env, pool, stack, true);

  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }

  if (m_src->is_single_element()) {
    auto src_as_se = dynamic_cast<SimpleExpressionElement*>(m_src->back());
    if (src_as_se) {
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          m_dst.reg().get_kind() == Reg::FPR && src_as_se->expr().get_arg(0).is_int() &&
          src_as_se->expr().get_arg(0).get_int() == 0) {
        // not sure this is the best place for this.
        stack.push_value_to_reg(m_dst,
                                pool.alloc_single_element_form<ConstantFloatElement>(nullptr, 0.0),
                                true, m_var_info);
        return;
      }

      // this might get skipped earlier because gpr->fpr gets wrapped in an operation that's
      // stripped off by update_children_from_stack.
      if (src_as_se->expr().kind() == SimpleExpression::Kind::IDENTITY &&
          src_as_se->expr().get_arg(0).is_var()) {
        if (env.op_id_is_eliminated_coloring_move(src_as_se->expr().get_arg(0).var().idx())) {
          m_var_info.is_eliminated_coloring_move = true;
        }
      }
    }
  }

  stack.push_value_to_reg(m_dst, m_src, true, m_var_info);
  for (auto x : m_src->elts()) {
    assert(x->parent_form == m_src);
  }
}

void SetFormFormElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  // todo - is the order here right?
  assert(m_popped);
  assert(m_real_push_count == 0);
  m_real_push_count++;
  stack.push_form_element(this, true);
}

void StoreInSymbolElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  auto sym = pool.alloc_single_element_form<SimpleExpressionElement>(
      nullptr, SimpleAtom::make_sym_val(m_sym_name).as_expr(), m_my_idx);
  auto val = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_value, m_my_idx);
  val->update_children_from_stack(env, pool, stack, true);

  auto elt = pool.alloc_element<SetFormFormElement>(sym, val);
  elt->mark_popped();
  stack.push_form_element(elt, true);
}

void StoreInPairElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  auto op = m_is_car ? FixedOperatorKind::CAR : FixedOperatorKind::CDR;
  if (m_value.is_var()) {
    auto vars = std::vector<Variable>({m_value.var(), m_pair});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    auto addr = pool.alloc_single_element_form<GenericElement>(
        nullptr, GenericOperator::make_fixed(op), popped.at(1));
    addr->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(addr, popped.at(0));
    fr->mark_popped();
    stack.push_form_element(fr, true);
  } else {
    auto val = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_value, m_my_idx);
    val->mark_popped();
    auto addr = pool.alloc_single_element_form<GenericElement>(
        nullptr, GenericOperator::make_fixed(op),
        pop_to_forms({m_pair}, env, pool, stack, true).at(0));
    addr->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(addr, val);
    fr->mark_popped();
    stack.push_form_element(fr, true);
  }
}

void StorePlainDeref::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  if (m_expr.is_var()) {
    auto vars = std::vector<Variable>({m_expr.var(), m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    if (m_cast_type.has_value()) {
      m_dst->set_base(
          pool.alloc_single_element_form<CastElement>(nullptr, *m_cast_type, popped.at(1)));
    } else {
      m_dst->set_base(popped.at(1));
    }

    m_dst->mark_popped();
    m_dst->inline_nested();
    auto fr = pool.alloc_element<SetFormFormElement>(pool.alloc_single_form(nullptr, m_dst),
                                                     popped.at(0));
    fr->mark_popped();
    stack.push_form_element(fr, true);
  } else {
    auto vars = std::vector<Variable>({m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    if (m_cast_type.has_value()) {
      m_dst->set_base(
          pool.alloc_single_element_form<CastElement>(nullptr, *m_cast_type, popped.at(1)));
    } else {
      m_dst->set_base(popped.at(0));
    }
    m_dst->mark_popped();
    m_dst->inline_nested();
    auto val = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_expr, m_my_idx);
    val->mark_popped();
    auto fr = pool.alloc_element<SetFormFormElement>(pool.alloc_single_form(nullptr, m_dst), val);
    fr->mark_popped();
    stack.push_form_element(fr, true);
  }
}

void StoreArrayAccess::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  Form* expr_form = nullptr;
  Form* array_form = nullptr;
  if (m_expr.is_var()) {
    auto vars = std::vector<Variable>({m_expr.var(), m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    m_dst->mark_popped();
    expr_form = popped.at(0);
    array_form = popped.at(1);
  } else {
    auto vars = std::vector<Variable>({m_base_var});
    auto popped = pop_to_forms(vars, env, pool, stack, true);
    m_dst->mark_popped();
    expr_form = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_expr, m_my_idx);
    array_form = popped.at(0);
  }

  std::vector<FormElement*> forms_out;
  m_dst->update_with_val(array_form, env, pool, &forms_out, true);
  auto form_out = pool.alloc_sequence_form(nullptr, forms_out);

  auto fr = pool.alloc_element<SetFormFormElement>(form_out, expr_form);
  fr->mark_popped();
  stack.push_form_element(fr, true);
}

///////////////////
// AshElement
///////////////////

void AshElement::update_from_stack(const Env& env,
                                   FormPool& pool,
                                   FormStack& stack,
                                   std::vector<FormElement*>* result,
                                   bool allow_side_effects) {
  mark_popped();
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
  mark_popped();
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
  mark_popped();
  std::vector<Form*> args;
  auto nargs = m_op->arg_vars().size();
  args.resize(nargs, nullptr);

  std::vector<Variable> all_pop_vars = {m_op->function_var()};
  for (size_t i = 0; i < nargs; i++) {
    all_pop_vars.push_back(m_op->arg_vars().at(i));
  }

  TypeSpec function_type;
  bool is_method = false;
  auto& tp_type = env.get_types_before_op(all_pop_vars.at(0).idx()).get(all_pop_vars.at(0).reg());
  if (env.has_type_analysis()) {
    if (tp_type.kind == TP_Type::Kind::VIRTUAL_METHOD && all_pop_vars.size() >= 1) {
      is_method = true;
    }
    function_type = tp_type.typespec();
  }

  assert(is_method == m_op->is_method());

  // if method, don't pop the obj arg.
  //  Variable method_obj_var;
  //  if (is_method) {
  //    method_obj_var = all_pop_vars.at(1);
  //    all_pop_vars.erase(all_pop_vars.begin() + 1);
  //  }

  if (tp_type.kind == TP_Type::Kind::NON_VIRTUAL_METHOD) {
    std::swap(all_pop_vars.at(0), all_pop_vars.at(1));
  }
  auto unstacked = pop_to_forms(all_pop_vars, env, pool, stack, allow_side_effects);
  if (tp_type.kind == TP_Type::Kind::NON_VIRTUAL_METHOD) {
    std::swap(unstacked.at(0), unstacked.at(1));
    std::swap(all_pop_vars.at(0), all_pop_vars.at(1));
  }

  std::vector<Form*> arg_forms;

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

  FormElement* new_form = nullptr;
  if (is_method) {
    //    fmt::print("STACK:\n{}\n\n", stack.print(env));
    auto matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::METHOD_OF_OBJECT),
                               {Matcher::any(0), Matcher::any(1)});
    auto mr = match(matcher, unstacked.at(0));
    if (!mr.matched) {
      throw std::runtime_error("Failed to match method call. Got " +
                               unstacked.at(0)->to_string(env));
    }

    auto unsafe = stack.unsafe_peek(Register(Reg::GPR, Reg::A0), env);
    if (!unsafe) {
      if (!stack.is_root()) {
        fmt::print("STACK:\n{}\n\n", stack.print(env));
        throw std::runtime_error("Peek got to back and not root stack");
      }
      // failed to peek by reaching the end AND root stack, means we just take the function
      // argument.
      unsafe = mr.maps.forms.at(0);
    }

    bool resolved = false;
    if (unsafe) {
      if (!unsafe->try_as_single_element()) {
        throw std::runtime_error(
            fmt::format("Peek got something weird: {}\n", unsafe->to_string(env)));
      }

      if (unsafe->try_as_single_element() == mr.maps.forms.at(0)->try_as_single_element()) {
        resolved = true;
      }

      if (!resolved) {
        if (unsafe->try_as_single_element()->to_form(env) ==
            mr.maps.forms.at(0)->try_as_single_element()->to_form(env)) {
          resolved = true;
          lg::warn(
              fmt::format("Rare method call (type 1). {} vs {}. Not an error, but check carefully",
                          unsafe->to_string(env), mr.maps.forms.at(0)->to_string(env)));
        }
      }

      if (!resolved) {
        lg::warn(
            fmt::format("Rare method call (type 2). {} vs {}. Not an error, but check carefully",
                        unsafe->to_string(env), mr.maps.forms.at(0)->to_string(env)));

        auto unsafe_as_se = dynamic_cast<SimpleExpressionElement*>(unsafe->try_as_single_element());
        if (unsafe_as_se && unsafe_as_se->expr().is_identity() &&
            unsafe_as_se->expr().get_arg(0).is_var()) {
          auto var = unsafe_as_se->expr().get_arg(0).var();
          auto unsafe_2 = stack.unsafe_peek(var.reg(), env);
          if (unsafe_2) {
            if (unsafe_2->try_as_single_element() == mr.maps.forms.at(0)->try_as_single_element()) {
              resolved = true;
              unsafe = unsafe_2;
            } else {
              if (unsafe_2->try_as_single_element()->to_form(env) ==
                  mr.maps.forms.at(0)->try_as_single_element()->to_form(env)) {
                lg::warn("Check even more carefully");
                resolved = true;
                unsafe = unsafe_2;
              }
            }
          }
        }
      }
    }

    if (!resolved) {
      throw std::runtime_error("Failed to resolve.");
    }

    arg_forms.insert(arg_forms.begin(), unsafe);
    new_form = pool.alloc_element<GenericElement>(
        GenericOperator::make_function(mr.maps.forms.at(1)), arg_forms);

  } else {
    new_form = pool.alloc_element<GenericElement>(GenericOperator::make_function(unstacked.at(0)),
                                                  arg_forms);
  }

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
          if (alloc != "global" && alloc != "debug" && alloc != "process") {
            throw std::runtime_error("Unrecognized heap symbol for new: " + alloc);
          }
          auto type_2 = match_result.maps.strings.at(type_for_arg);
          if (type_1 != type_2) {
            throw std::runtime_error(
                fmt::format("Inconsistent types in method call: {} and {}", type_1, type_2));
          }

          auto quoted_type = pool.alloc_single_element_form<SimpleAtomElement>(
              nullptr, SimpleAtom::make_sym_ptr(type_2));

          if (alloc == "global" && type_1 == "pair") {
            // cons!
            // (new 'global 'pair a b) -> (cons a b)
            std::vector<Form*> cons_args = {dynamic_cast<GenericElement*>(new_form)->elts().at(2),
                                            dynamic_cast<GenericElement*>(new_form)->elts().at(3)};
            auto cons_op = pool.alloc_element<GenericElement>(
                GenericOperator::make_fixed(FixedOperatorKind::CONS), cons_args);
            result->push_back(cons_op);
            return;
          } else {
            // just normal construction on the heap
            std::vector<Form*> new_args = dynamic_cast<GenericElement*>(new_form)->elts();
            new_args.at(1) = quoted_type;

            auto new_op = pool.alloc_element<GenericElement>(
                GenericOperator::make_fixed(FixedOperatorKind::NEW), new_args);
            result->push_back(new_op);
            return;
          }
        } else {
          lg::warn("Got a suspicious new method. This may be fine, but should be uncommon: {}",
                   temp_form->to_string(env));
          //          throw std::runtime_error("Failed to match new method: " +
          //          temp_form->to_string(env));
        }
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
  mark_popped();
  // todo - update var tokens from stack?
  m_base->update_children_from_stack(env, pool, stack, allow_side_effects);

  // merge nested ->'s
  inline_nested();

  if (m_tokens.size() >= 3) {
    auto& method_name = m_tokens.at(m_tokens.size() - 1);
    auto& mbn = m_tokens.at(m_tokens.size() - 2);
    auto& type = m_tokens.at(m_tokens.size() - 3);
    if (method_name.kind() == DerefToken::Kind::FIELD_NAME &&
        mbn.kind() == DerefToken::Kind::FIELD_NAME && mbn.field_name() == "methods-by-name" &&
        type.kind() == DerefToken::Kind::FIELD_NAME && type.field_name() == "type") {
      std::string name = method_name.field_name();
      m_tokens.pop_back();
      m_tokens.pop_back();
      m_tokens.pop_back();

      if (m_tokens.empty()) {
        auto method_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_OBJECT), m_base,
            pool.alloc_single_element_form<ConstantTokenElement>(nullptr, name));
        result->push_back(method_op);
      } else {
        auto method_op = pool.alloc_element<GenericElement>(
            GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_OBJECT),
            pool.alloc_single_form(nullptr, this),
            pool.alloc_single_element_form<ConstantTokenElement>(nullptr, name));
        result->push_back(method_op);
      }
      return;
    }
  }

  // rewrite access to the method table to use method-of-object
  // (-> <some-object> type methods-by-name <method-name>)
  // (method-of-object <some-object> <method-name>)
  auto get_method_matcher = Matcher::deref(
      Matcher::any(0), false,
      {DerefTokenMatcher::string("type"), DerefTokenMatcher::string("methods-by-name"),
       DerefTokenMatcher::any_string(1)});
  Form hack_form;
  hack_form.elts() = {this};
  auto mr = match(get_method_matcher, &hack_form);
  if (mr.matched) {
    auto method_op = pool.alloc_element<GenericElement>(
        GenericOperator::make_fixed(FixedOperatorKind::METHOD_OF_OBJECT), mr.maps.forms.at(0),
        pool.alloc_single_element_form<ConstantTokenElement>(nullptr, mr.maps.strings.at(1)));
    result->push_back(method_op);
  } else {
    result->push_back(this);
  }
}

void DerefElement::inline_nested() {
  auto as_deref = dynamic_cast<DerefElement*>(m_base->try_as_single_element());
  if (as_deref) {
    if (!m_is_addr_of && !as_deref->is_addr_of()) {
      m_tokens.insert(m_tokens.begin(), as_deref->tokens().begin(), as_deref->tokens().end());
      m_base = as_deref->m_base;
    }
  }
}

///////////////////
// UntilElement
///////////////////

void UntilElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  for (auto form : {condition, body}) {
    FormStack temp_stack(false);
    for (auto& entry : form->elts()) {
      entry->push_to_stack(env, pool, temp_stack);
    }
    auto new_entries = temp_stack.rewrite(pool, env);
    form->clear();
    for (auto e : new_entries) {
      form->push_back(e);
    }
  }

  stack.push_form_element(this, true);
}

void WhileElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  bool first = true;
  for (auto form : {body, condition}) {
    FormStack temp_stack(first && stack.is_root());
    first = false;
    for (auto& entry : form->elts()) {
      entry->push_to_stack(env, pool, temp_stack);
    }
    auto new_entries = temp_stack.rewrite(pool, env);
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
  mark_popped();
  if (already_rewritten) {
    stack.push_form_element(this, true);
    return;
  }

  // the first condition is special
  auto first_condition = entries.front().condition;
  // lets evaluate in on the parent stack...
  for (auto x : first_condition->elts()) {
    x->push_to_stack(env, pool, stack);
  }

  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      if (form == first_condition) {
        form->clear();
        form->push_back(stack.pop_back(pool));
      } else {
        FormStack temp_stack(false);
        for (auto& elt : form->elts()) {
          elt->push_to_stack(env, pool, temp_stack);
        }

        std::vector<FormElement*> new_entries;
        if (form == entry.body && used_as_value) {
          new_entries = rewrite_to_get_var(temp_stack, pool, final_destination, env);
        } else {
          new_entries = temp_stack.rewrite(pool, env);
        }

        form->clear();
        for (auto e : new_entries) {
          form->push_back(e);
        }
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
  mark_popped();
  if (already_rewritten) {
    stack.push_form_element(this, true);
    return;
  }
  // first, let's try to detect if all bodies write the same value
  std::optional<Variable> last_var;
  bool rewrite_as_set = true;

  // the first condition is special
  auto first_condition = entries.front().condition;
  // lets evaluate in on the parent stack...
  for (auto x : first_condition->elts()) {
    x->push_to_stack(env, pool, stack);
  }

  // process conditions and bodies
  for (auto& entry : entries) {
    for (auto form : {entry.condition, entry.body}) {
      if (form == first_condition) {
        form->clear();
        form->push_back(stack.pop_back(pool));
      } else {
        FormStack temp_stack(false);
        for (auto& elt : form->elts()) {
          elt->push_to_stack(env, pool, temp_stack);
        }

        std::vector<FormElement*> new_entries;
        new_entries = temp_stack.rewrite(pool, env);

        form->clear();
        for (auto e : new_entries) {
          form->push_back(e);
        }
      }
    }
  }

  // process else.
  FormStack temp_stack(false);
  for (auto& elt : else_ir->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  new_entries = temp_stack.rewrite(pool, env);

  else_ir->clear();
  for (auto e : new_entries) {
    else_ir->push_back(e);
  }

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

  // rewrite extra sets as needed.
  if (rewrite_as_set && !set_unused) {
    for (auto& entry : entries) {
      rewrite_to_get_var(entry.body->elts(), pool, *last_var, env);
    }
    rewrite_to_get_var(else_ir->elts(), pool, *last_var, env);
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
  mark_popped();
  if (!used_as_value.value_or(false)) {
    throw std::runtime_error(
        "ShortCircuitElement::push_to_stack not implemented for result not used case.");

    stack.push_form_element(this, true);
  } else {
    if (already_rewritten) {
      stack.push_form_element(this, true);
      return;
    }

    // the first condition is special
    auto first_condition = entries.front().condition;
    // lets evaluate in on the parent stack...
    for (auto x : first_condition->elts()) {
      x->push_to_stack(env, pool, stack);
    }

    for (int i = 0; i < int(entries.size()); i++) {
      auto& entry = entries.at(i);
      if (entry.condition == first_condition) {
        entry.condition->clear();
        entry.condition->push_back(stack.pop_back(pool));
      } else {
        FormStack temp_stack(false);
        for (auto& elt : entry.condition->elts()) {
          elt->push_to_stack(env, pool, temp_stack);
        }

        std::vector<FormElement*> new_entries;
        if (i == int(entries.size()) - 1) {
          new_entries = rewrite_to_get_var(temp_stack, pool, final_result, env);
        } else {
          new_entries = temp_stack.rewrite(pool, env);
        }

        entry.condition->clear();
        for (auto e : new_entries) {
          entry.condition->push_back(e);
        }
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
  mark_popped();
  (void)stack;
  if (already_rewritten) {
    result->push_back(this);
    return;
  }
  for (int i = 0; i < int(entries.size()); i++) {
    auto& entry = entries.at(i);
    FormStack temp_stack(false);
    for (auto& elt : entry.condition->elts()) {
      elt->push_to_stack(env, pool, temp_stack);
    }

    std::vector<FormElement*> new_entries;
    if (i == int(entries.size()) - 1) {
      new_entries = rewrite_to_get_var(temp_stack, pool, final_result, env);
    } else {
      new_entries = temp_stack.rewrite(pool, env);
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

FormElement* ConditionElement::make_zero_check_generic(const Env&,
                                                       FormPool& pool,
                                                       const std::vector<Form*>& source_forms,
                                                       const std::vector<TypeSpec>&) {
  // (zero? (+ thing small-integer)) -> (= thing (- small-integer))

  auto mr = match(Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                              {Matcher::any(0), Matcher::any_integer(1)}),
                  source_forms.at(0));
  if (mr.matched) {
    s64 value = -mr.maps.ints.at(1);
    auto value_form = pool.alloc_single_element_form<SimpleAtomElement>(
        nullptr, SimpleAtom::make_int_constant(value));
    return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::EQ),
                                              std::vector<Form*>{mr.maps.forms.at(0), value_form});
  } else {
    return pool.alloc_element<GenericElement>(GenericOperator::make_compare(m_kind), source_forms);
  }
}

FormElement* ConditionElement::make_generic(const Env& env,
                                            FormPool& pool,
                                            const std::vector<Form*>& source_forms,
                                            const std::vector<TypeSpec>& types) {
  switch (m_kind) {
    case IR2_Condition::Kind::ZERO:
      return make_zero_check_generic(env, pool, source_forms, types);
    case IR2_Condition::Kind::TRUTHY:
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

    case IR2_Condition::Kind::GEQ_SIGNED:
      return pool.alloc_element<GenericElement>(
          GenericOperator::make_fixed(FixedOperatorKind::GEQ),
          make_cast(source_forms, types, TypeSpec("int"), pool));

    case IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("int"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LT),
                                                casted);
    }

    case IR2_Condition::Kind::LEQ_ZERO_SIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("int"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::LEQ),
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

    case IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED: {
      auto casted = make_cast(source_forms, types, TypeSpec("uint"), pool);
      auto zero = pool.alloc_single_element_form<SimpleAtomElement>(
          nullptr, SimpleAtom::make_int_constant(0));
      casted.push_back(zero);
      return pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::GT),
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
  mark_popped();
  std::vector<Form*> source_forms, popped_forms;
  std::vector<TypeSpec> source_types;
  std::vector<Variable> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      auto& var = m_src[i]->var();
      vars.push_back(var);
      source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
    } else if (m_src[i]->is_int()) {
      if (m_src[i]->get_int() == 0 && condition_uses_float(m_kind)) {
        // if we're doing a floating point comparison, and one of our arguments is a constant
        // which is an "integer zero", treat it as a floating point zero.
        source_types.push_back(TypeSpec("float"));
      } else {
        source_types.push_back(TypeSpec("int"));
      }
    } else {
      throw std::runtime_error("Unsupported atom in ConditionElement::push_to_stack");
    }
  }
  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }

  popped_forms = pop_to_forms(vars, env, pool, stack, true, m_consumed);
  if (m_flipped) {
    std::reverse(popped_forms.begin(), popped_forms.end());
  }

  int popped_counter = 0;
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      source_forms.push_back(popped_forms.at(popped_counter++));
    } else {
      source_forms.push_back(pool.alloc_single_element_form<SimpleAtomElement>(nullptr, *m_src[i]));
    }
  }
  assert(popped_counter == int(popped_forms.size()));
  assert(source_forms.size() == source_types.size());

  stack.push_form_element(make_generic(env, pool, source_forms, source_types), true);
}

void ConditionElement::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  mark_popped();
  std::vector<Form*> source_forms, popped_forms;
  std::vector<TypeSpec> source_types;
  std::vector<Variable> vars;

  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      auto& var = m_src[i]->var();
      vars.push_back(var);
      source_types.push_back(env.get_types_before_op(var.idx()).get(var.reg()).typespec());
    } else if (m_src[i]->is_int()) {
      if (m_src[i]->get_int() == 0 && condition_uses_float(m_kind)) {
        // if we're doing a floating point comparison, and one of our arguments is a constant
        // which is an "integer zero", treat it as a floating point zero.
        source_types.push_back(TypeSpec("float"));
      } else {
        source_types.push_back(TypeSpec("int"));
      }
    } else {
      throw std::runtime_error("Unsupported atom in ConditionElement::push_to_stack");
    }
  }
  if (m_flipped) {
    std::reverse(vars.begin(), vars.end());
  }

  popped_forms = pop_to_forms(vars, env, pool, stack, allow_side_effects, m_consumed);
  if (m_flipped) {
    std::reverse(popped_forms.begin(), popped_forms.end());
  }

  int popped_counter = 0;
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    if (m_src[i]->is_var()) {
      source_forms.push_back(popped_forms.at(popped_counter++));
    } else {
      source_forms.push_back(pool.alloc_single_element_form<SimpleAtomElement>(nullptr, *m_src[i]));
    }
  }
  assert(popped_counter == int(popped_forms.size()));
  assert(source_forms.size() == source_types.size());

  result->push_back(make_generic(env, pool, source_forms, source_types));
}

void ReturnElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  FormStack temp_stack(false);
  for (auto& elt : return_code->elts()) {
    elt->push_to_stack(env, pool, temp_stack);
  }

  std::vector<FormElement*> new_entries;
  new_entries = rewrite_to_get_var(temp_stack, pool, env.end_var(), env);

  return_code->clear();
  for (auto e : new_entries) {
    return_code->push_back(e);
  }
  stack.push_form_element(this, true);
}

void AtomicOpElement::push_to_stack(const Env& env, FormPool&, FormStack& stack) {
  mark_popped();
  auto as_end = dynamic_cast<const FunctionEndOp*>(m_op);
  if (as_end) {
    // we don't want to push this to the stack (for now at least)
    return;
  }

  auto as_special = dynamic_cast<const SpecialOp*>(m_op);
  if (as_special) {
    if (as_special->kind() == SpecialOp::Kind::NOP ||
        as_special->kind() == SpecialOp::Kind::BREAK ||
        as_special->kind() == SpecialOp::Kind::CRASH) {
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
  mark_popped();
  stack.push_form_element(this, true);
}

void GenericElement::update_from_stack(const Env& env,
                                       FormPool& pool,
                                       FormStack& stack,
                                       std::vector<FormElement*>* result,
                                       bool) {
  mark_popped();
  if (m_elts.size() == 1) {
    // a bit of a hack, but AtomicOpForm uses this for loading car/cdr
    // this is safe to do.
    m_elts.front()->update_children_from_stack(env, pool, stack, true);
  }
  result->push_back(this);
}

void GenericElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  (void)env;
  (void)pool;
  mark_popped();
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
  mark_popped();
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

void ArrayFieldAccess::update_with_val(Form* new_val,
                                       const Env& env,
                                       FormPool& pool,
                                       std::vector<FormElement*>* result,
                                       bool) {
  int power_of_two = 0;

  if (m_constant_offset == 0) {
    if (m_expected_stride == 1) {
      throw std::runtime_error("One case, not yet implemented (no offset)");
    } else if (is_power_of_two(m_expected_stride, &power_of_two)) {
      // reg0 is base
      // reg1 is idx

      auto reg0_matcher =
          Matcher::match_or({Matcher::cast("int", Matcher::any(0)),
                             Matcher::cast("uint", Matcher::any(0)), Matcher::any(0)});
      auto reg1_matcher =
          Matcher::match_or({Matcher::cast("uint", Matcher::any(1)), Matcher::any(1)});
      auto sll_matcher =
          Matcher::fixed_op(FixedOperatorKind::SHL, {reg1_matcher, Matcher::integer(power_of_two)});
      sll_matcher = Matcher::match_or({Matcher::cast("uint", sll_matcher), sll_matcher});
      auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {reg0_matcher, sll_matcher});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {sll_matcher, reg0_matcher});
        match_result = match(matcher, new_val);
        if (!match_result.matched) {
          fmt::print("power {}\n", power_of_two);
          throw std::runtime_error(
              "Couldn't match ArrayFieldAccess (stride power of 2, 0 offset) values: " +
              new_val->to_string(env));
        }
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
          Matcher::match_or({Matcher::any(0), Matcher::cast("int", Matcher::any_reg(0))});
      // reg1 is base
      auto reg1_matcher =
          Matcher::match_or({Matcher::any_reg(1), Matcher::cast("int", Matcher::any_reg(1))});
      auto matcher = Matcher::fixed_op(FixedOperatorKind::ADDITION, {reg0_matcher, reg1_matcher});
      auto match_result = match(matcher, new_val);
      if (!match_result.matched) {
        throw std::runtime_error("Couldn't match ArrayFieldAccess (stride 1) values: " +
                                 new_val->to_string(env));
      }
      auto idx = match_result.maps.forms.at(0);
      auto base = match_result.maps.regs.at(1);
      assert(idx && base.has_value());

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
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
      // (+ v0-0 (the-as uint (* 12 (+ a3-0 -1))))
      auto mult_matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::MULTIPLICATION),
                                      {Matcher::integer(m_expected_stride), Matcher::any(0)});
      mult_matcher = Matcher::match_or({Matcher::cast("uint", mult_matcher), mult_matcher});
      auto add_matcher = Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::ADDITION),
                                     {Matcher::any(1), mult_matcher});

      auto mr = match(add_matcher, new_val);
      if (!mr.matched) {
        throw std::runtime_error("Failed to match non-power of two case: " +
                                 new_val->to_string(env));
      }

      auto base = mr.maps.forms.at(1);
      auto idx = mr.maps.forms.at(0);

      assert(idx && base);

      std::vector<DerefToken> tokens = m_deref_tokens;
      for (auto& x : tokens) {
        if (x.kind() == DerefToken::Kind::EXPRESSION_PLACEHOLDER) {
          x = DerefToken::make_int_expr(idx);
        }
      }

      auto deref = pool.alloc_element<DerefElement>(base, false, tokens);
      result->push_back(deref);
    }
  }
}

void ArrayFieldAccess::update_from_stack(const Env& env,
                                         FormPool& pool,
                                         FormStack& stack,
                                         std::vector<FormElement*>* result,
                                         bool allow_side_effects) {
  mark_popped();
  auto new_val = stack.pop_reg(m_source, {}, env, allow_side_effects);
  update_with_val(new_val, env, pool, result, allow_side_effects);
}

////////////////////////
// CastElement
////////////////////////

void CastElement::update_from_stack(const Env& env,
                                    FormPool& pool,
                                    FormStack& stack,
                                    std::vector<FormElement*>* result,
                                    bool allow_side_effects) {
  mark_popped();
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
  mark_popped();
  value->update_children_from_stack(env, pool, stack, allow_side_effects);
  result->push_back(this);
}

////////////////////////
// EmptyElement
////////////////////////

void EmptyElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  mark_popped();
  stack.push_form_element(this, true);
}

void StoreElement::push_to_stack(const Env&, FormPool&, FormStack& stack) {
  mark_popped();
  stack.push_form_element(this, true);
}

bool is_symbol_true(const Form* form) {
  auto as_simple = dynamic_cast<SimpleExpressionElement*>(form->try_as_single_element());
  if (as_simple && as_simple->expr().is_identity() && as_simple->expr().get_arg(0).is_sym_ptr() &&
      as_simple->expr().get_arg(0).get_str() == "#t") {
    return true;
  }
  return false;
}

void ConditionalMoveFalseElement::push_to_stack(const Env& env, FormPool& pool, FormStack& stack) {
  mark_popped();
  // pop the value and the original
  auto popped = pop_to_forms({old_value, source}, env, pool, stack, true);
  if (!is_symbol_true(popped.at(0))) {
    throw std::runtime_error("Got unrecognized ConditionalMoveFalseElement original: " +
                             popped.at(0)->to_string(env));
  }
  stack.push_value_to_reg(dest,
                          pool.alloc_single_element_form<GenericElement>(
                              nullptr,
                              GenericOperator::make_compare(on_zero ? IR2_Condition::Kind::NONZERO
                                                                    : IR2_Condition::Kind::ZERO),
                              std::vector<Form*>{popped.at(1)}),
                          true);
}

void SimpleAtomElement::update_from_stack(const Env&,
                                          FormPool&,
                                          FormStack&,
                                          std::vector<FormElement*>* result,
                                          bool) {
  mark_popped();
  result->push_back(this);
}

void StringConstantElement::update_from_stack(const Env&,
                                              FormPool&,
                                              FormStack&,
                                              std::vector<FormElement*>* result,
                                              bool) {
  mark_popped();
  result->push_back(this);
}

void GetMethodElement::update_from_stack(const Env&,
                                         FormPool&,
                                         FormStack&,
                                         std::vector<FormElement*>* result,
                                         bool) {
  mark_popped();
  result->push_back(this);
}

void CondNoElseElement::update_from_stack(const Env&,
                                          FormPool&,
                                          FormStack&,
                                          std::vector<FormElement*>* result,
                                          bool) {
  mark_popped();
  result->push_back(this);
}

void ConstantTokenElement::update_from_stack(const Env&,
                                             FormPool&,
                                             FormStack&,
                                             std::vector<FormElement*>* result,
                                             bool) {
  mark_popped();
  result->push_back(this);
}

void ConstantFloatElement::update_from_stack(const Env&,
                                             FormPool&,
                                             FormStack&,
                                             std::vector<FormElement*>* result,
                                             bool) {
  mark_popped();
  result->push_back(this);
}

}  // namespace decompiler
