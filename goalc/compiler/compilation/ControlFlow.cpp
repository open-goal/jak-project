/*!
 * @file ControlFlow.cpp
 * Compiler forms related to conditional branching and control flow.
 */

#include "common/goos/ParseHelpers.h"

#include "goalc/compiler/Compiler.h"

/*!
 * Convert an expression into a GoalCondition for use in a conditional branch.
 *
 * The reason for this design is to allow an optimization for
 * (if (< a b) ...) to be compiled without actually computing a true/false value for the (< a b)
 * expression. Instead, it will generate a cmp + jle sequence of instructions, which is much faster.
 * In particular, getting GOAL "true" requires a few instructions, so it's to avoid this when
 * possible.
 *
 * This can be applied to _any_ GOAL form, and will return a GoalCondition which can be used with a
 * Branch IR to branch if the condition is true/false.  When possible it applies the optimization
 * mentioned above, but will be fine in other cases too.  I believe the original GOAL compiler had a
 * similar system.
 *
 * Will branch if the condition is true and the invert flag is false.
 * Will branch if the condition is false and the invert flag is true.
 */
Condition Compiler::compile_condition(const goos::Object& condition, Env* env, bool invert) {
  Condition gc;

  // These are special conditions that can be optimized into a cmp + jxx instruction.
  const std::unordered_map<std::string, ConditionKind> conditions_inverted = {
      {"!=", ConditionKind::EQUAL},   {"eq?", ConditionKind::NOT_EQUAL},
      {"neq?", ConditionKind::EQUAL}, {"=", ConditionKind::NOT_EQUAL},
      {">", ConditionKind::LEQ},      {"<", ConditionKind::GEQ},
      {">=", ConditionKind::LT},      {"<=", ConditionKind::GT}};

  const std::unordered_map<std::string, ConditionKind> conditions_normal = {
      {"!=", ConditionKind::NOT_EQUAL},   {"eq?", ConditionKind::EQUAL},
      {"neq?", ConditionKind::NOT_EQUAL}, {"=", ConditionKind::EQUAL},
      {">", ConditionKind::GT},           {"<", ConditionKind::LT},
      {">=", ConditionKind::GEQ},         {"<=", ConditionKind::LEQ}};

  // possibly a form with an optimizable condition?
  if (condition.is_pair()) {
    auto first = pair_car(condition);
    auto rest = pair_cdr(condition);

    if (first.is_symbol()) {
      auto fas = first.as_symbol();

      // if there's a not, we can just try again to get an optimization with the invert flipped.
      if (fas->name == "not") {
        auto arg = pair_car(rest);
        if (!pair_cdr(rest).is_empty_list()) {
          throw_compiler_error(condition, "A condition with \"not\" can have only one argument");
        }
        return compile_condition(arg, env, !invert);
      }

      auto& conditions = invert ? conditions_inverted : conditions_normal;
      auto nc_kv = conditions.find(fas->name);

      if (nc_kv != conditions.end()) {
        // it is an optimizable condition!
        gc.kind = nc_kv->second;

        // get args...
        auto args = get_va(rest, rest);
        va_check(rest, args, {{}, {}}, {});
        auto first_arg = compile_error_guard(args.unnamed.at(0), env);
        auto second_arg = compile_error_guard(args.unnamed.at(1), env);

        if (is_number(first_arg->type())) {
          // it's a numeric comparison, so we may need to coerce.
          auto math_mode = get_math_mode(first_arg->type());

          // there is no support for comparing bintegers, so we turn the binteger comparison into an
          // integer.
          if (is_binteger(first_arg->type())) {
            first_arg = number_to_integer(condition, first_arg, env);
          }

          // convert second one to appropriate type as needed
          if (is_number(second_arg->type())) {
            second_arg = to_math_type(condition, second_arg, math_mode, env);
          }
        }

        // use signed comparison only if first argument is a signed integer (or coerced binteger)
        // (floating point ignores this)
        gc.is_signed = is_singed_integer_or_binteger(first_arg->type());

        // pick between a floating point and an integer comparison.
        if (is_float(first_arg->type())) {
          gc.a = first_arg->to_fpr(condition, env);
          gc.b = second_arg->to_fpr(condition, env);
          gc.is_float = true;
        } else {
          gc.a = first_arg->to_gpr(condition, env);
          gc.b = second_arg->to_gpr(condition, env);
        }

        if (gc.a->type() == TypeSpec("none") || gc.b->type() == TypeSpec("none")) {
          throw_compiler_error(condition, "Cannot use none-typed variable in a condition.");
        }

        return gc;
      }
    }
  }

  // not something we can process more.  Just evaluate as normal and check if we get false.
  // todo - it's possible to optimize a false comparison because the false offset is zero
  gc.kind = invert ? ConditionKind::EQUAL : ConditionKind::NOT_EQUAL;
  gc.a = compile_error_guard(condition, env)->to_gpr(condition, env);
  if (gc.a->type() == TypeSpec("none")) {
    throw_compiler_error(condition, "Cannot use none-typed variable in a condition.");
  }
  gc.b = compile_get_sym_obj("#f", env)->to_gpr(condition, env);

  return gc;
}

/*!
 * Compile a comparison when we explicitly want a boolean result. This is used whenever a condition
 * _isn't_ used as a branch condition. Like (set! x (< 1 2))
 *
 * TODO, this could be optimized quite a bit.
 */
Val* Compiler::compile_condition_as_bool(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env) {
  (void)rest;
  auto c = compile_condition(form, env, true);
  auto result = compile_get_sym_obj("#f", env)->to_gpr(form, env);  // todo - can be optimized.
  Label label(env->function_env(), -5);
  auto branch_ir = std::make_unique<IR_ConditionalBranch>(c, label);
  auto branch_ir_ref = branch_ir.get();
  env->emit(form, std::move(branch_ir));

  // move true
  env->emit(form, std::make_unique<IR_RegSet>(result, compile_get_sym_obj("#t", env)->to_gpr(
                                                          form, env)));  // todo, can be optimized
  branch_ir_ref->label.idx = branch_ir_ref->label.func->code().size();
  branch_ir_ref->mark_as_resolved();

  return result;
}

/*!
 * The when-goto form is a better version of (if condition (goto x))
 * It compiles into a single conditional branch.
 */
Val* Compiler::compile_when_goto(const goos::Object& form, const goos::Object& _rest, Env* env) {
  (void)form;
  auto* rest = &_rest;
  auto condition_code = pair_car(*rest);
  rest = &pair_cdr(*rest);

  auto label = symbol_string(pair_car(*rest));
  expect_empty_list(pair_cdr(*rest));

  // compile as condition (will set flags register with a cmp instruction)
  auto condition = compile_condition(condition_code, env, false);
  auto branch = std::make_unique<IR_ConditionalBranch>(condition, Label());
  env->function_env()->unresolved_cond_gotos.push_back({branch.get(), label});
  env->emit(form, std::move(branch));
  return get_none();
}

/*!
 * The Scheme/Lisp "cond" form.
 * Works like you expect. Return type is the lowest common ancestor of all possible return values.
 * If no cases match and there's no else, returns #f.
 * TODO - how should the return type work if #f can possibly be returned?
 */
Val* Compiler::compile_cond(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto result = env->make_gpr(m_ts.make_typespec("object"));

  auto fenv = env->function_env();
  auto end_label = fenv->alloc_unnamed_label();
  end_label->func = fenv;
  end_label->idx = -3;  // placeholder

  bool got_else = false;

  std::vector<TypeSpec> case_result_types;

  for_each_in_list(rest, [&](const goos::Object& o) {
    auto test = pair_car(o);
    auto clauses = pair_cdr(o);

    if (got_else) {
      throw_compiler_error(form, "Cond from cannot have any cases after else.");
    }

    if (test.is_symbol() && symbol_string(test) == "else") {
      got_else = true;
    }

    if (got_else) {
      // just set the output to this.
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
        if (!dynamic_cast<None*>(case_result)) {
          case_result = case_result->to_reg(clause, env);
        }
      });

      case_result_types.push_back(case_result->type());

      // optimization - if we get junk, don't bother moving it, just leave junk in return.
      if (!is_none(case_result)) {
        // todo, what does GOAL do here? does it matter?
        env->emit(o, std::make_unique<IR_RegSet>(result, case_result->to_reg(o, env)));
      }

    } else {
      // CONDITION CHECK
      auto condition = compile_condition(test, env, true);

      // BRANCH FWD
      auto branch_ir = std::make_unique<IR_ConditionalBranch>(condition, Label());
      auto branch_ir_ref = branch_ir.get();
      branch_ir->mark_as_resolved();
      env->emit(test, std::move(branch_ir));

      // CODE
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
        if (!dynamic_cast<None*>(case_result)) {
          case_result = case_result->to_reg(clause, env);
        }
      });

      case_result_types.push_back(case_result->type());
      if (!is_none(case_result)) {
        // todo, what does GOAL do here?
        env->emit(o, std::make_unique<IR_RegSet>(result, case_result->to_reg(o, env)));
      }

      // GO TO END
      auto ir_goto_end = std::make_unique<IR_GotoLabel>(end_label);
      env->emit(o, std::move(ir_goto_end));

      // PATCH BRANCH FWD
      branch_ir_ref->label.idx = fenv->code().size();
    }
  });

  if (!got_else) {
    // if no else, clause, return #f.  But don't retype. todo what does goal do here?
    auto get_false = std::make_unique<IR_LoadSymbolPointer>(result, "#f");
    env->emit(form, std::move(get_false));
  }

  if (case_result_types.empty()) {
    result->set_type(TypeSpec("none"));
  } else {
    result->set_type(coerce_to_reg_type(m_ts.lowest_common_ancestor(case_result_types)));
  }

  // maybe use 128-bit register
  if (result->type().base_type() != "none" &&
      m_ts.lookup_type_allow_partial_def(result->type())->get_load_size() == 16) {
    result->change_class(RegClass::INT_128);
  }

  // PATCH END
  end_label->idx = fenv->code().size();

  return result;
}

Val* Compiler::compile_and_or(const goos::Object& form, const goos::Object& rest, Env* env) {
  std::string op_name = form.as_pair()->car.as_symbol()->name;
  bool is_and = false;
  if (op_name == "and") {
    is_and = true;
  } else if (op_name == "or") {
    is_and = false;
  } else {
    throw_compiler_error(form, "compile_and_or got an invalid operation {}", op_name);
  }

  if (rest.is_empty_list()) {
    throw_compiler_error(form, "and/or form must have at least one element");
  }

  auto result = env->make_gpr(m_ts.make_typespec("object"));  // temp type for now.
  auto fenv = env->function_env();
  auto end_label = fenv->alloc_unnamed_label();
  end_label->func = fenv;
  end_label->idx = -4;  // placeholder

  std::vector<TypeSpec> case_result_types;
  case_result_types.push_back(TypeSpec("symbol"));  // can always return #f.

  std::vector<IR_ConditionalBranch*> branch_irs;
  auto n_elts = goos::list_length(rest);
  int i = 0;
  for_each_in_list(rest, [&](const goos::Object& o) {
    // get the result of this case, put it in the main result and remember the type
    auto temp = compile_error_guard(o, env)->to_gpr(o, env);
    case_result_types.push_back(temp->type());
    env->emit_ir<IR_RegSet>(o, result, temp);

    // no need check if we are the last element.
    if (i != n_elts - 1) {
      // now, check.
      Condition gc;
      gc.is_signed = false;
      gc.is_float = false;
      gc.a = result;
      gc.b = compile_get_sym_obj("#f", env)->to_gpr(o, env);  // todo, optimize
      if (is_and) {
        // for and we abort if we get a false:
        gc.kind = ConditionKind::EQUAL;
      } else {
        // for or, we abort when we get truthy
        gc.kind = ConditionKind::NOT_EQUAL;
      }
      // jump to end
      auto branch = std::make_unique<IR_ConditionalBranch>(gc, Label());
      branch_irs.push_back(branch.get());
      env->emit(o, std::move(branch));
    }
    i++;
  });

  // now patch branches
  end_label->idx = fenv->code().size();
  for (auto* br : branch_irs) {
    br->label = *end_label;
    br->mark_as_resolved();
  }

  // and set the result type
  result->set_type(m_ts.lowest_common_ancestor(case_result_types));

  return result;
}
