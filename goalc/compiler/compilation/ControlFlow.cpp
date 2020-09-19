#include "goalc/compiler/Compiler.h"

/*!
 * Convert a condition expression into a GoalCondition for use in a conditional branch.
 * The reason for this design is to allow an optimization for
 * (if (< a b) ...) to be compiled without actually computing a true/false value for the (< a b)
 * expression. Instead, it will generate a cmp + jle sequence of instructions, which is much faster.
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
          throw_compile_error(condition, "A condition with \"not\" can have only one argument");
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
            first_arg = number_to_integer(first_arg, env);
          }

          // convert second one to appropriate type as needed
          if (is_number(second_arg->type())) {
            second_arg = to_math_type(second_arg, math_mode, env);
          }
        }

        // use signed comparison only if first argument is a signed integer (or coerced binteger)
        // (floating point ignores this)
        gc.is_signed = is_singed_integer_or_binteger(first_arg->type());

        // pick between a floating point and an integer comparison.
        if (is_float(first_arg->type())) {
          gc.a = first_arg->to_xmm(env);
          gc.b = second_arg->to_xmm(env);
          gc.is_float = true;
        } else {
          gc.a = first_arg->to_gpr(env);
          gc.b = second_arg->to_gpr(env);
        }

        return gc;
      }
    }
  }

  // not something we can process more.  Just check if we get false.
  // todo - it's possible to optimize a false comparison because the false offset is zero
  gc.kind = invert ? ConditionKind::EQUAL : ConditionKind::NOT_EQUAL;
  gc.a = compile_error_guard(condition, env)->to_gpr(env);
  gc.b = compile_get_sym_obj("#f", env)->to_gpr(env);

  return gc;
}

Val* Compiler::compile_condition_as_bool(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env) {
  (void)rest;
  auto c = compile_condition(form, env, true);
  auto result = compile_get_sym_obj("#f", env)->to_gpr(env);  // todo - can be optimized.
  Label label(get_parent_env_of_type<FunctionEnv>(env), -5);
  auto branch_ir = std::make_unique<IR_ConditionalBranch>(c, label);
  auto branch_ir_ref = branch_ir.get();
  env->emit(std::move(branch_ir));

  // move true
  env->emit(std::make_unique<IR_RegSet>(
      result, compile_get_sym_obj("#t", env)->to_gpr(env)));  // todo, can be optimized
  branch_ir_ref->label.idx = branch_ir_ref->label.func->code().size();
  branch_ir_ref->mark_as_resolved();

  return result;
}

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
  get_parent_env_of_type<FunctionEnv>(env)->unresolved_cond_gotos.push_back({branch.get(), label});
  env->emit(std::move(branch));
  return get_none();
}

Val* Compiler::compile_cond(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto result = env->make_gpr(m_ts.make_typespec("object"));

  auto fenv = get_parent_env_of_type<FunctionEnv>(env);
  auto end_label = fenv->alloc_unnamed_label();
  end_label->func = fenv;
  end_label->idx = -3;  // placeholder

  bool got_else = false;

  std::vector<TypeSpec> case_result_types;

  for_each_in_list(rest, [&](const goos::Object& o) {
    auto test = pair_car(o);
    auto clauses = pair_cdr(o);

    if (got_else) {
      throw_compile_error(form, "cannot have anything after an else in a cond");
    }

    if (test.is_symbol() && symbol_string(test) == "else") {
      got_else = true;
    }

    if (got_else) {
      // just set the output to this.
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
      });

      case_result_types.push_back(case_result->type());

      // optimization - if we get junk, don't bother moving it, just leave junk in return.
      if (!is_none(case_result)) {
        env->emit(std::make_unique<IR_RegSet>(result, case_result->to_gpr(env)));
      }

    } else {
      // CONDITION CHECK
      auto condition = compile_condition(test, env, true);

      // BRANCH FWD
      auto branch_ir = std::make_unique<IR_ConditionalBranch>(condition, Label());
      auto branch_ir_ref = branch_ir.get();
      branch_ir->mark_as_resolved();
      env->emit(std::move(branch_ir));

      // CODE
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
      });

      case_result_types.push_back(case_result->type());
      if (!is_none(case_result)) {
        env->emit(std::make_unique<IR_RegSet>(result, case_result->to_gpr(env)));
      }

      // GO TO END
      auto ir_goto_end = std::make_unique<IR_GotoLabel>(end_label);
      env->emit(std::move(ir_goto_end));

      // PATCH BRANCH FWD
      branch_ir_ref->label.idx = fenv->code().size();
    }
  });

  if (!got_else) {
    // if no else, clause, return #f.  But don't retype.  I don't know how I feel about this typing
    // setup.
    auto get_false = std::make_unique<IR_LoadSymbolPointer>(result, "#f");
    env->emit(std::move(get_false));
  }

  result->set_type(coerce_to_reg_type(m_ts.lowest_common_ancestor(case_result_types)));

  // PATCH END
  end_label->idx = fenv->code().size();

  return result;
}