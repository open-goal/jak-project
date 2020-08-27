/*!
 * @file GoalControlFlow.cpp
 * Branching control flow implementation.
 * Contains "cond", the only control flow structure known to the compiler, and branch condition
 * optimizations.
 */

#include "Goal.h"
#include "util.h"

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
GoalCondition Goal::compile_condition(Object condition, std::shared_ptr<GoalEnv> env, bool invert) {
  GoalCondition gc;

  // These are special conditions that can be optimized into a cmp + jxx instruction.
  const std::unordered_map<std::string, ConditionKind> conditions_inverted = {
      {"!=", ConditionKind::EQUAL_64},   {"eq?", ConditionKind::NOT_EQUAL_64},
      {"neq?", ConditionKind::EQUAL_64}, {"=", ConditionKind::NOT_EQUAL_64},
      {">", ConditionKind::LEQ_64},      {"<", ConditionKind::GEQ_64},
      {">=", ConditionKind::LT_64},      {"<=", ConditionKind::GT_64}};

  const std::unordered_map<std::string, ConditionKind> conditions_normal = {
      {"!=", ConditionKind::NOT_EQUAL_64},   {"eq?", ConditionKind::EQUAL_64},
      {"neq?", ConditionKind::NOT_EQUAL_64}, {"=", ConditionKind::EQUAL_64},
      {">", ConditionKind::GT_64},           {"<", ConditionKind::LT_64},
      {">=", ConditionKind::GEQ_64},         {"<=", ConditionKind::LEQ_64}};

  // possibly a form with an optimizable condition?
  if (condition.type == PAIR) {
    auto first = pair_car(condition);
    auto rest = pair_cdr(condition);

    if (first.type == SYMBOL) {
      auto fas = first.as_symbol();

      // if there's a not, we can just try again to get an optimization with the invert flipped.
      if (fas->name == "not") {
        auto arg = pair_car(rest);
        if (pair_cdr(rest).type != EMPTY_LIST) {
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
        auto args = goos.get_uneval_args_no_rest(rest, rest, 2);
        if (!args.named_args.empty() || args.unnamed_args.size() != 2) {
          throw_compile_error(rest, "invalid arguments to " + nc_kv->first);
        }
        auto first_arg = compile_error_guard(args.unnamed_args.at(0), env);
        auto second_arg = compile_error_guard(args.unnamed_args.at(1), env);

        if (is_number(first_arg->type)) {
          // it's a numeric comparison, so we may need to coerce.

          // there is no support for comparing bintegers, so we turn the binteger comparison into an
          // integer.
          if (is_binteger(first_arg->type)) {
            first_arg = to_integer(first_arg, env);
          }

          // convert second one to appropriate type as needed
          if (is_number(second_arg->type)) {
            second_arg = to_same_numeric_type(second_arg, first_arg->type, env);
          }
        }

        // use signed comparison only if first argument is a signed integer (or coerced binteger)
        // (floating point ignores this)
        gc.is_signed = is_signed_integer(first_arg->type);

        // pick between a floating point and an integer comparison.
        if (is_float(first_arg->type)) {
          gc.a = resolve_to_xmm(first_arg, env);
          gc.b = resolve_to_xmm(second_arg, env);
          gc.is_float = true;
        } else {
          gc.a = resolve_to_gpr(first_arg, env);
          gc.b = resolve_to_gpr(second_arg, env);
        }

        return gc;
      }
    }
  }

  // not something we can optimize.  Just check if we get false.
  // todo - it's possible to optimize a false comparison because the false offset is zero
  gc.kind = invert ? EQUAL_64 : NOT_EQUAL_64;
  gc.a = resolve_to_gpr(compile_error_guard(condition, env), env);
  gc.b = compile_get_sym_obj("#f", env);

  return gc;
}

/*!
 * In the event that we have an expression like (< 1 2) that's _not_ a branch condition,
 * we can reuse the logic of the above comparison, and just set up an (if cond #t #f)-like program.
 */
std::shared_ptr<Place> Goal::compile_condition_as_bool(const Object& form,
                                                       Object rest,
                                                       std::shared_ptr<GoalEnv> env) {
  (void)rest;
  auto c = compile_condition(form, env, true);
  auto result = compile_get_sym_obj("#f", env);  // todo - can be optimized.
  auto branch_ir = make_unique<IR_ConditionalBranch>();
  auto branch_ir_ref = branch_ir.get();
  branch_ir->cond = c;
  branch_ir->label = std::make_shared<Label>();
  branch_ir->label->func = get_parent_env_of_type<FunctionEnv>(env).get();
  branch_ir->label->idx = -5;  // placeholder
  branch_ir->resolved = true;
  env->emit(std::move(branch_ir));

  // move true
  env->emit(make_unique<IR_Set>(result, compile_get_sym_obj("#t", env)));
  branch_ir_ref->label->idx = branch_ir_ref->label->func->code.size();

  return result;
}

std::shared_ptr<Place> Goal::compile_when_goto(const Object& form,
                                               Object rest,
                                               std::shared_ptr<GoalEnv> env) {
  (void)form;
  auto condition_code = pair_car(rest);
  rest = pair_cdr(rest);

  auto label = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  // compile as condition (will set flags register with a cmp instruction)
  auto condition = compile_condition(condition_code, env, false);
  auto branch = make_unique<IR_ConditionalBranch>();
  branch->cond = condition;
  branch->label = nullptr;  // will be resolved later
  branch->resolved = false;
  get_parent_env_of_type<FunctionEnv>(env)->unresolved_cond_gotos.push_back({branch.get(), label});
  env->emit(std::move(branch));
  return get_none();
}

/*!
 * The scheme "cond" statement.
 */
std::shared_ptr<Place> Goal::compile_cond(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  auto result = env->alloc_reg(get_base_typespec("object"));

  auto end_label = std::make_shared<Label>();
  auto fenv = get_parent_env_of_type<FunctionEnv>(env).get();
  end_label->func = fenv;
  end_label->idx = -3;  // placeholder

  bool got_else = false;

  std::vector<TypeSpec> case_result_types;

  for_each_in_list(rest, [&](Object o) {
    auto test = pair_car(o);
    auto clauses = pair_cdr(o);

    if (got_else) {
      throw_compile_error(form, "cannot have anything after an else in a cond");
    }

    if (test.type == SYMBOL && symbol_string(test) == "else") {
      got_else = true;
    }

    if (got_else) {
      // just set the output to this.
      auto case_result = get_none();
      for_each_in_list(clauses,
                       [&](Object clause) { case_result = compile_error_guard(clause, env); });

      case_result_types.push_back(case_result->type);

      // optimization - if we get junk, don't bother moving it, just leave junk in return.
      if (!is_none(case_result)) {
        env->emit(make_unique<IR_Set>(result, resolve_to_gpr(case_result, env)));
      }

    } else {
      // CONDITION CHECK
      auto condition = compile_condition(test, env, true);

      // BRANCH FWD
      auto branch_ir = make_unique<IR_ConditionalBranch>();
      auto branch_ir_ref = branch_ir.get();
      branch_ir->cond = condition;
      branch_ir->label = std::make_shared<Label>();
      branch_ir->label->func = fenv;
      branch_ir->label->idx = -2;  // temporary placeholder
      branch_ir->resolved = true;
      env->emit(std::move(branch_ir));

      // CODE
      auto case_result = get_none();
      for_each_in_list(clauses,
                       [&](Object clause) { case_result = compile_error_guard(clause, env); });

      case_result_types.push_back(case_result->type);
      if (!is_none(case_result)) {
        env->emit(make_unique<IR_Set>(result, resolve_to_gpr(case_result, env)));
      }

      // GO TO END
      auto ir_goto_end = make_unique<IR_Goto_Label>();
      ir_goto_end->resolved = true;
      ir_goto_end->label = end_label;
      env->emit(std::move(ir_goto_end));

      // PATCH BRANCH FWD
      branch_ir_ref->label->idx = fenv->code.size();
    }
  });

  if (!got_else) {
    // if no else, clause, return #f.  But don't retype.  I don't know how I feel about this typing
    // setup.
    auto get_false = make_unique<IR_GetSymbolObj>();
    auto sym_ts = get_base_typespec("symbol");
    get_false->sym = std::make_shared<SymbolPlace>("#f", sym_ts);
    get_false->dest = result;
    env->emit(std::move(get_false));
  }

  result->type = lowest_common_ancestor(case_result_types);

  // PATCH END
  end_label->idx = fenv->code.size();

  return result;
}

// TODO - move optimized and/ors into the compiler?