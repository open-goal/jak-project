#include "Goal.h"
#include "util.h"

MathMode Goal::get_math_mode(TypeSpec& ts) {
  TypeSpec bint_ts = get_base_typespec("binteger");
  TypeSpec int_ts = get_base_typespec("integer");
  TypeSpec float_ts = get_base_typespec("float");
  if (bint_ts.typecheck_base_only(ts, types)) {
    return MATH_BINT;
  } else if (float_ts.typecheck_base_only(ts, types)) {
    return MATH_FLOAT;
  } else if (int_ts.typecheck_base_only(ts, types)) {
    return MATH_INT;
  } else {
    return MATH_INVALID;
  }
}

std::shared_ptr<Place> Goal::to_integer(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env) {
  (void)env;  // we'll need this later on to emit conversion instructions.
  TypeSpec int_ts = get_base_typespec("integer");
  if (is_binteger(in->type)) {
    auto result = env->alloc_reg(get_base_typespec("integer"));
    env->emit(make_unique<IR_Set>(result, resolve_to_gpr(in, env)));
    return compile_shift(result, compile_integer_to_gpr(3, env), env, false, true);
  } else if (int_ts.typecheck_base_only(in->type, types)) {
    return in;
  }
  if (is_float(in->type)) {
    auto result = env->alloc_reg(get_base_typespec("integer"));
    env->emit(make_unique<IR_FloatToInt>(result, resolve_to_xmm(in, env)));
    return result;
  } else {
    // todo, fail with a slightly better error message.
    throw std::runtime_error("can't convert value " + in->print() + " to an integer!");
  }
}

std::shared_ptr<Place> Goal::to_binteger(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env) {
  if (is_binteger(in->type)) {
    return in;
  } else if (is_integer(in->type)) {
    in = resolve_to_gpr(in, env);
    auto result = compile_shift(in, compile_integer_to_gpr(3, env), env, true, false);
    result->type = get_base_typespec("binteger");
    return result;
  } else {
    throw std::runtime_error("can't convert value " + in->print() + " to an binteger!");
  }
}

std::shared_ptr<Place> Goal::to_float(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env) {
  (void)env;
  if (is_float(in->type)) {
    return in;
  }
  if (is_integer(in->type)) {
    auto result =
        get_parent_env_of_type<FunctionEnv>(env)->alloc_xmm_reg(get_base_typespec("float"));
    env->emit(make_unique<IR_IntToFloat>(result, resolve_to_gpr(in, env)));
    return result;
  } else {
    throw std::runtime_error("can't convert value " + in->print() + " to an float!");
  }
}

bool Goal::is_number(TypeSpec& ts) {
  return get_base_typespec("number").typecheck_base_only(ts, types);
}

bool Goal::is_float(TypeSpec& ts) {
  return get_base_typespec("float").typecheck_base_only(ts, types);
}

bool Goal::is_binteger(TypeSpec& ts) {
  return get_base_typespec("binteger").typecheck_base_only(ts, types);
}

bool Goal::is_integer(TypeSpec& ts) {
  return get_base_typespec("integer").typecheck_base_only(ts, types);
}

bool Goal::is_signed_integer(TypeSpec& ts) {
  return get_base_typespec("integer").typecheck_base_only(ts, types) &&
         !get_base_typespec("uinteger").typecheck_base_only(ts, types);
}

std::shared_ptr<Place> Goal::to_same_numeric_type(std::shared_ptr<Place> obj,
                                                  TypeSpec numeric_type,
                                                  std::shared_ptr<GoalEnv> env) {
  if (is_float(numeric_type)) {
    return to_float(obj, env);
  } else if (is_integer(numeric_type)) {
    return to_integer(obj, env);
  } else if (is_binteger(numeric_type)) {
    return to_binteger(obj, env);
  }

  throw std::runtime_error("couldn't convert to same numeric type: " + numeric_type.print());
}

std::shared_ptr<Place> Goal::compile_add(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  if (rest.type != PAIR) {
    throw_compile_error(form, "+ must get at least one argument!");
  }

  auto first = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  auto first_thing = resolve_to_gpr_or_xmm(compile_error_guard(first, env), env);
  auto first_type = first_thing->type;

  auto math_type = get_math_mode(first_type);

  if (math_type == MATH_INT) {
    auto result = env->alloc_reg(first_type);
    env->emit(make_unique<IR_Set>(result, first_thing));

    for_each_in_list(rest, [&](Object o) {
      // todo - constant prop!
      env->emit(make_unique<IR_IntegerMath>(
          ADD_64, result, resolve_to_gpr(to_integer(compile_error_guard(o, env), env), env)));
    });
    return result;
  } else if (math_type == MATH_FLOAT) {
    auto result = get_parent_env_of_type<FunctionEnv>(env)->alloc_xmm_reg(first_type);
    env->emit(make_unique<IR_Set>(result, first_thing));

    for_each_in_list(rest, [&](Object o) {
      env->emit(make_unique<IR_FloatMath>(
          ADD_SS, result, resolve_to_xmm(to_float(compile_error_guard(o, env), env), env)));
    });
    return result;
  } else if (math_type == MATH_BINT) {
    auto result = env->alloc_reg(get_base_typespec("integer"));
    env->emit(make_unique<IR_Set>(result, to_integer(first_thing, env)));

    for_each_in_list(rest, [&](Object o) {
      // todo - constant prop!
      env->emit(make_unique<IR_IntegerMath>(
          ADD_64, result, resolve_to_gpr(to_integer(compile_error_guard(o, env), env), env)));
    });
    result = to_binteger(result, env);
    return result;
  }

  else {
    throw_compile_error(form, "invalid math mode");
  }

  return get_none();
}

std::shared_ptr<Place> Goal::compile_logop(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env,
                                           LogOpKind kind) {
  if (rest.type != PAIR) {
    throw_compile_error(form, "LOGOP must get at least one argument!");
  }

  auto first = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  auto first_thing = resolve_to_gpr(compile_error_guard(first, env), env);
  auto first_type = first_thing->type;
  auto result = env->alloc_reg(first_type);
  env->emit(make_unique<IR_Set>(result, first_thing));

  auto math_type = get_math_mode(first_type);
  if (math_type == MATH_INVALID) {
    throw_compile_error(form, "invalid math mode");
  }

  IntegerMathKind math_kind;

  switch (kind) {
    case LOGXOR:
      math_kind = XOR_64;
      break;
    case LOGAND:
      math_kind = AND_64;
      break;
    case LOGIOR:
      math_kind = OR_64;
      break;
    default:
      throw std::runtime_error("unknown logop kind");
  }

  for_each_in_list(rest, [&](Object o) {
    // todo - constant prop!

    switch (math_type) {
      case MATH_INT:
        env->emit(make_unique<IR_IntegerMath>(
            math_kind, result, resolve_to_gpr(to_integer(compile_error_guard(o, env), env), env)));
        break;

      default:
        throw_compile_error(form, "unhandled math for " + o.print());
    }
  });

  return result;
}

std::shared_ptr<Place> Goal::compile_logand(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  return compile_logop(form, rest, env, LOGAND);
}

std::shared_ptr<Place> Goal::compile_logior(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  return compile_logop(form, rest, env, LOGIOR);
}

std::shared_ptr<Place> Goal::compile_logxor(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  return compile_logop(form, rest, env, LOGXOR);
}

std::shared_ptr<Place> Goal::compile_lognot(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  if (rest.type != PAIR) {
    throw_compile_error(form, "lognot must get 1 argument");
  }

  if (rest.as_pair()->cdr.type != EMPTY_LIST) {
    throw_compile_error(form, "lognot must get 1 argument");
  }

  auto val = resolve_to_gpr(compile_error_guard(rest.as_pair()->car, env), env);
  auto type = val->type;
  auto math_type = get_math_mode(type);
  auto result = env->alloc_reg(type);

  if (math_type != MATH_INT) {
    throw_compile_error(form, "invalid math mode for lognot");
  }

  env->emit(make_unique<IR_Set>(result, val));
  env->emit(make_unique<IR_IntegerMath>(NOT_64, result, result));

  return result;
}

std::shared_ptr<Place> Goal::compile_sub(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  if (rest.type != PAIR) {
    throw_compile_error(form, "- must get at least one argument!");
  }

  auto first = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  // auto result = compile_error_guard(first, env);
  auto first_thing = compile_error_guard(first, env);
  auto first_type = first_thing->type;
  auto math_type = get_math_mode(first_type);

  if (math_type == MATH_INT) {
    auto result = env->alloc_reg(first_type);

    if (rest.type == EMPTY_LIST) {
      // just negate the argument!
      auto zero_ir = make_unique<IR_LoadInteger>();
      zero_ir->size = 1;
      zero_ir->is_signed = true;
      zero_ir->s_value = 0;
      zero_ir->value = result;
      env->emit(std::move(zero_ir));

      env->emit(make_unique<IR_IntegerMath>(SUB_64, result, resolve_to_gpr(first_thing, env)));
      return result;

    } else {
      env->emit(make_unique<IR_Set>(result, resolve_to_gpr(first_thing, env)));
      for_each_in_list(rest, [&](Object o) {
        // todo - constant prop!
        env->emit(make_unique<IR_IntegerMath>(
            SUB_64, result, resolve_to_gpr(to_integer(compile_error_guard(o, env), env), env)));
      });

      return result;
    }
  } else if (math_type == MATH_FLOAT) {
    auto result = get_parent_env_of_type<FunctionEnv>(env)->alloc_xmm_reg(first_type);
    if (rest.type == EMPTY_LIST) {
      // just negate argument

      auto zero = compile_float(0.f, env);
      env->emit(make_unique<IR_Set>(result, resolve_to_xmm(zero, env)));
      env->emit(make_unique<IR_FloatMath>(SUB_SS, result, resolve_to_xmm(first_thing, env)));
      return result;
    } else {
      env->emit(make_unique<IR_Set>(result, resolve_to_xmm(first_thing, env)));
      for_each_in_list(rest, [&](Object o) {
        // todo - constant prop!
        env->emit(make_unique<IR_FloatMath>(
            SUB_SS, result, resolve_to_xmm(to_float(compile_error_guard(o, env), env), env)));
      });
      return result;
    }
  } else {
    throw_compile_error(form, "unhandled math for " + first.print());
  }

  return get_none();
}

std::shared_ptr<Place> Goal::compile_mult(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  if (rest.type != PAIR) {
    throw_compile_error(form, "* must get at least one argument!");
  }

  auto first = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  auto first_thing = compile_error_guard(first, env);
  auto first_type = first_thing->type;

  auto math_type = get_math_mode(first_type);
  if (math_type == MATH_INVALID) {
    throw_compile_error(form, "invalid math mode");
  }

  switch (math_type) {
    case MATH_INT: {
      auto result = env->alloc_reg(first_type);
      env->emit(make_unique<IR_Set>(result, resolve_to_gpr(first_thing, env)));
      for_each_in_list(rest, [&](Object o) {
        // todo - constant prop!
        env->emit(make_unique<IR_IntegerMath>(
            IMUL_32, result, resolve_to_gpr(to_integer(compile_error_guard(o, env), env), env)));
      });
      return result;
    } break;

    case MATH_FLOAT: {
      auto fenv = get_parent_env_of_type<FunctionEnv>(env);
      auto result = fenv->alloc_xmm_reg(first_type);
      env->emit(make_unique<IR_Set>(result, resolve_to_xmm(first_thing, env)));

      for_each_in_list(rest, [&](Object o) {
        // todo - constant prop!
        env->emit(make_unique<IR_FloatMath>(
            MUL_SS, result, resolve_to_xmm(to_float(compile_error_guard(o, env), env), env)));
      });
      return result;
    } break;

    default:
      throw_compile_error(form, "unhandled math for mult" + first_thing->print());
  }

  return get_none();
}

std::shared_ptr<Place> Goal::compile_divide(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args_no_rest(form, rest, 2);
  if (args.unnamed_args.size() != 2 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid / form");
  }

  auto first_thing = compile_error_guard(args.unnamed_args.at(0), env);
  auto second_thing = compile_error_guard(args.unnamed_args.at(1), env);
  auto math_type = get_math_mode(first_thing->type);
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  switch (math_type) {
    case MATH_INT: {
      first_thing = resolve_to_gpr(first_thing, env);
      auto result = env->alloc_reg(first_thing->type);
      env->emit(make_unique<IR_Set>(result, first_thing));

      RegConstraint result_rax_constraint;
      result_rax_constraint.instr_id = fenv->code.size() - 1;
      result_rax_constraint.var_id = fenv->vars.size() - 1;
      result_rax_constraint.ass.kind = AssignmentKind::REGISTER;
      result_rax_constraint.ass.reg_id = RAX;
      env->constrain_reg(result_rax_constraint);

      env->emit(make_unique<IR_IntegerMath>(IDIV_32, result,
                                            resolve_to_gpr(to_integer(second_thing, env), env)));
      return result;
    } break;

    case MATH_FLOAT: {
      auto result = fenv->alloc_xmm_reg(first_thing->type);
      env->emit(make_unique<IR_Set>(result, resolve_to_xmm(first_thing, env)));
      // todo - constant prop!
      env->emit(make_unique<IR_FloatMath>(DIV_SS, result,
                                          resolve_to_xmm(to_float(second_thing, env), env)));

      return result;
    } break;

    default:
      throw_compile_error(form, "unhandled math type in divide");
  }

  return get_none();
}

static IntegerMathKind get_shift_kind(bool is_left, bool is_arith, bool is_variable) {
  if (is_left) {
    return is_variable ? SHLV_64 : SHL_64;
  } else if (is_arith) {
    return is_variable ? SARV_64 : SAR_64;
  } else {
    return is_variable ? SHRV_64 : SHR_64;
  }
}

std::shared_ptr<Place> Goal::compile_shift(std::shared_ptr<Place> in,
                                           std::shared_ptr<Place> sa,
                                           std::shared_ptr<GoalEnv> env,
                                           bool is_left,
                                           bool is_arith) {
  auto result = env->alloc_reg(in->type);
  env->emit(make_unique<IR_Set>(result, resolve_to_gpr(in, env)));

  auto sa_reg = env->alloc_reg(sa->type);
  env->emit(make_unique<IR_Set>(sa_reg, resolve_to_gpr(sa, env)));
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  RegConstraint sa_con;
  sa_con.instr_id = fenv->code.size() - 1;
  sa_con.var_id = fenv->vars.size() - 1;
  sa_con.ass.kind = AssignmentKind::REGISTER;
  sa_con.ass.reg_id = RCX;
  env->constrain_reg(sa_con);

  auto mathkind = get_shift_kind(is_left, is_arith, true);

  auto math_type = get_math_mode(in->type);
  switch (math_type) {
    case MATH_INT:
      env->emit(make_unique<IR_IntegerMath>(mathkind, result, sa_reg));
      break;
    default:
      throw std::runtime_error("unhandled math type in compile_shift");
  }

  return result;
}

std::shared_ptr<Place> Goal::compile_shift(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env,
                                           bool is_left,
                                           bool is_arith) {
  auto args = goos.get_uneval_args_no_rest(form, rest, 2);
  if (args.unnamed_args.size() != 2 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid shlv form");
  }

  auto first_thing = compile_error_guard(args.unnamed_args.at(0), env);
  auto second_thing = compile_error_guard(args.unnamed_args.at(1), env);
  return compile_shift(first_thing, second_thing, env, is_left, is_arith);
}

std::shared_ptr<Place> Goal::compile_mod(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args_no_rest(form, rest, 2);
  if (args.unnamed_args.size() != 2 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid mod form");
  }

  auto first_thing = compile_error_guard(args.unnamed_args.at(0), env);
  first_thing = resolve_to_gpr(first_thing, env);
  auto second_thing = compile_error_guard(args.unnamed_args.at(1), env);
  auto math_type = get_math_mode(first_thing->type);
  auto result = env->alloc_reg(first_thing->type);
  env->emit(make_unique<IR_Set>(result, first_thing));
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  RegConstraint result_rax_constraint;
  result_rax_constraint.instr_id = fenv->code.size() - 1;
  result_rax_constraint.var_id = fenv->vars.size() - 1;
  result_rax_constraint.ass.kind = AssignmentKind::REGISTER;
  result_rax_constraint.ass.reg_id = RAX;
  env->constrain_reg(result_rax_constraint);

  switch (math_type) {
    case MATH_INT:
      env->emit(make_unique<IR_IntegerMath>(IMOD_32, result,
                                            resolve_to_gpr(to_integer(second_thing, env), env)));

      break;
    default:
      throw_compile_error(form, "unhandled math type in mod");
  }

  return result;
}

std::shared_ptr<Place> Goal::compile_shlv(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  return compile_shift(form, rest, env, true, false);
}

std::shared_ptr<Place> Goal::compile_sarv(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  return compile_shift(form, rest, env, false, true);
}

std::shared_ptr<Place> Goal::compile_shrv(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  return compile_shift(form, rest, env, false, false);
}

std::shared_ptr<Place> Goal::compile_fixed_shift(std::shared_ptr<Place> in,
                                                 uint8_t sa,
                                                 std::shared_ptr<GoalEnv> env,
                                                 bool is_left,
                                                 bool is_arith) {
  // force in to be in a register
  in = resolve_to_gpr(in, env);

  // get new register for result (keep same type as input)
  auto result = env->alloc_reg(in->type);

  // move into result register before shifting
  env->emit(make_unique<IR_Set>(result, in));

  auto math_type = get_math_mode(in->type);

  switch (math_type) {
    case MATH_INT:
      env->emit(make_unique<IR_IntegerMath>(get_shift_kind(is_left, is_arith, false), result, sa));
      break;
    default:
      ice("unsupported math type in compile_fixed_shift");
  }
  // do shift!

  return result;
}

std::shared_ptr<Place> Goal::compile_fixed_shift(const Object& form,
                                                 Object rest,
                                                 std::shared_ptr<GoalEnv> env,
                                                 bool is_left,
                                                 bool is_arith) {
  (void)form;
  auto value = compile_error_guard(pair_car(rest), env);
  rest = pair_cdr(rest);
  auto shift_amount = compile_to_integer_constant(pair_car(rest), env);
  expect_empty_list(pair_cdr(rest));
  return compile_fixed_shift(value, shift_amount, env, is_left, is_arith);
}

std::shared_ptr<Place> Goal::compile_shl(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  return compile_fixed_shift(form, rest, env, true, false);
}

std::shared_ptr<Place> Goal::compile_sar(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  return compile_fixed_shift(form, rest, env, false, true);
}

std::shared_ptr<Place> Goal::compile_shr(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  return compile_fixed_shift(form, rest, env, false, false);
}