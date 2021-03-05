#include "goalc/compiler/Compiler.h"

MathMode Compiler::get_math_mode(const TypeSpec& ts) {
  if (m_ts.tc(m_ts.make_typespec("binteger"), ts)) {
    return MATH_BINT;
  }

  if (m_ts.tc(m_ts.make_typespec("integer"), ts)) {
    return MATH_INT;
  }

  if (m_ts.tc(m_ts.make_typespec("float"), ts)) {
    return MATH_FLOAT;
  }

  return MATH_INVALID;
}

bool Compiler::is_number(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("number"), ts);
}

bool Compiler::is_float(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("float"), ts);
}

bool Compiler::is_integer(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("integer"), ts) && !m_ts.tc(m_ts.make_typespec("binteger"), ts);
}

bool Compiler::is_binteger(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("binteger"), ts);
}

bool Compiler::is_singed_integer_or_binteger(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("integer"), ts) && !m_ts.tc(m_ts.make_typespec("uinteger"), ts);
}

Val* Compiler::number_to_integer(const goos::Object& form, Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw_compiler_error(form, "Cannot convert {} (a binteger) to an integer yet.", in->print());
  } else if (is_float(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_FloatToInt>(result, in->to_fpr(env)));
    return result;
  } else if (is_integer(ts)) {
    return in;
  }
  throw_compiler_error(form, "Cannot convert a {} to an integer.", in->type().print());
  return nullptr;
}

Val* Compiler::number_to_binteger(const goos::Object& form, Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    return in;
  } else if (is_float(ts)) {
    throw_compiler_error(form, "Cannot convert {} (a float) to an integer yet.", in->print());
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    RegVal* input = in->to_reg(env);
    auto sa = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_LoadConstant64>(sa, 3));
    auto result = compile_variable_shift(form, input, sa, env, IntegerMathKind::SHLV_64);
    result->set_type(m_ts.make_typespec("binteger"));
    return result;
  }
  throw_compiler_error(form, "Cannot convert a {} to a binteger.", in->type().print());
  return nullptr;
}

Val* Compiler::number_to_float(const goos::Object& form, Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw_compiler_error(form, "Cannot convert {} (a binteger) to an float yet.", in->print());
  } else if (is_float(ts)) {
    return in;
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_fpr(m_ts.make_typespec("float"));
    env->emit(std::make_unique<IR_IntToFloat>(result, in->to_gpr(env)));
    return result;
  }
  throw_compiler_error(form, "Cannot convert a {} to a float.", in->type().print());
  return nullptr;
}

Val* Compiler::to_math_type(const goos::Object& form, Val* in, MathMode mode, Env* env) {
  switch (mode) {
    case MATH_BINT:
      return number_to_binteger(form, in, env);
    case MATH_INT:
      return number_to_integer(form, in, env);
    case MATH_FLOAT:
      return number_to_float(form, in, env);
    default:
      throw_compiler_error(form, "Cannot do math on a {}.", in->type().print());
  }
  return nullptr;
}

Val* Compiler::compile_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid + form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT:
    case MATH_BINT: {
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_gpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::ADD_64, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_fpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_fpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::ADD_SS, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_fpr(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_mul(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid * form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      // todo, signed vs unsigned?
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_gpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::IMUL_32, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }
    case MATH_FLOAT: {
      auto result = env->make_fpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_fpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::MUL_SS, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_fpr(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_fmin(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid fmin form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  if (get_math_mode(first_val->type()) != MATH_FLOAT) {
    throw_compiler_error(form, "Must use floats in fmin");
  }
  auto result = env->make_fpr(first_val->type());
  env->emit(std::make_unique<IR_RegSet>(result, first_val->to_fpr(env)));
  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto val = compile_error_guard(args.unnamed.at(i), env);
    if (get_math_mode(val->type()) != MATH_FLOAT) {
      throw_compiler_error(form, "Must use floats in fmin");
    }
    env->emit(std::make_unique<IR_FloatMath>(FloatMathKind::MIN_SS, result, val->to_fpr(env)));
  }
  return result;
}

Val* Compiler::compile_fmax(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid fmax form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  if (get_math_mode(first_val->type()) != MATH_FLOAT) {
    throw_compiler_error(form, "Must use floats in fmax");
  }
  auto result = env->make_fpr(first_val->type());
  env->emit(std::make_unique<IR_RegSet>(result, first_val->to_fpr(env)));
  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto val = compile_error_guard(args.unnamed.at(i), env);
    if (get_math_mode(val->type()) != MATH_FLOAT) {
      throw_compiler_error(form, "Must use floats in fmax");
    }
    env->emit(std::make_unique<IR_FloatMath>(FloatMathKind::MAX_SS, result, val->to_fpr(env)));
  }
  return result;
}

Val* Compiler::compile_sqrtf(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  if (get_math_mode(first_val->type()) != MATH_FLOAT) {
    throw_compiler_error(form, "Must use a float for sqrtf");
  }
  auto result = env->make_fpr(first_val->type());
  env->emit_ir<IR_FloatMath>(FloatMathKind::SQRT_SS, result, first_val->to_fpr(env));
  return result;
}

Val* Compiler::compile_imul64(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid imul64 form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_gpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::IMUL_64, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }
    case MATH_FLOAT:
    case MATH_INVALID:
    case MATH_BINT:
      throw_compiler_error(form, "Cannot do imul64 on a {}.", first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_sub(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid - form");
  }

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT:
      if (args.unnamed.size() == 1) {
        auto result = compile_integer(0, env)->to_gpr(env);
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::SUB_64, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_gpr(env)));
        return result;
      } else {
        auto result = env->make_gpr(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(form, compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_gpr(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_IntegerMath>(
              IntegerMathKind::SUB_64, result,
              to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_gpr(env)));
        }
        return result;
      }

    case MATH_FLOAT:
      if (args.unnamed.size() == 1) {
        auto result =
            compile_float(0, env, get_parent_env_of_type<FunctionEnv>(env)->segment)->to_fpr(env);
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::SUB_SS, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_fpr(env)));
        return result;
      } else {
        auto result = env->make_fpr(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(form, compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_fpr(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_FloatMath>(
              FloatMathKind::SUB_SS, result,
              to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_fpr(env)));
        }
        return result;
      }

    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_div(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.size() != 2) {
    throw_compiler_error(form, "Invalid / form");
  }

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto fe = get_parent_env_of_type<FunctionEnv>(env);
      auto first_thing = first_val->to_gpr(env);
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_thing));

      IRegConstraint result_rax_constraint;
      result_rax_constraint.instr_idx = fe->code().size();
      result_rax_constraint.ireg = result->ireg();
      result_rax_constraint.desired_register = emitter::RAX;
      fe->constrain(result_rax_constraint);

      env->emit(std::make_unique<IR_IntegerMath>(
          IntegerMathKind::IDIV_32, result,
          to_math_type(form, compile_error_guard(args.unnamed.at(1), env), math_type, env)
              ->to_gpr(env)));
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_fpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_fpr(env)));
      env->emit(std::make_unique<IR_FloatMath>(
          FloatMathKind::DIV_SS, result,
          to_math_type(form, compile_error_guard(args.unnamed.at(1), env), math_type, env)
              ->to_fpr(env)));
      return result;
    }

    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_variable_shift(const goos::Object& form,
                                      const RegVal* in,
                                      const RegVal* sa,
                                      Env* env,
                                      IntegerMathKind kind) {
  auto result = env->make_gpr(in->type());
  auto sa_in = env->make_gpr(sa->type());

  env->emit(std::make_unique<IR_RegSet>(result, in));
  env->emit(std::make_unique<IR_RegSet>(sa_in, sa));
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  IRegConstraint sa_con;
  sa_con.ireg = sa_in->ireg();
  sa_con.instr_idx = fenv->code().size();
  sa_con.desired_register = emitter::RCX;

  if (get_math_mode(in->type()) != MathMode::MATH_INT ||
      get_math_mode(sa->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot shift a {} by a {}", in->type().print(), sa->type().print());
  }

  fenv->constrain(sa_con);
  env->emit(std::make_unique<IR_IntegerMath>(kind, result, sa_in));
  return result;
}

Val* Compiler::compile_shl(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  int64_t constant_sa = -1;
  if (try_getting_constant_integer(args.unnamed.at(1), &constant_sa, env)) {
    if (constant_sa < 0 || constant_sa > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, constant_sa, env, IntegerMathKind::SHL_64);
  } else {
    auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
    return compile_variable_shift(form, first, second, env, IntegerMathKind::SHLV_64);
  }
}

Val* Compiler::compile_shr(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  int64_t constant_sa = -1;
  if (try_getting_constant_integer(args.unnamed.at(1), &constant_sa, env)) {
    if (constant_sa < 0 || constant_sa > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, constant_sa, env, IntegerMathKind::SHR_64);
  } else {
    auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
    return compile_variable_shift(form, first, second, env, IntegerMathKind::SHRV_64);
  }
}

Val* Compiler::compile_sar(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  int64_t constant_sa = -1;
  if (try_getting_constant_integer(args.unnamed.at(1), &constant_sa, env)) {
    if (constant_sa < 0 || constant_sa > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, constant_sa, env, IntegerMathKind::SAR_64);
  } else {
    auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
    return compile_variable_shift(form, first, second, env, IntegerMathKind::SARV_64);
  }
}

Val* Compiler::compile_fixed_shift(const goos::Object& form,
                                   const RegVal* in,
                                   u8 sa,
                                   Env* env,
                                   IntegerMathKind kind) {
  // type check
  if (get_math_mode(in->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot shift a {}.", in->type().print());
  }

  if (sa > 64) {
    throw_compiler_error(form, "Cannot shift by more than 64.");
  }

  // copy to result register
  auto result = env->make_gpr(in->type());
  env->emit(std::make_unique<IR_RegSet>(result, in));
  // do the shift
  env->emit(std::make_unique<IR_IntegerMath>(kind, result, sa));
  return result;
}

Val* Compiler::compile_mod(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot mod a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));

  IRegConstraint con;
  con.ireg = result->ireg();
  con.instr_idx = fenv->code().size();
  con.desired_register = emitter::RAX;

  fenv->constrain(con);
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::IMOD_32, result, second));
  return result;
}

Val* Compiler::compile_logand(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot logand a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::AND_64, result, second));
  return result;
}

Val* Compiler::compile_logior(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot logior a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::OR_64, result, second));
  return result;
}

Val* Compiler::compile_logxor(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot logxor a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::XOR_64, result, second));
  return result;
}

Val* Compiler::compile_lognot(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot lognot a {}.", first->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::NOT_64, result, nullptr));
  return result;
}