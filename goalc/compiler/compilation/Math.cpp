#include "goalc/compiler/Compiler.h"

MathMode Compiler::get_math_mode(const TypeSpec& ts) {
  if (m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false)) {
    return MATH_BINT;
  }

  if (m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false)) {
    return MATH_INT;
  }

  if (m_ts.typecheck(m_ts.make_typespec("float"), ts, "", false, false)) {
    return MATH_FLOAT;
  }

  return MATH_INVALID;
}

bool Compiler::is_number(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("number"), ts, "", false, false);
}

bool Compiler::is_float(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("float"), ts, "", false, false);
}

bool Compiler::is_integer(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false) &&
         !m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false);
}

bool Compiler::is_binteger(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false);
}

bool Compiler::is_singed_integer_or_binteger(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false) &&
         !m_ts.typecheck(m_ts.make_typespec("uinteger"), ts, "", false, false);
}

Val* Compiler::number_to_integer(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a binteger) to an integer.");
  } else if (is_float(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_FloatToInt>(result, in->to_xmm(env)));
    return result;
  } else if (is_integer(ts)) {
    return in;
  }
  throw std::runtime_error("Can't convert " + in->print() + " to an integer.");
}

Val* Compiler::number_to_binteger(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    return in;
  } else if (is_float(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a float) to a binteger.");
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    RegVal* input = in->to_reg(env);
    auto sa = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_LoadConstant64>(sa, 3));
    return compile_variable_shift(input, sa, env, IntegerMathKind::SHLV_64);
  }
  throw std::runtime_error("Can't convert " + in->print() + " to a binteger.");
}

Val* Compiler::number_to_float(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a binteger) to a float.");
  } else if (is_float(ts)) {
    return in;
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_xmm(m_ts.make_typespec("float"));
    env->emit(std::make_unique<IR_IntToFloat>(result, in->to_gpr(env)));
    return result;
  } else {
    throw std::runtime_error("Can't convert " + in->print() + " a float.");
  }
}

Val* Compiler::to_math_type(Val* in, MathMode mode, Env* env) {
  switch (mode) {
    case MATH_BINT:
      return number_to_binteger(in, env);
    case MATH_INT:
      return number_to_integer(in, env);
    case MATH_FLOAT:
      return number_to_float(in, env);
    default:
      throw std::runtime_error("Unknown math type: " + in->print());
  }
}

Val* Compiler::compile_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compile_error(form, "Invalid + form");
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
            IntegerMathKind::ADD_64, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::ADD_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_xmm(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
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
    throw_compile_error(form, "Invalid * form");
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
            IntegerMathKind::IMUL_32, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }
    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::MUL_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_xmm(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
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
    throw_compile_error(form, "Invalid - form");
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
            to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_gpr(env)));
        return result;
      } else {
        auto result = env->make_gpr(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_gpr(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_IntegerMath>(
              IntegerMathKind::SUB_64, result,
              to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_gpr(env)));
        }
        return result;
      }

    case MATH_FLOAT:
      if (args.unnamed.size() == 1) {
        auto result =
            compile_float(0, env, get_parent_env_of_type<FunctionEnv>(env)->segment)->to_xmm(env);
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::SUB_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_xmm(env)));
        return result;
      } else {
        auto result = env->make_xmm(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_xmm(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_FloatMath>(
              FloatMathKind::SUB_SS, result,
              to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_xmm(env)));
        }
        return result;
      }

    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
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
    throw_compile_error(form, "Invalid / form");
  }

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto fe = get_parent_env_of_type<FunctionEnv>(env);
      auto first_thing = first_val->to_gpr(env);
      auto result = env->make_ireg(first_type, emitter::RegKind::GPR);
      env->emit(std::make_unique<IR_RegSet>(result, first_thing));

      IRegConstraint result_rax_constraint;
      result_rax_constraint.instr_idx = fe->code().size();
      result_rax_constraint.ireg = result->ireg();
      result_rax_constraint.desired_register = emitter::RAX;
      fe->constrain(result_rax_constraint);

      env->emit(std::make_unique<IR_IntegerMath>(
          IntegerMathKind::IDIV_32, result,
          to_math_type(compile_error_guard(args.unnamed.at(1), env), math_type, env)->to_gpr(env)));
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));
      env->emit(std::make_unique<IR_FloatMath>(
          FloatMathKind::DIV_SS, result,
          to_math_type(compile_error_guard(args.unnamed.at(1), env), math_type, env)->to_xmm(env)));
      return result;
    }

    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_shlv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SHLV_64);
}

Val* Compiler::compile_sarv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SARV_64);
}

Val* Compiler::compile_shrv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SHRV_64);
}

Val* Compiler::compile_variable_shift(const RegVal* in,
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
    throw std::runtime_error("Can't shift a " + in->type().print() + " by a " + sa->type().print());
  }

  fenv->constrain(sa_con);
  env->emit(std::make_unique<IR_IntegerMath>(kind, result, sa_in));
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
    throw std::runtime_error("Can't mod a " + first->type().print() + " by a " +
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
    throw std::runtime_error("Can't logand a " + first->type().print() + " by a " +
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
    throw std::runtime_error("Can't logior a " + first->type().print() + " by a " +
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
    throw std::runtime_error("Can't logxor a " + first->type().print() + " by a " +
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
    throw std::runtime_error("Can't lognot a " + first->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::NOT_64, result, nullptr));
  return result;
}