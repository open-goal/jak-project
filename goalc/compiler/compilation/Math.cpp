#include <cfloat>

#include "common/goos/PrettyPrinter.h"
#include "common/util/BitUtils.h"

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
    auto fe = env->function_env();
    auto result = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit_ir<IR_FloatToInt>(form, result, in->to_fpr(form, env));
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
    auto fe = env->function_env();
    RegVal* input = in->to_reg(form, env);
    auto sa = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit_ir<IR_LoadConstant64>(form, sa, 3);
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
    auto fe = env->function_env();
    auto result = fe->make_fpr(m_ts.make_typespec("float"));
    env->emit_ir<IR_IntToFloat>(form, result, in->to_gpr(form, env));
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
      env->emit_ir<IR_RegSet>(form, result, first_val->to_gpr(form, env));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit_ir<IR_IntegerMath>(
            form, IntegerMathKind::ADD_64, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(form, env));
      }
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_fpr(first_type);
      env->emit_ir<IR_RegSet>(form, result, first_val->to_fpr(form, env));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit_ir<IR_FloatMath>(
            form, FloatMathKind::ADD_SS, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_fpr(form, env));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      ASSERT(false);
  }
  ASSERT(false);
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
      auto result = env->make_gpr(first_type);
      env->emit_ir<IR_RegSet>(form, result, first_val->to_gpr(form, env));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        auto val = compile_error_guard(args.unnamed.at(i), env);
        auto val_as_int = dynamic_cast<IntegerConstantVal*>(val);
        int power_of_two = -1;
        if (val_as_int && val_as_int->value().uses_gpr() && val_as_int->value().value_64() > 0) {
          auto p = get_power_of_two(val_as_int->value().value_64());
          if (p) {
            power_of_two = *p;
          }
        }

        if (power_of_two >= 0) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, result, power_of_two);
        } else {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::IMUL_32, result,
                                       to_math_type(form, val, math_type, env)->to_gpr(form, env));
        }
      }
      return result;
    }
    case MATH_FLOAT: {
      auto result = env->make_fpr(first_type);
      env->emit_ir<IR_RegSet>(form, result, first_val->to_fpr(form, env));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit_ir<IR_FloatMath>(
            form, FloatMathKind::MUL_SS, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_fpr(form, env));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      ASSERT(false);
  }
  ASSERT(false);
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
  env->emit_ir<IR_RegSet>(form, result, first_val->to_fpr(form, env));
  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto val = compile_error_guard(args.unnamed.at(i), env);
    if (get_math_mode(val->type()) != MATH_FLOAT) {
      throw_compiler_error(form, "Must use floats in fmin");
    }
    env->emit_ir<IR_FloatMath>(form, FloatMathKind::MIN_SS, result, val->to_fpr(form, env));
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
  env->emit_ir<IR_RegSet>(form, result, first_val->to_fpr(form, env));
  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto val = compile_error_guard(args.unnamed.at(i), env);
    if (get_math_mode(val->type()) != MATH_FLOAT) {
      throw_compiler_error(form, "Must use floats in fmax");
    }
    env->emit_ir<IR_FloatMath>(form, FloatMathKind::MAX_SS, result, val->to_fpr(form, env));
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
  env->emit_ir<IR_FloatMath>(form, FloatMathKind::SQRT_SS, result, first_val->to_fpr(form, env));
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
      env->emit_ir<IR_RegSet>(form, result, first_val->to_gpr(form, env));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit_ir<IR_IntegerMath>(
            form, IntegerMathKind::IMUL_64, result,
            to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(form, env));
      }
      return result;
    }
    case MATH_FLOAT:
    case MATH_INVALID:
    case MATH_BINT:
      throw_compiler_error(form, "Cannot do imul64 on a {}.", first_type.print());
      break;
    default:
      ASSERT(false);
  }
  ASSERT(false);
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
        auto result = compile_integer(0, env)->to_gpr(form, env);
        env->emit_ir<IR_IntegerMath>(
            form, IntegerMathKind::SUB_64, result,
            to_math_type(form, first_val, math_type, env)->to_gpr(form, env));
        return result;
      } else {
        auto result = env->make_gpr(first_type);
        env->emit_ir<IR_RegSet>(form, result,
                                to_math_type(form, first_val, math_type, env)->to_gpr(form, env));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit_ir<IR_IntegerMath>(
              form, IntegerMathKind::SUB_64, result,
              to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_gpr(form, env));
        }
        return result;
      }

    case MATH_FLOAT:
      if (args.unnamed.size() == 1) {
        auto result = compile_float(0, env, env->function_env()->segment)->to_fpr(form, env);
        env->emit_ir<IR_FloatMath>(
            form, FloatMathKind::SUB_SS, result,
            to_math_type(form, first_val, math_type, env)->to_fpr(form, env));
        return result;
      } else {
        auto result = env->make_fpr(first_type);
        env->emit_ir<IR_RegSet>(form, result,
                                to_math_type(form, first_val, math_type, env)->to_fpr(form, env));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit_ir<IR_FloatMath>(
              form, FloatMathKind::SUB_SS, result,
              to_math_type(form, compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_fpr(form, env));
        }
        return result;
      }

    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      ASSERT(false);
  }
  ASSERT(false);
  return get_none();
}

Val* Compiler::compile_floating_point_division(const goos::Object& form,
                                               const TypeSpec& result_type,
                                               RegVal* a,
                                               RegVal* b,
                                               Env* env,
                                               bool imm_divisor) {
  constexpr bool use_accurate = true;
  auto result = env->make_fpr(result_type);

  if (use_accurate && !imm_divisor) {
    auto fenv = env->function_env();
    auto end_label = fenv->alloc_unnamed_label();
    end_label->func = fenv;
    end_label->idx = -10;  // placeholder

    auto zero = compile_float(0.0, env, fenv->segment_for_static_data())->to_fpr(form, env);
    Condition zero_check;
    zero_check.kind = ConditionKind::EQUAL;
    zero_check.a = zero;
    zero_check.b = b;
    zero_check.is_float = true;

    // check for divide by zero
    auto branch_ir = std::make_unique<IR_ConditionalBranch>(zero_check, Label());
    auto branch_ir_ref = branch_ir.get();
    env->emit(form, std::move(branch_ir));

    // code for not dividing by zero
    env->emit_ir<IR_RegSet>(form, result, a);
    env->emit_ir<IR_FloatMath>(form, FloatMathKind::DIV_SS, result, b);

    env->emit_ir<IR_GotoLabel>(form, end_label);

    branch_ir_ref->mark_as_resolved();
    branch_ir_ref->label.idx = fenv->code().size();

    // code for dividing by zero
    auto flt_max = compile_integer(0x7f7fffff, env)->to_gpr(form, env);
    auto mask = compile_integer(0x80000000, env)->to_gpr(form, env);
    auto temp_int = env->make_gpr(result_type);
    env->emit_ir<IR_RegSet>(form, temp_int, a);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::AND_64, temp_int, mask);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::XOR_64, flt_max, temp_int);
    env->emit_ir<IR_RegSet>(form, temp_int, b);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::AND_64, temp_int, mask);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::XOR_64, flt_max, temp_int);
    env->emit_ir<IR_RegSet>(form, result, flt_max);

    end_label->idx = fenv->code().size();

  } else {
    env->emit_ir<IR_RegSet>(form, result, a);
    env->emit_ir<IR_FloatMath>(form, FloatMathKind::DIV_SS, result, b);
  }
  return result;
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
      auto fe = env->function_env();
      auto first_thing = first_val->to_gpr(form, env);
      auto result = env->make_gpr(first_type);
      env->emit_ir<IR_RegSet>(form, result, first_thing);

      auto val = compile_error_guard(args.unnamed.at(1), env);
      auto val_as_int = dynamic_cast<IntegerConstantVal*>(val);
      int power_of_two = -1;
      if (val_as_int && val_as_int->value().uses_gpr() && val_as_int->value().value_64() > 0) {
        auto p = get_power_of_two(val_as_int->value().value_64());
        if (p) {
          power_of_two = *p;
        }
      }

      if (power_of_two >= 0) {
        if (is_singed_integer_or_binteger(first_type)) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SAR_64, result, power_of_two);
        } else {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHR_64, result, power_of_two);
        }
      } else {
        IRegConstraint result_rax_constraint;
        result_rax_constraint.instr_idx = fe->code().size();
        result_rax_constraint.ireg = result->ireg();
        result_rax_constraint.desired_register = emitter::RAX;
        fe->constrain(result_rax_constraint);

        if (is_singed_integer_or_binteger(first_type)) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::IDIV_32, result,
                                       to_math_type(form, val, math_type, env)->to_gpr(form, env));
        } else {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::UDIV_32, result,
                                       to_math_type(form, val, math_type, env)->to_gpr(form, env));
        }

        auto result_moved = env->make_gpr(first_type);
        env->emit_ir<IR_RegSet>(form, result_moved, result);
        return result_moved;
      }

      return result;
    }

    case MATH_FLOAT: {
      const auto& divisor = args.unnamed.at(1);
      // in original GOAL, immediate divisions were turned into inverse multiplications
      if (divisor.is_float() && !divisor.is_float(0) && divisor.is_power_of_2_float()) {
        // TODO eventually this should be smarter somehow
        return compile_mul(
            form,
            pretty_print::build_list(
                {goos::Object::make_float(1.0 / divisor.as_float()), args.unnamed.at(0)}),
            env);
      } else {
        auto a = first_val->to_fpr(form, env);
        auto b = to_math_type(form, compile_error_guard(divisor, env), math_type, env)
                     ->to_fpr(form, env);
        return compile_floating_point_division(form, first_type, a, b, env,
                                               divisor.is_float() && !divisor.is_float(0));
      }
    }

    case MATH_INVALID:
      throw_compiler_error(form, "Cannot do math on a {}.", first_type.print());
      break;
    default:
      ASSERT(false);
  }
  ASSERT(false);
  return get_none();
}

Val* Compiler::compile_variable_shift(const goos::Object& form,
                                      const RegVal* in,
                                      const RegVal* sa,
                                      Env* env,
                                      IntegerMathKind kind) {
  auto result = env->make_gpr(in->type());
  auto sa_in = env->make_gpr(sa->type());

  env->emit_ir<IR_RegSet>(form, result, in);
  env->emit_ir<IR_RegSet>(form, sa_in, sa);
  auto fenv = env->function_env();

  IRegConstraint sa_con;
  sa_con.ireg = sa_in->ireg();
  sa_con.instr_idx = fenv->code().size();
  sa_con.desired_register = emitter::RCX;

  if (get_math_mode(in->type()) != MathMode::MATH_INT ||
      get_math_mode(sa->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot shift a {} by a {}", in->type().print(), sa->type().print());
  }

  fenv->constrain(sa_con);
  env->emit_ir<IR_IntegerMath>(form, kind, result, sa_in);
  return result;
}

Val* Compiler::compile_shl(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);

  auto sa = get_constant_integer_or_variable(args.unnamed.at(1), env);
  if (sa.is_constant()) {
    if (sa.constant < 0 || sa.constant > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, sa.constant, env, IntegerMathKind::SHL_64);
  } else {
    auto second = sa.val->to_gpr(form, env);
    return compile_variable_shift(form, first, second, env, IntegerMathKind::SHLV_64);
  }
}

Val* Compiler::compile_shr(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);

  auto sa = get_constant_integer_or_variable(args.unnamed.at(1), env);
  if (sa.is_constant()) {
    if (sa.constant < 0 || sa.constant > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, sa.constant, env, IntegerMathKind::SHR_64);
  } else {
    auto second = sa.val->to_gpr(form, env);
    return compile_variable_shift(form, first, second, env, IntegerMathKind::SHRV_64);
  }
}

Val* Compiler::compile_sar(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);

  auto sa = get_constant_integer_or_variable(args.unnamed.at(1), env);
  if (sa.is_constant()) {
    if (sa.constant < 0 || sa.constant > 64) {
      throw_compiler_error(form, "Cannot shift by more than 64, or by a negative amount.");
    }
    return compile_fixed_shift(form, first, sa.constant, env, IntegerMathKind::SAR_64);
  } else {
    auto second = sa.val->to_gpr(form, env);
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
  env->emit_ir<IR_RegSet>(form, result, in);
  // do the shift
  env->emit_ir<IR_IntegerMath>(form, kind, result, sa);
  return result;
}

Val* Compiler::compile_mod(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(form, env);
  auto fenv = env->function_env();

  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot mod a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result, first);

  IRegConstraint con;
  con.ireg = result->ireg();
  con.instr_idx = fenv->code().size();
  con.desired_register = emitter::RAX;

  fenv->constrain(con);
  env->emit_ir<IR_IntegerMath>(form,
                               is_singed_integer_or_binteger(first->type())
                                   ? IntegerMathKind::IMOD_32
                                   : IntegerMathKind::UMOD_32,
                               result, second);

  auto result_moved = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result_moved, result);
  return result_moved;
}

Val* Compiler::compile_logand(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(form, env);
  auto math_1 = get_math_mode(first->type());
  auto math_2 = get_math_mode(second->type());
  if (!((math_1 == MathMode::MATH_INT && math_2 == MathMode::MATH_INT) ||
        (math_1 == MathMode::MATH_INT && m_ts.tc(TypeSpec("pointer"), second->type())) ||
        (m_ts.tc(TypeSpec("pointer"), first->type()) && math_2 == MathMode::MATH_INT))) {
    throw_compiler_error(form, "Cannot logand a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  // kind of a hack, but make (logand int pointer) return pointer.
  auto result =
      env->make_gpr(m_ts.tc(TypeSpec("pointer"), second->type()) ? second->type() : first->type());
  env->emit_ir<IR_RegSet>(form, result, first);
  env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::AND_64, result, second);
  return result;
}

Val* Compiler::compile_logior(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compiler_error(form, "Invalid logior form");
  }

  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);

  auto result = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result, first);

  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto sec = compile_error_guard(args.unnamed.at(i), env);
    if (!is_integer(sec->type())) {
      throw_compiler_error(form, "Cannot logior a {} by a {}.", first->type().print(),
                           sec->type().print());
    }
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::OR_64, result, sec->to_gpr(form, env));
  }
  return result;
}

Val* Compiler::compile_logxor(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(form, env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot logxor a {} by a {}.", first->type().print(),
                         second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result, first);
  env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::XOR_64, result, second);
  return result;
}

Val* Compiler::compile_lognot(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT) {
    throw_compiler_error(form, "Cannot lognot a {}.", first->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result, first);
  env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::NOT_64, result, nullptr);
  return result;
}
