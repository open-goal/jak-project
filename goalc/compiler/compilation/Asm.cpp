#include "goalc/compiler/Compiler.h"

namespace {
const char* reg_names[] = {
    "rax",  "rcx",  "rdx",  "rbx",  "rsp",   "rbp",   "rsi",   "rdi",   "r8",    "r9",    "r10",
    "r11",  "r12",  "r13",  "r14",  "r15",   "xmm0",  "xmm1",  "xmm2",  "xmm3",  "xmm4",  "xmm5",
    "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};

}

emitter::Register Compiler::parse_register(const goos::Object& code) {
  if (!code.is_symbol()) {
    throw_compiler_error(code, "Could not parse {} as a register name", code.print());
  }

  auto nas = code.as_symbol();
  for (int i = 0; i < 32; i++) {
    if (nas->name == reg_names[i]) {
      return emitter::Register(i);
    }
  }

  throw_compiler_error(code, "Could not parse {} as a register name", code.print());
  return {};
}

Val* Compiler::compile_rlet(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (args.unnamed.size() < 1 || !args.named.empty()) {
    throw_compiler_error(form, "Must have an rlet body.");
  }

  auto defs = args.unnamed.front();

  auto fenv = get_parent_env_of_type<FunctionEnv>(env);
  auto lenv = fenv->alloc_env<LexicalEnv>(env);

  std::vector<IRegConstraint> constraints;
  std::vector<RegVal*> reset_regs;

  for_each_in_list(defs, [&](const goos::Object& o) {
    // (new-place [:reg old-place] [:type type-spec] [:class reg-type] [:bind #f|lexical|lambda])
    auto def_args = get_va(o, o);
    va_check(o, def_args, {goos::ObjectType::SYMBOL},
             {{"reg", {false, goos::ObjectType::SYMBOL}},
              {"type", {false, {}}},
              {"reset-here", {false, {}}},
              {"class", {false, goos::ObjectType::SYMBOL}}});

    // get the name of the new place
    auto new_place_name = def_args.unnamed.at(0);

    // get the type of the new place
    TypeSpec ts = m_ts.make_typespec("object");
    if (def_args.has_named("type")) {
      ts = parse_typespec(def_args.named.at("type"));
    }

    // figure out the class
    RegClass register_class = RegClass::GPR_64;
    if (def_args.has_named("class")) {
      auto& class_name = def_args.named.at("class").as_symbol()->name;
      if (class_name == "gpr") {
        register_class = RegClass::GPR_64;
      } else if (class_name == "fpr") {
        register_class = RegClass::FLOAT;
      } else if (class_name == "vf") {
        register_class = RegClass::VECTOR_FLOAT;
      } else {
        throw_compiler_error(o, "Register class {} is unknown.", class_name);
      }
    }

    // alloc a register:
    auto new_place_reg = env->make_ireg(ts, register_class);
    new_place_reg->mark_as_settable();

    if (def_args.has_named("reg")) {
      IRegConstraint constraint;
      constraint.ireg = new_place_reg->ireg();
      constraint.contrain_everywhere = true;
      constraint.desired_register = parse_register(def_args.named.at("reg"));
      if (def_args.has_named("reset-here") &&
          get_true_or_false(form, def_args.get_named("reset-here"))) {
        reset_regs.push_back(new_place_reg);
      }

      new_place_reg->set_rlet_constraint(constraint.desired_register);
      constraints.push_back(constraint);
    }

    lenv->vars[new_place_name.as_symbol()->name] = new_place_reg;
  });

  if (!reset_regs.empty()) {
    lenv->emit_ir<IR_ValueReset>(reset_regs);
  }

  Val* result = get_none();
  for (u64 i = 1; i < args.unnamed.size(); i++) {
    auto& o = args.unnamed.at(i);
    result = compile_error_guard(o, lenv);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(lenv);
    }
  }

  for (auto c : constraints) {
    fenv->constrain(c);
  }

  return result;
}

Val* Compiler::compile_asm_ret(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  env->emit_ir<IR_AsmRet>(color);
  return get_none();
}

Val* Compiler::compile_asm_pop(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }
  auto pop_dest = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (!pop_dest->settable()) {
    throw_compiler_error(form, "Cannot pop into this destination. Got a {}.", pop_dest->print());
  }
  env->emit_ir<IR_AsmPop>(color, pop_dest);
  return get_none();
}

Val* Compiler::compile_asm_push(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }
  env->emit_ir<IR_AsmPush>(color, compile_error_guard(args.unnamed.at(0), env)->to_gpr(env));
  return get_none();
}

Val* Compiler::compile_asm_sub(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }
  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (!dest->settable()) {
    throw_compiler_error(form, "Cannot .sub this. Got a {}.", dest->print());
  }
  auto src = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  env->emit_ir<IR_AsmSub>(color, dest, src);
  return get_none();
}

Val* Compiler::compile_asm_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }
  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (!dest->settable()) {
    throw_compiler_error(form, "Cannot .add this. Got a {}.", dest->print());
  }
  auto src = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  env->emit_ir<IR_AsmAdd>(color, dest, src);
  return get_none();
}

Val* Compiler::compile_asm_load_sym(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(
      form, args, {{}, {goos::ObjectType::SYMBOL}},
      {{"sext", {false, goos::ObjectType::SYMBOL}}, {"color", {false, goos::ObjectType::SYMBOL}}});
  auto& sym_name = args.unnamed.at(1).as_symbol()->name;
  auto sym_kv = m_symbol_types.find(sym_name);
  if (sym_kv == m_symbol_types.end()) {
    throw_compiler_error(form, "Cannot find a symbol named {}.", sym_name);
  }
  auto ts = sym_kv->second;
  bool sext = m_ts.lookup_type(ts)->get_load_signed();
  if (args.has_named("sext")) {
    sext = get_true_or_false(form, args.named.at("sext"));
  }

  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (!dest->settable()) {
    throw_compiler_error(form, "Cannot .load-sym this. Got a {}.", dest->print());
  }

  env->emit_ir<IR_GetSymbolValueAsm>(color, dest, sym_name, sext);
  return get_none();
}

Val* Compiler::compile_asm_jr(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto src = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  env->emit_ir<IR_JumpReg>(color, src);
  return get_none();
}

Val* Compiler::compile_asm_mov(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }
  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_reg(env);
  if (!dest->settable()) {
    throw_compiler_error(form, "Cannot .mov this. Got a {}.", dest->print());
  }
  auto src = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);
  env->emit_ir<IR_RegSetAsm>(color, dest, src);
  return get_none();
}

/*!
 * Load a vector float from memory. Does an aligned load.
 */
Val* Compiler::compile_asm_lvf(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_reg(env);
  if (!dest->settable() || dest->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(form, "Cannot .lvf into this. Got a {}.", dest->print());
  }
  auto src = compile_error_guard(args.unnamed.at(1), env);
  auto as_co = dynamic_cast<MemoryOffsetConstantVal*>(src);
  auto as_sv = dynamic_cast<StaticVal*>(src);
  MemLoadInfo info;
  info.sign_extend = false;
  info.size = 16;
  info.reg = RegClass::VECTOR_FLOAT;
  if (as_co) {
    // can do a clever offset here
    assert(false);
    env->emit_ir<IR_LoadConstOffset>(dest, as_co->offset, as_co->base->to_gpr(env), info, color);
  } else if (as_sv) {
    if (!color) {
      throw std::runtime_error("no color nyi for static loads");
    }
    env->emit_ir<IR_StaticVarLoad>(dest, as_sv->obj);
  } else {
    env->emit_ir<IR_LoadConstOffset>(dest, 0, src->to_gpr(env), info, color);
  }
  return get_none();
}

/*!
 * Store a vector float into memory. Does an aligned load.
 */
Val* Compiler::compile_asm_svf(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto dest = compile_error_guard(args.unnamed.at(0), env);
  auto src = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);

  if (!src->settable() || src->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(form, "Cannot .svf from this. Got a {}.", dest->print());
  }

  auto as_co = dynamic_cast<MemoryOffsetConstantVal*>(dest);
  MemLoadInfo info;
  info.sign_extend = false;
  info.size = 16;
  info.reg = RegClass::VECTOR_FLOAT;
  if (as_co) {
    // can do a clever offset here
    assert(false);
    env->emit_ir<IR_StoreConstOffset>(src, as_co->offset, as_co->base->to_gpr(env), 16, color);
  } else {
    env->emit_ir<IR_StoreConstOffset>(src, 0, dest->to_gpr(env), 16, color);
  }
  return get_none();
}

Val* Compiler::compile_asm_xor_vf(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_asm_vf_math3(form, rest, IR_VFMath3Asm::Kind::XOR, env);
}

Val* Compiler::compile_asm_sub_vf(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_asm_vf_math3(form, rest, IR_VFMath3Asm::Kind::SUB, env);
}

Val* Compiler::compile_asm_add_vf(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_asm_vf_math3(form, rest, IR_VFMath3Asm::Kind::ADD, env);
}

Val* Compiler::compile_asm_blend_vf(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}, {}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_reg(env);
  if (!dest->settable() || dest->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid destination register for a vector float 3-arg math form. Got a {}.",
        dest->print());
  }

  auto src1 = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);
  if (src1->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid first source register for a vector float 3-arg math form. Got a {}.",
        src1->print());
  }

  auto src2 = compile_error_guard(args.unnamed.at(2), env)->to_reg(env);
  if (src2->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid second source register for a vector float 3-arg math form. Got a {}.",
        src2->print());
  }

  int64_t mask;
  if (!try_getting_constant_integer(args.unnamed.at(3), &mask, env)) {
    throw_compiler_error(form,
                         "The value {} is invalid for a blend mask, it could not be evaluated as a "
                         "constant integer.",
                         args.unnamed.at(3).print());
  }

  if (mask < 0 || mask > 15) {
    throw_compiler_error(form, "The value {} is out of range for a blend mask.", mask);
  }
  env->emit_ir<IR_BlendVF>(color, dest, src1, src2, mask);
  return get_none();
}

Val* Compiler::compile_asm_vf_math3(const goos::Object& form,
                                    const goos::Object& rest,
                                    IR_VFMath3Asm::Kind kind,
                                    Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}, {}}, {{"color", {false, goos::ObjectType::SYMBOL}}});
  bool color = true;
  if (args.has_named("color")) {
    color = get_true_or_false(form, args.named.at("color"));
  }

  auto dest = compile_error_guard(args.unnamed.at(0), env)->to_reg(env);
  if (!dest->settable() || dest->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid destination register for a vector float 3-arg math form. Got a {}.",
        dest->print());
  }

  auto src1 = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);
  if (src1->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid first source register for a vector float 3-arg math form. Got a {}.",
        src1->print());
  }

  auto src2 = compile_error_guard(args.unnamed.at(2), env)->to_reg(env);
  if (src2->ireg().reg_class != RegClass::VECTOR_FLOAT) {
    throw_compiler_error(
        form, "Invalid second source register for a vector float 3-arg math form. Got a {}.",
        src2->print());
  }

  env->emit_ir<IR_VFMath3Asm>(color, dest, src1, src2, kind);

  return get_none();
}