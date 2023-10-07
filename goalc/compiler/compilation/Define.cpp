/*!
 * @file Define.cpp
 * Forms which define or set things.
 */

#include "goalc/compiler/Compiler.h"

/*!
 * Define or set a global value. Has some special magic to store data for functions which may be
 * inlined.
 */
Val* Compiler::compile_define(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  SymbolInfo::Metadata sym_meta;
  // Grab the docstring (if it's there) and then rip it out so we can do the normal validation
  if (args.unnamed.size() == 3 && args.unnamed.at(1).is_string()) {
    sym_meta.docstring = args.unnamed.at(1).as_string()->data;
    args.unnamed.erase(args.unnamed.begin() + 1);
  }

  va_check(form, args, {goos::ObjectType::SYMBOL, {}},
           {{"no-typecheck", {false, goos::ObjectType::SYMBOL}}});
  auto& sym = args.unnamed.at(0);
  auto& val = args.unnamed.at(1);

  // check we aren't duplicated a name as both a symbol and global constant
  auto global_constant = m_global_constants.find(sym.as_symbol());
  if (global_constant != m_global_constants.end()) {
    throw_compiler_error(
        form, "Cannot define a symbol named {}, it already exists as a global constant (value {}).",
        sym.print(), global_constant->second.print());
  }

  auto fe = env->function_env();
  auto sym_val = fe->alloc_val<SymbolVal>(symbol_string(sym), m_ts.make_typespec("symbol"));
  auto compiled_val = compile_error_guard(val, env);
  auto as_lambda = dynamic_cast<LambdaVal*>(compiled_val);
  if (as_lambda) {
    // there are two cases in which we save a function body that is passed to a define:
    // 1. It generated code [so went through the compiler] and the allow_inline flag is set.
    // 2. It didn't generate code [so explicitly with :inline-only lambdas]
    // The third case - immediate lambdas - don't get passed to a define,
    //   so this won't cause those to live for longer than they should
    if ((as_lambda->func && as_lambda->func->settings.allow_inline) || !as_lambda->func) {
      auto& f = m_inlineable_functions[sym.as_symbol()];
      // default inline if we have to (because no code), or if that's the option.
      f.inline_by_default = (!as_lambda->func) || as_lambda->func->settings.inline_by_default;
      f.lambda = as_lambda->lambda;
      f.type = as_lambda->type();
    }
    // Most defines come via macro invokations, we want the TRUE defining form location
    // if we can get it
    if (env->macro_expand_env()) {
      m_symbol_info.add_function(symbol_string(sym), as_lambda->lambda.params,
                                 env->macro_expand_env()->root_form(), sym_meta);
    } else {
      m_symbol_info.add_function(symbol_string(sym), as_lambda->lambda.params, form, sym_meta);
    }
  }

  if (!sym_val->settable()) {
    throw_compiler_error(form, "Cannot define {} because it cannot be set.", sym_val->print());
  }

  auto in_gpr = compiled_val->to_gpr(form, fe);
  auto existing_type = m_symbol_types.find(sym.as_symbol());
  if (existing_type == m_symbol_types.end()) {
    m_symbol_types[sym.as_symbol()] = in_gpr->type();
  } else {
    bool do_typecheck = true;
    if (args.has_named("no-typecheck")) {
      do_typecheck = !get_true_or_false(form, args.named.at("no-typecheck"));
    }
    if (do_typecheck) {
      typecheck(form, existing_type->second, in_gpr->type(),
                fmt::format("define on existing symbol {}", sym.as_symbol().name_ptr));
    }
  }

  if (!as_lambda) {
    // Don't double-add functions as globals
    m_symbol_info.add_global(symbol_string(sym), form, sym_meta);
  }

  env->emit(form, std::make_unique<IR_SetSymbolValue>(sym_val, in_gpr));
  return in_gpr;
}

/*!
 * Inform the compiler of the type of a global. Will warn on changing type.
 */
Val* Compiler::compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  // Grab the docstring (if it's there) and then rip it out so we can do the normal validation
  if (args.unnamed.size() == 3 && args.unnamed.at(1).is_string()) {
    // TODO - docstring - actually use it!
    // std::string docstring = args.unnamed.at(1).as_string()->data;
    args.unnamed.erase(args.unnamed.begin() + 1);
  }
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& typespec = args.unnamed.at(1);

  auto new_type = parse_typespec(typespec, env);

  auto existing_type = m_symbol_types.find(sym.as_symbol());
  // symbol already declared, and doesn't match existing definition. do more checks...
  if (existing_type != m_symbol_types.end() && existing_type->second != new_type) {
    if (m_allow_inconsistent_definition_symbols.find(symbol_string(sym)) ==
        m_allow_inconsistent_definition_symbols.end()) {
      // throw if we have throws enabled, and new definition is NOT just more generic
      // (that case is fine in goal)
      if (!m_ts.tc(new_type, existing_type->second) && m_throw_on_define_extern_redefinition) {
        throw_compiler_error(form,
                             "define-extern would redefine the type of symbol {} from {} to {}.",
                             symbol_string(sym), existing_type->second.print(), new_type.print());
      } else {
        print_compiler_warning(
            "define-extern has redefined the type of symbol {}\npreviously: {}\nnow: {}\n",
            symbol_string(sym).c_str(), existing_type->second.print().c_str(),
            new_type.print().c_str());
      }
    }
  }

  m_symbol_types[sym.as_symbol()] = new_type;
  m_symbol_info.add_fwd_dec(symbol_string(sym), form);
  return get_none();
}

/*!
 * Modify dst by setting the bitfield with give size/offset to the value in src.
 */
void Compiler::set_bits_in_bitfield(const goos::Object& form,
                                    int size,
                                    int offset,
                                    RegVal* dst,
                                    RegVal* src,
                                    FunctionEnv* fe,
                                    Env* env) {
  // we'll need a temp register to hold a mask:
  auto temp = fe->make_gpr(src->type());
  // mask value should be 1's everywhere except for the field so we can AND with it
  u64 mask_val = ~((((u64)1 << (u64)size) - (u64)1) << (u64)offset);
  env->emit_ir<IR_LoadConstant64>(form, temp, mask_val);
  // modify the original!
  env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::AND_64, dst, temp);

  // put the source in temp
  env->emit_ir<IR_RegSet>(form, temp, src);

  // to shift us all the way to the left and clear upper bits
  int left_shift_amnt = 64 - size;
  int right_shift_amnt = (64 - size) - offset;
  ASSERT(right_shift_amnt >= 0);

  if (left_shift_amnt > 0) {
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, temp, left_shift_amnt);
  }

  if (right_shift_amnt > 0) {
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHR_64, temp, right_shift_amnt);
  }

  env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::OR_64, dst, temp);
}

void Compiler::set_bitfield(const goos::Object& form, BitFieldVal* dst, RegVal* src, Env* env) {
  if (dst->use_128_bit()) {
    set_bitfield_128(form, dst, src, env);
    return;
  }

  auto fe = env->function_env();

  // first, get the value we want to modify:
  auto original_original = dst->parent()->to_gpr(form, env);
  // let's not directly modify original, and instead create a copy then use do_set on parent.
  // this way we avoid "cheating" the set system, although it should be safe...
  auto original = fe->make_gpr(original_original->type());
  env->emit_ir<IR_RegSet>(form, original, original_original);
  set_bits_in_bitfield(form, dst->size(), dst->offset(), original, src, fe, env);

  do_set(form, dst->parent(), original, original, env);
}

void Compiler::set_bitfield_128(const goos::Object& form, BitFieldVal* dst, RegVal* src, Env* env) {
  auto fe = env->function_env();

  bool get_top = dst->offset() >= 64;

  // first, get the value we want to modify:
  ASSERT(m_ts.lookup_type(dst->parent()->type())->get_preferred_reg_class() == RegClass::INT_128);
  RegVal* original_original = dst->parent()->to_xmm128(form, env);

  // next, get the 64-bit part we want to modify in the lower 64 bits of an XMM
  RegVal* xmm_temp = fe->make_ireg(original_original->type(), RegClass::INT_128);
  if (get_top) {
    env->emit_ir<IR_Int128Math3Asm>(form, true, xmm_temp, original_original, original_original,
                                    IR_Int128Math3Asm::Kind::PCPYUD);
  } else {
    env->emit_ir<IR_RegSet>(form, xmm_temp, original_original);
  }

  // convert that xmm to a GPR.
  RegVal* gpr_64_section = fe->make_gpr(original_original->type());
  env->emit_ir<IR_RegSet>(form, gpr_64_section, xmm_temp);

  // set the bits in the GPR
  int corrected_offset = get_top ? dst->offset() - 64 : dst->offset();
  set_bits_in_bitfield(form, dst->size(), corrected_offset, gpr_64_section, src, fe, env);

  // back to xmm
  env->emit_ir<IR_RegSet>(form, xmm_temp, gpr_64_section);

  // rebuild the xmm
  if (get_top) {
    env->emit_ir<IR_Int128Math3Asm>(form, true, xmm_temp, xmm_temp, original_original,
                                    IR_Int128Math3Asm::Kind::PCPYLD);
  } else {
    env->emit_ir<IR_Int128Math3Asm>(form, true, xmm_temp, xmm_temp, xmm_temp,
                                    IR_Int128Math3Asm::Kind::PCPYLD);
    env->emit_ir<IR_Int128Math3Asm>(form, true, xmm_temp, xmm_temp, original_original,
                                    IR_Int128Math3Asm::Kind::PCPYUD);
  }

  // set
  do_set(form, dst->parent(), xmm_temp, xmm_temp, env);
}

/*!
 * The internal "set" logic.
 * The source is provided both as the directly Val* from compilation and as a RegVal*.
 * This is a bit weird, but is required to do things in exactly the same order as GOAL, but
 * makes us able to check if the source is #f, which is allowed to bypass type checking in some
 * cases. If the source is unavailable, you can just put the same thing for src_in_reg and src, but
 * you'll lose the ability to detect and accept #f as a null reference.
 */
Val* Compiler::do_set(const goos::Object& form, Val* dest, RegVal* src_in_reg, Val* src, Env* env) {
  if (!dest->settable()) {
    throw_compiler_error(form, "Cannot set! {} because it is not settable.", dest->print());
  }
  auto as_mem_deref = dynamic_cast<MemoryDerefVal*>(dest);
  auto as_pair = dynamic_cast<PairEntryVal*>(dest);
  auto as_reg = dynamic_cast<RegVal*>(dest);
  auto as_sym_val = dynamic_cast<SymbolValueVal*>(dest);
  auto as_bitfield = dynamic_cast<BitFieldVal*>(dest);

  if (as_mem_deref) {
    auto dest_type = coerce_to_reg_type(as_mem_deref->type());
    if (dest_type != TypeSpec("uint") || coerce_to_reg_type(src->type()) != TypeSpec("int")) {
      typecheck_reg_type_allow_false(form, dest_type, src, "set! memory");
    }

    // we want to allow setting a 128-bit field from a 64-bit variable.
    if (as_mem_deref->info.size == 16) {
      auto fe = env->function_env();
      auto src_128 = fe->make_ireg(src_in_reg->type(), RegClass::INT_128);
      env->emit_ir<IR_RegSet>(form, src_128, src_in_reg);
      src_in_reg = src_128;
    }

    // we want to allow setting a smaller thing from a 128-bit variable
    if (as_mem_deref->info.size != 16 && (src_in_reg->ireg().reg_class == RegClass::VECTOR_FLOAT ||
                                          src_in_reg->ireg().reg_class == RegClass::INT_128)) {
      auto fe = env->function_env();
      auto src_gpr = fe->make_ireg(src_in_reg->type(), RegClass::GPR_64);
      env->emit_ir<IR_RegSet>(form, src_gpr, src_in_reg);
      src_in_reg = src_gpr;
    }

    // we want to allow setting a 64-bit place to a float
    if (as_mem_deref->info.size == 8 && src_in_reg->ireg().reg_class == RegClass::FLOAT) {
      auto fe = env->function_env();
      auto src_gpr = fe->make_ireg(src_in_reg->type(), RegClass::GPR_64);
      env->emit_ir<IR_RegSet>(form, src_gpr, src_in_reg);
      src_in_reg = src_gpr;
    }

    // setting somewhere in memory
    auto base = as_mem_deref->base;
    auto base_as_mco = dynamic_cast<MemoryOffsetConstantVal*>(base);
    int load_size = m_ts.get_load_size_allow_partial_def(as_mem_deref->type());
    if (base_as_mco) {
      // if it is a constant offset, we can use a fancy x86-64 addressing mode to simplify
      env->emit_ir<IR_StoreConstOffset>(form, src_in_reg, base_as_mco->offset,
                                        base_as_mco->base->to_gpr(form, env), load_size);
      return src_in_reg;
    } else {
      // nope, the pointer to dereference is some complicated thing.
      env->emit_ir<IR_StoreConstOffset>(form, src_in_reg, 0, base->to_gpr(form, env), load_size);
      return src_in_reg;
    }
  } else if (as_pair) {
    // this could probably be part of MemoryDerefVal and not a special case here.
    env->emit_ir<IR_StoreConstOffset>(form, src_in_reg, as_pair->is_car ? -2 : 2,
                                      as_pair->base->to_gpr(form, env), 4);
    return src_in_reg;
  } else if (as_reg) {
    typecheck_reg_type_allow_false(form, as_reg->type(), src, "set! lexical variable");
    env->emit_ir<IR_RegSet>(form, as_reg, src_in_reg);
    return src_in_reg;
  } else if (as_sym_val) {
    typecheck_reg_type_allow_false(form, as_sym_val->type(), src, "set! global symbol");
    auto result_in_gpr = src_in_reg->to_gpr(form, env);
    env->emit_ir<IR_SetSymbolValue>(form, as_sym_val->sym(), result_in_gpr);
    return result_in_gpr;
  } else if (as_bitfield) {
    set_bitfield(form, as_bitfield, src_in_reg, env);
    return get_none();
  }

  throw_compiler_error(form, "There is no implementation of set! for {}.", dest->print());
  return get_none();
}

/*!
 * Set something to something.
 * Lots of special cases.
 */
Val* Compiler::compile_set(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});

  auto& destination = args.unnamed.at(0);
  // this is the order I'm using in the decompiler and it seems to be right.
  // see StorePlainDeref::push_to_stack for example
  auto source = compile_error_guard(args.unnamed.at(1), env);
  auto source_reg = source->to_reg(form, env);
  auto dest = compile_error_guard(destination, env);
  return do_set(form, dest, source_reg, source, env);
}
