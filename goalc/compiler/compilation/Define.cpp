/*!
 * @file Define.cpp
 * Forms which define or set things.
 */

#include "goalc/compiler/Compiler.h"
#include "goalc/logger/Logger.h"

/*!
 * Define or set a global value. Has some special magic to store data for functions which may be
 * inlined.
 */
Val* Compiler::compile_define(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& val = args.unnamed.at(1);

  // check we aren't duplicated a name as both a symbol and global constant
  auto global_constant = m_global_constants.find(sym.as_symbol());
  if (global_constant != m_global_constants.end()) {
    throw_compile_error(
        form, "it is illegal to define a GOAL symbol with the same name as a GOAL global constant");
  }

  auto fe = get_parent_env_of_type<FunctionEnv>(env);
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
      m_inlineable_functions[sym.as_symbol()] = as_lambda;
    }
  }

  auto in_gpr = compiled_val->to_gpr(fe);
  auto existing_type = m_symbol_types.find(sym.as_symbol()->name);
  if (existing_type == m_symbol_types.end()) {
    m_symbol_types[sym.as_symbol()->name] = in_gpr->type();
  } else {
    typecheck(form, existing_type->second, in_gpr->type(), "define on existing symbol");
  }

  if (!sym_val->settable()) {
    throw_compile_error(
        form, "Tried to use define on something that wasn't settable: " + sym_val->print());
  }
  fe->emit(std::make_unique<IR_SetSymbolValue>(sym_val, in_gpr));
  return in_gpr;
}

/*!
 * Inform the compiler of the type of a global. Will warn on changing type.
 */
Val* Compiler::compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& typespec = args.unnamed.at(1);

  auto new_type = parse_typespec(typespec);

  auto existing_type = m_symbol_types.find(symbol_string(sym));
  if (existing_type != m_symbol_types.end() && existing_type->second != new_type) {
    // todo spdlog
    gLogger.log(
        MSG_WARN,
        "[Warning] define-extern has redefined the type of symbol %s\npreviously: %s\nnow: %s\n",
        symbol_string(sym).c_str(), existing_type->second.print().c_str(),
        new_type.print().c_str());

    if (m_throw_on_define_extern_redefinition) {
      throw_compile_error(form, "define-extern redefinition");
    }
  }

  if (new_type == m_ts.make_typespec("type")) {
    m_ts.forward_declare_type(symbol_string(sym));
  }

  m_symbol_types[symbol_string(sym)] = new_type;
  return get_none();
}

/*!
 * The internal "set" logic.
 */
Val* Compiler::do_set(const goos::Object& form, Val* dest, RegVal* source, Env* env) {
  if (!dest->settable()) {
    throw_compile_error(form,
                        "Tried to use set! on something that wasn't settable: " + dest->print());
  }
  auto as_mem_deref = dynamic_cast<MemoryDerefVal*>(dest);
  auto as_pair = dynamic_cast<PairEntryVal*>(dest);
  auto as_reg = dynamic_cast<RegVal*>(dest);
  auto as_sym_val = dynamic_cast<SymbolValueVal*>(dest);

  if (as_mem_deref) {
    // setting somewhere in memory
    auto base = as_mem_deref->base;
    auto base_as_mco = dynamic_cast<MemoryOffsetConstantVal*>(base);
    if (base_as_mco) {
      // if it is a constant offset, we can use a fancy x86-64 addressing mode to simplify
      auto ti = m_ts.lookup_type(as_mem_deref->type());
      env->emit(std::make_unique<IR_StoreConstOffset>(
          source, base_as_mco->offset, base_as_mco->base->to_gpr(env), ti->get_load_size()));
      return source;
    } else {
      // nope, the pointer to dereference is some compliated thing.
      auto ti = m_ts.lookup_type(as_mem_deref->type());
      env->emit(
          std::make_unique<IR_StoreConstOffset>(source, 0, base->to_gpr(env), ti->get_load_size()));
      return source;
    }
  } else if (as_pair) {
    // this could probably be part of MemoryDerefVal and not a special case here.
    env->emit(std::make_unique<IR_StoreConstOffset>(source, as_pair->is_car ? -2 : 2,
                                                    as_pair->base->to_gpr(env), 4));
    return source;
  } else if (as_reg) {
    typecheck(form, as_reg->type(), source->type(), "set! lexical variable");
    env->emit(std::make_unique<IR_RegSet>(as_reg, source));
    return source;
  } else if (as_sym_val) {
    typecheck(form, as_sym_val->type(), source->type(), "set! global symbol");
    auto result_in_gpr = source->to_gpr(env);
    env->emit(std::make_unique<IR_SetSymbolValue>(as_sym_val->sym(), result_in_gpr));
    return result_in_gpr;
  }

  throw_compile_error(form, "Set not implemented for: " + dest->print());
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
  // todo, I don't know if this is the correct order or not. Right now the value is computed
  // and to_reg'd first, then the destination is computed, if the destination requires math to
  // compute.
  auto source = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);
  auto dest = compile_error_guard(destination, env);
  return do_set(form, dest, source, env);
}