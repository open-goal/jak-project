#include "goalc/compiler/Compiler.h"
#include "goalc/logger/Logger.h"

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

  fe->emit(std::make_unique<IR_SetSymbolValue>(sym_val, in_gpr));
  return in_gpr;
}

Val* Compiler::compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& typespec = args.unnamed.at(1);

  auto new_type = parse_typespec(typespec);

  auto existing_type = m_symbol_types.find(symbol_string(sym));
  if (existing_type != m_symbol_types.end() && existing_type->second != new_type) {
    gLogger.log(
        MSG_WARN,
        "[Warning] define-extern has redefined the type of symbol %s\npreviously: %s\nnow: %s\n",
        symbol_string(sym).c_str(), existing_type->second.print().c_str(),
        new_type.print().c_str());
  }
  m_symbol_types[symbol_string(sym)] = new_type;
  return get_none();
}

Val* Compiler::compile_set(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});

  auto& destination = args.unnamed.at(0);
  auto source = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);

  if (destination.is_symbol()) {
    // destination is just a symbol, so it's either a lexical variable or a global.

    // first, attempt a lexical set:
    auto lex_place = env->lexical_lookup(destination);
    if (lex_place) {
      // typecheck and set!
      typecheck(form, lex_place->type(), source->type(), "set! lexical variable");
      env->emit(std::make_unique<IR_RegSet>(lex_place, source));
      return source;
    } else {
      // try to set symbol
      auto existing = m_symbol_types.find(destination.as_symbol()->name);
      if (existing == m_symbol_types.end()) {
        throw_compile_error(
            form, "could not find something called " + symbol_string(destination) + " to set!");
      } else {
        typecheck(form, existing->second, source->type(), "set! global symbol");
        auto fe = get_parent_env_of_type<FunctionEnv>(env);
        auto sym_val =
            fe->alloc_val<SymbolVal>(symbol_string(destination), m_ts.make_typespec("symbol"));
        auto result_in_gpr = source->to_gpr(env);
        env->emit(std::make_unique<IR_SetSymbolValue>(sym_val, result_in_gpr));
        return result_in_gpr;
      }
    }
  } else {
    throw_compile_error(form, "Set not implemented for this yet");
  }
  throw std::runtime_error("Unexpected error in Set");
}