/*!
 * @file GoalDefineForms.cpp
 * GOAL Compiler forms related to defining and setting things.
 */

#include <logger/Logger.h>
#include "Goal.h"
#include "util.h"

/*!
 * Set a global place
 */
std::shared_ptr<Place> Goal::compile_define(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 3);
  if (!args.check_count(2) || !args.named_args.empty()) {
    throw_compile_error(form, "invalid define form");
  }

  auto& sym = args.unnamed_args.at(0);
  auto& value = args.unnamed_args.at(1);

  if (sym.type != SYMBOL) {
    throw_compile_error(form, "define must act on a symbol");
  }

  auto global_constant = global_constants.find(sym.as_symbol());
  if (global_constant != global_constants.end()) {
    throw_compile_error(
        form, "it is illegal to define a GOAL symbol with the same name as a GOAL global constant");
  }

  auto ir = make_unique<IR_SetSymbolValue>();
  auto compiled_value = compile_error_guard(value, env);

  // special case to handle defining a function so that it can be inline later on.
  auto as_lambda_place = std::dynamic_pointer_cast<LambdaPlace>(compiled_value);
  if (as_lambda_place) {
    // there are two cases in which we save a function body that is passed to a define:
    // 1. It generated code [so went through the compiler] and the allow_inline flag is set.
    // 2. It didn't generate code [so explicitly with :inline-only lambdas]
    // The third case - immediate lambdas - don't get passed to a define,
    //   so this won't cause those to live for longer than they should
    if ((as_lambda_place->func && as_lambda_place->func->settings.allow_inline) ||
        !as_lambda_place->func) {
      inlineable_functions[sym.as_symbol()] = as_lambda_place;
    }
  }

  ir->value = resolve_to_gpr(compiled_value, env);

  // typecheck, or define the type of the symbol.
  auto existing_symbol_type = symbol_types.find(sym.as_symbol());
  if (existing_symbol_type == symbol_types.end()) {
    symbol_types[sym.as_symbol()] = ir->value->type;
  } else {
    typecheck_base_only(form, existing_symbol_type->second, ir->value->type,
                        "define on existing symbol");
  }

  TypeSpec symbol_type = get_base_typespec("symbol");
  ir->dest = std::make_shared<SymbolPlace>(sym.as_symbol()->name, symbol_type);

  auto result = ir->value;
  env->emit(std::move(ir));
  return result;
}

/*!
 * Set the type of a global symbol
 */
std::shared_ptr<Place> Goal::compile_define_extern(const Object& form,
                                                   Object rest,
                                                   std::shared_ptr<GoalEnv> env) {
  (void)env;
  auto args = goos.get_uneval_args(form, rest, 3);
  if (!args.check_count(2) || !args.named_args.empty()) {
    throw_compile_error(form, "invalid define-extern form");
  }

  auto& sym = args.unnamed_args.at(0);
  auto& typespec = args.unnamed_args.at(1);

  if (sym.type != SYMBOL) {
    throw_compile_error(form, "define-extern must act on a symbol");
  }

  // we should print a warning if we change the type.
  // it's probably fine if the user is doing this at the REPL, but pretty bad if this happens in
  // code.
  auto new_type = compile_typespec(typespec);

  auto existing_symbol_type = symbol_types.find(sym.as_symbol());
  if (existing_symbol_type != symbol_types.end() && existing_symbol_type->second != new_type) {
    gLogger.log(
        MSG_WARN,
        "[Warning] define-extern has redefined the type of symbol %s\npreviously: %s\nnow: %s\n",
        symbol_string(sym).c_str(), existing_symbol_type->second.print().c_str(),
        new_type.print().c_str());
  }

  symbol_types[sym.as_symbol()] = new_type;

  return get_none();
}

/*!
 * Set a global, lexical, or field
 */
std::shared_ptr<Place> Goal::compile_set(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args_no_rest(form, rest, 2);
  if (!args.check_count(2) || !args.named_args.empty()) {
    throw_compile_error(form, "invalid set! form");
  }

  auto& destination = args.unnamed_args.at(0);
  auto source = resolve_to_gpr_or_xmm(compile_error_guard(args.unnamed_args.at(1), env), env);

  if (destination.type == SYMBOL) {
    // destination is just a symbol, so it's either a lexical variable or a global.

    // first, attempt a lexical set:
    auto lex_place = env->lexical_lookup(destination);
    if (lex_place) {
      // typecheck and set!
      typecheck_base_only(form, lex_place->type, source->type, "set! lexical variable");
      env->emit(make_unique<IR_Set>(lex_place, source));
      return source;
    } else {
      // try to set symbol
      auto existing = symbol_types.find(destination.as_symbol());
      if (existing == symbol_types.end()) {
        throw_compile_error(
            form, "could not find something called " + symbol_string(destination) + " to set!");
      } else {
        typecheck_base_only(form, existing->second, source->type, "set! global symbol");
        auto ir = make_unique<IR_SetSymbolValue>();
        ir->value = resolve_to_gpr(source, env);
        ir->dest = std::make_shared<SymbolPlace>(destination.as_symbol()->name, existing->second);
        auto result = ir->value;
        env->emit(std::move(ir));
        return result;
      }
    }
  } else {
    // destination is something more complicated, like a (-> obj field) or a (car obj)

    auto dest = compile_error_guard(destination, env);
    auto dest_as_mem_deref = std::dynamic_pointer_cast<MemoryDerefPlace>(dest);
    auto dest_as_pair = std::dynamic_pointer_cast<PairPlace>(dest);
    auto dest_as_bitfield = std::dynamic_pointer_cast<BitfieldPlace>(dest);

    if (dest_as_mem_deref) {
      // special typecheck to handle the interger type weirdness (all registers are
      // integer/uinteger, all fields are sized integers)
      typecheck_for_set(form, dest_as_mem_deref->type, source->type, "set! mem deref");

      // get the address of the thing we're setting:
      auto base = dest_as_mem_deref->base;

      // if the pointer is a const offset from another pointer, we can do an optimization
      // this is useful to do someting like (set! (-> obj field) x), which field is a compile-time
      // known offset from obj and we can use fancy x86 addressing to do this in a single
      // instruction.
      auto base_as_mem_c_offset = std::dynamic_pointer_cast<MemoryOffsetConstPlace>(base);
      if (base_as_mem_c_offset) {
        auto ir = make_unique<IR_StoreConstOffset>(
            resolve_to_gpr(base_as_mem_c_offset->base, env), resolve_to_gpr(source, env),
            base_as_mem_c_offset->offset, dest_as_mem_deref->type.type->load_size,
            dest_as_mem_deref->type.type->load_signed);
        auto result = ir->val;
        env->emit(std::move(ir));
        return result;
      } else {
        // the offset is not constant, so we need to compute the address and put it in a gpr first.
        // this is done in the case of (set! (-> obj my-array x) y) where the x-th element of
        // my-array is not at a known address at compile time.
        auto addr = resolve_to_gpr(addr_of(dest_as_mem_deref, env), env);

        // then do a store with 0 offset.
        auto ir = make_unique<IR_StoreConstOffset>(addr, resolve_to_gpr(source, env), 0,
                                                   dest_as_mem_deref->type.type->load_size,
                                                   dest_as_mem_deref->type.type->load_signed);
        auto result = ir->val;
        env->emit(std::move(ir));
        return result;
      }

    } else if (dest_as_pair) {
      // store into the car/cdr of a pair.
      auto ptr = resolve_to_gpr(dest_as_pair->base, env);
      auto ir = make_unique<IR_StoreConstOffset>(ptr, resolve_to_gpr(source, env),
                                                 dest_as_pair->is_car ? -2 : 2, 4, false);
      auto result = ir->val;
      env->emit(std::move(ir));
      return result;
    } else if (dest_as_bitfield) {
      typecheck_for_set(form, dest_as_bitfield->type, source->type, "set! bitfield");
      return set_bitfield(dest_as_bitfield, source, env);
    }

    else {
      throw_compile_error(form, "unknown set! target: " + dest->print());
    }
  }

  throw_compile_error(form, "unreachable in compile_set");
  return get_none();
}

/*!
 * Declare a thing to be a function of the given type.
 */
std::shared_ptr<Place> Goal::compile_defun_extern(const Object& form,
                                                  Object rest,
                                                  std::shared_ptr<GoalEnv> env) {
  (void)env;
  auto args = goos.get_uneval_args_no_rest(form, rest, 3);
  if (!args.check_count(3) || !args.named_args.empty()) {
    throw_compile_error(form, "invalid defun-extern form");
  }

  auto function_name = args.unnamed_args.at(0);
  auto param_list = args.unnamed_args.at(1);
  auto return_type = args.unnamed_args.at(2);

  TypeSpec ts = get_base_typespec("function");
  ts.ts_args.emplace_back(return_type, types);

  for_each_in_list(param_list, [&](Object o) {
    if (o.type == SYMBOL) {
      ts.ts_args.push_back(get_base_typespec("object"));
    } else {
      auto param_args = goos.get_uneval_args(o, o, 3);
      if (param_args.unnamed_args.size() >= 3 || param_args.unnamed_args.size() < 1 ||
          param_args.has_rest || !param_args.named_args.empty()) {
        throw_compile_error(o, "invalid defun-extern parameter");
      }

      TypeSpec parm_type;

      if (param_args.unnamed_args.size() >= 2) {
        parm_type = compile_typespec(param_args.unnamed_args[1]);
      } else {
        parm_type = get_base_typespec("object");
      }

      ts.ts_args.push_back(parm_type);
    }
  });

  auto st_kv = symbol_types.find(function_name.as_symbol());
  if (st_kv == symbol_types.end()) {
    symbol_types[function_name.as_symbol()] = ts;
  } else {
    if (ts != st_kv->second) {
      // could be a warning?
      throw_compile_error(form, "defun-extern changes the type of " + function_name.print() +
                                    " from " + st_kv->second.print() + " to " + ts.print());
    }
    symbol_types[function_name.as_symbol()] = ts;
  }

  return get_none();
}

/*!
 * Declare a method. Use of this form is discouraged - when possible put method defs in the type
 * definition itself.
 */
std::shared_ptr<Place> Goal::compile_declare_method(const Object& form,
                                                    Object rest,
                                                    std::shared_ptr<GoalEnv> env) {
  (void)env;
  auto args = goos.get_uneval_args_no_rest(form, rest, 4);
  if (!args.check_count(4) || !args.named_args.empty()) {
    throw_compile_error(form, "invalid declare-method form");
  }

  auto type_name = args.unnamed_args.at(0);
  auto method_name = symbol_string(args.unnamed_args.at(1));
  auto param_list = args.unnamed_args.at(2);
  auto return_type = args.unnamed_args.at(3);

  TypeSpec ts = get_base_typespec("function");
  ts.ts_args.emplace_back(return_type, types);

  for_each_in_list(param_list, [&](Object o) {
    if (o.type == SYMBOL) {
      ts.ts_args.push_back(get_base_typespec("object"));
    } else {
      auto param_args = goos.get_uneval_args(o, o, 3);
      if (param_args.unnamed_args.size() >= 3 || param_args.unnamed_args.size() < 1 ||
          param_args.has_rest || !param_args.named_args.empty()) {
        throw_compile_error(o, "invalid declare-method parameter");
      }

      TypeSpec parm_type;

      if (param_args.unnamed_args.size() >= 2) {
        parm_type = TypeSpec(param_args.unnamed_args[1], types);  // todo improve
      } else {
        parm_type = get_base_typespec("object");
      }

      ts.ts_args.push_back(parm_type);
    }
  });

  MethodType existing;
  if (types.try_get_method_info(symbol_string(type_name), method_name, &existing)) {
    if (existing.type != ts) {
      gLogger.log(MSG_WARN,
                  "[Warning] declare-method changes the type of method %s %s\nold: %s\nnew: %s\n",
                  symbol_string(type_name).c_str(), method_name.c_str(),
                  existing.type.print().c_str(), ts.print().c_str());
    }
  }

  types.add_method(symbol_string(type_name), method_name, ts);
  return get_none();
}