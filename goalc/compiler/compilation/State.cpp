#include "goalc/compiler/Compiler.h"

Val* Compiler::compile_define_state_hook(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env) {
  auto args = get_va(form, rest);
  // args:
  //  state_name
  //  state_parent
  //  state object
  //  named args for enter/exit/trans/post/event/code
  va_check(form, args, {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL, {}},
           {
               {"enter", {true, {}}},
               {"exit", {true, {}}},
               {"trans", {true, {}}},
               {"post", {true, {}}},
               {"event", {true, {}}},
               {"code", {true, {}}},
           });

  // check parent
  auto state_parent = args.unnamed.at(1).as_symbol()->name;
  auto state_parent_type = m_ts.make_typespec(state_parent);
  if (!m_ts.tc(TypeSpec("process"), state_parent_type)) {
    throw_compiler_error(form, "define-state got a type {} which is not a child of process",
                         state_parent_type.print());
  }

  // get state object
  auto state_object = compile_error_guard(args.unnamed.at(2), env)->to_gpr(env);
  if (state_object->type() != TypeSpec("state")) {
    throw_compiler_error(form, "define-state-hook got an invalid state object: had type {}",
                         state_object->type().print());
  }

  auto state_type_info = m_ts.get_type_of_type<StructureType>("state");

  // set the easy ones
  for (auto name : {"exit", "trans", "post", "event"}) {
    auto field = get_field_of_structure(state_type_info, state_object, name, env);
    auto value = compile_error_guard(args.named.at(name), env);
    do_set(form, field, value->to_gpr(env), value, env);
  }

  auto enter_field = get_field_of_structure(state_type_info, state_object, "enter", env);
  auto enter_value = compile_error_guard(args.named.at("enter"), env);
  do_set(form, enter_field, enter_value->to_gpr(env), enter_value, env);

  auto code_field = get_field_of_structure(state_type_info, state_object, "code", env);
  auto code_value = compile_error_guard(args.named.at("code"), env);
  do_set(form, code_field, code_value->to_gpr(env), code_value, env);

  // state name
  TypeSpec state_type("state");  // todo

  for (int i = 0; i < (int)code_value->type().arg_count() - 1; i++) {
    state_type.add_arg(code_value->type().get_arg(i));
  }
  state_type.add_arg(state_parent_type);
  auto state_name = args.unnamed.at(0).as_symbol()->name;
  auto existing_var = m_symbol_types.find(state_name);
  if (existing_var != m_symbol_types.end() && existing_var->second != state_type) {
    throw_compiler_error(form, "define-state would redefine the type of symbol {} from {} to {}",
                         state_name, existing_var->second.print(), state_type.print());
  }
  m_symbol_types[state_name] = state_type;
  auto sym_val =
      get_parent_env_of_type<FunctionEnv>(env)->alloc_val<SymbolVal>(state_name, state_type);
  env->emit(std::make_unique<IR_SetSymbolValue>(sym_val, state_object));

  return get_none();
}