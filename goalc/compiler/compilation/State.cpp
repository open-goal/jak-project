#include "goalc/compiler/Compiler.h"
#include "common/type_system/state.h"

/*!
 * The define-state-hook compiler form is used from a macro in gstate.gc.
 * Args:
 *  - state_name, a symbol of the state's name.
 *  - state_parent, a symbol with the type name. Must be child of process
 *  - state, the actual state object to initialize.
 */
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
  auto state_object = compile_error_guard(args.unnamed.at(2), env)->to_gpr(form, env);
  if (state_object->type() != TypeSpec("state")) {
    throw_compiler_error(form, "define-state-hook got an invalid state object: had type {}",
                         state_object->type().print());
  }

  auto state_type_info = m_ts.get_type_of_type<StructureType>("state");

  // set the easy ones
  for (auto name : {"exit", "trans", "post", "event"}) {
    auto field = get_field_of_structure(state_type_info, state_object, name, env);
    auto value = compile_error_guard(args.named.at(name), env);
    do_set(form, field, value->to_gpr(form, env), value, env);
  }

  auto enter_field = get_field_of_structure(state_type_info, state_object, "enter", env);
  auto enter_value = compile_error_guard(args.named.at("enter"), env);
  do_set(form, enter_field, enter_value->to_gpr(form, env), enter_value, env);

  auto code_field = get_field_of_structure(state_type_info, state_object, "code", env);
  auto code_value = compile_error_guard(args.named.at("code"), env);
  do_set(form, code_field, code_value->to_gpr(form, env), code_value, env);

  // state name
  auto state_type = get_state_type_from_enter_and_code(enter_value->type(), code_value->type(),
                                                       state_parent_type, m_ts);

  auto state_name = args.unnamed.at(0).as_symbol()->name;
  auto existing_var = m_symbol_types.find(state_name);

  TypeSpec type_to_use;
  if (existing_var == m_symbol_types.end()) {
    // we're a new state. we must have a type.
    if (!state_type) {
      throw_compiler_error(form,
                           "define-state doesn't have enough information to determine the type of "
                           "state {}, and it was not forward declared.",
                           state_name);
    }
    type_to_use = *state_type;
    m_symbol_types[state_name] = *state_type;
  } else {
    type_to_use = existing_var->second;
    if (state_type) {
      typecheck(form, existing_var->second, *state_type,
                fmt::format("type of state {}", state_name));
    }
  }

  auto sym_val = env->function_env()->alloc_val<SymbolVal>(state_name, type_to_use);

  env->emit_ir<IR_SetSymbolValue>(form, sym_val, state_object);

  return get_none();
}

/*!
 * The define-state-hook compiler form is used from a macro in gstate.gc.
 * Args:
 *  - state_name, a symbol of the state's name.
 *  - state_parent, a symbol with the type name. Must be child of process
 *  - state, the actual state object to initialize.
 */
Val* Compiler::compile_define_virtual_state_hook(const goos::Object& form,
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
  auto state_object = compile_error_guard(args.unnamed.at(2), env)->to_gpr(form, env);
  if (state_object->type() != TypeSpec("state")) {
    throw_compiler_error(form, "define-state-hook got an invalid state object: had type {}",
                         state_object->type().print());
  }

  auto state_type_info = m_ts.get_type_of_type<StructureType>("state");

  // set the easy ones
  for (auto name : {"exit", "trans", "post", "event"}) {
    auto field = get_field_of_structure(state_type_info, state_object, name, env);
    auto value = compile_error_guard(args.named.at(name), env);
    do_set(form, field, value->to_gpr(form, env), value, env);
  }

  auto enter_field = get_field_of_structure(state_type_info, state_object, "enter", env);
  auto enter_value = compile_error_guard(args.named.at("enter"), env);
  do_set(form, enter_field, enter_value->to_gpr(form, env), enter_value, env);

  auto code_field = get_field_of_structure(state_type_info, state_object, "code", env);
  auto code_value = compile_error_guard(args.named.at("code"), env);
  do_set(form, code_field, code_value->to_gpr(form, env), code_value, env);

  // state name
  TypeSpec state_type("state");

  for (int i = 0; i < (int)code_value->type().arg_count() - 1; i++) {
    state_type.add_arg(code_value->type().get_arg(i));
  }
  state_type.add_arg(TypeSpec("_type_"));
  auto state_name = args.unnamed.at(0).as_symbol()->name;

  MethodInfo child_method_info;
  if (!m_ts.try_lookup_method(state_parent, state_name, &child_method_info)) {
    throw_compiler_error(
        form, "Tried to define a virtual state {} for type {}, but the state was not declared.",
        state_name, state_parent);
  }

  if (state_type != child_method_info.type) {
    throw_compiler_error(
        form, "Virtual state {} of {} was declared as {}, but got {}. First declared in type {}.",
        state_name, state_parent, child_method_info.type.print(), state_type.print(),
        child_method_info.defined_in_type);
  }

  MethodInfo parent_method_info;
  auto parent_of_parent_type = m_ts.lookup_type(state_parent)->get_parent();
  if (m_ts.try_lookup_method(parent_of_parent_type, state_name, &parent_method_info)) {
    // need to call inherit state TODO
    auto inherit_state_func =
        compile_get_symbol_value(form, "inherit-state", env)->to_gpr(form, env);
    auto parents_state =
        compile_get_method_of_type(form, TypeSpec(parent_of_parent_type), state_name, env)
            ->to_gpr(form, env);
    compile_real_function_call(form, inherit_state_func, {state_object, parents_state}, env);
  }

  // call method set.
  // (method-set! sunken-elevator 22 (the-as function gp-0))
  auto method_set_func = compile_get_symbol_value(form, "method-set!", env)->to_gpr(form, env);
  auto type_obj = compile_get_symbol_value(form, state_parent, env)->to_gpr(form, env);
  auto method_id = compile_integer(child_method_info.id, env)->to_gpr(form, env);
  compile_real_function_call(form, method_set_func, {type_obj, method_id, state_object}, env);

  return get_none();
}

/*!
 * The go-hook compiler form is used within go macros.
 * Args:
 * - process to perform the go on
 * - state
 * - args to enter the state with.
 */
Val* Compiler::compile_go_hook(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty()) {
    throw_compiler_error(form, "go-hook does not take named arguments");
  }

  if (args.unnamed.size() < 2) {
    throw_compiler_error(form, "go-hook must get at least 2 arguments");
  }

  // get the process
  auto proc = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);
  if (!m_ts.tc(TypeSpec("process"), proc->type())) {
    throw_compiler_error(form, "First argument to go-hook should be a process, got {} instead",
                         proc->type().print());
  }

  // get the state
  auto state = compile_error_guard(args.unnamed.at(1), env)->to_gpr(form, env);
  if (!m_ts.tc(TypeSpec("state"), state->type())) {
    throw_compiler_error(form, "Second argument to go-hook should be a state, got {} instead",
                         state->type().print());
  }

  // make sure the state is ok.
  if (state->type().arg_count() == 0) {
    throw_compiler_error(form, "Attempting to go to a state with incomplete type.");
  }

  // if we wanted to typecheck the process, we could do it here:
  //  auto expected_proc_type = state->type().last_arg();
  //  if (!m_ts.tc(expected_proc_type, proc->type())) {
  //    print_compiler_warning("Going to state of type {} from process of type {}.",
  //                           expected_proc_type.print(), proc->type().print());
  //  }

  // set the next state
  auto proc_type_info = m_ts.get_type_of_type<StructureType>("process");
  auto next_state_field = get_field_of_structure(proc_type_info, proc, "next-state", env);
  do_set(form, next_state_field, state, state, env);

  // now we have to call the function.
  auto enter_state_func = compile_get_symbol_value(form, "enter-state", env);
  enter_state_func->set_type(state_to_go_function(state->type()));

  std::vector<RegVal*> function_arguments;
  for (size_t i = 2; i < args.unnamed.size(); i++) {
    function_arguments.push_back(compile_error_guard(args.unnamed.at(i), env)->to_gpr(form, env));
  }

  // typechecking here will make sure the go is possible.
  return compile_real_function_call(form, enter_state_func->to_gpr(form, env), function_arguments,
                                    env);
}