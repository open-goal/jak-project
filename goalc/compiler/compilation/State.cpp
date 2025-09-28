#include "common/type_system/state.h"

#include "goalc/compiler/Compiler.h"

/*!
 * Helper func for compiling the set!'s within defstates
 */
void Compiler::compile_state_handler_set(StructureType* state_type_info,
                                         RegVal* state_object,
                                         const std::string& name,
                                         goos::Arguments& args,
                                         const goos::Object& form,
                                         Env* env,
                                         Val*& code_val,
                                         Val*& enter_val) {
  // do not set state handler field if handler is *no-state*
  // we don't use #f here because you might want to actually set the state to #f
  // (see crate-buzzer wait)
  auto& arg = args.named.at(name);
  if (!(arg.is_symbol() && arg.as_symbol() == "*no-state*")) {
    auto field = get_field_of_structure(state_type_info, state_object, name, env);
    auto value = compile_error_guard(arg, env);
    // we need to save these for typechecking later
    if (name == "code") {
      code_val = value;
    } else if (name == "enter") {
      enter_val = value;
    }
    do_set(form, field, value->to_gpr(form, env), value, env);
  }
}

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
  //  docstring
  //  named args for enter/exit/trans/post/event/code
  va_check(form, args,
           {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL, {}, goos::ObjectType::STRING},
           {
               {"enter", {true, {}}},
               {"exit", {true, {}}},
               {"trans", {true, {}}},
               {"post", {true, {}}},
               {"event", {true, {}}},
               {"code", {true, {}}},
           });

  std::string docstring = "";
  if (args.unnamed.size() == 4 && args.unnamed.at(3).is_string()) {
    docstring = args.unnamed.at(3).as_string()->data;
  }

  Val* code_value = NULL;
  Val* enter_value = NULL;

  // check parent
  auto& state_parent = args.unnamed.at(1).as_symbol();
  auto state_parent_type = m_ts.make_typespec(state_parent.name_ptr);
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
    compile_state_handler_set(state_type_info, state_object, name, args, form, env, code_value,
                              enter_value);
  }

  compile_state_handler_set(state_type_info, state_object, "enter", args, form, env, code_value,
                            enter_value);
  compile_state_handler_set(state_type_info, state_object, "code", args, form, env, code_value,
                            enter_value);

  // typecheck state
  std::optional<TypeSpec> state_type;
  if (code_value && enter_value) {
    state_type = get_state_type_from_enter_and_code(enter_value->type(), code_value->type(),
                                                    state_parent_type, m_ts);
  } else if (code_value || enter_value) {
    state_type = get_state_type_from_func(code_value ? code_value->type() : enter_value->type(),
                                          state_parent_type);
  }

  auto& state_name = args.unnamed.at(0).as_symbol();
  auto existing_var = m_symbol_types.lookup(state_name);

  TypeSpec type_to_use;
  if (!existing_var) {
    // we're a new state. we must have a type.
    if (!state_type) {
      throw_compiler_error(form,
                           "define-state doesn't have enough information to determine the type of "
                           "state {}, and it was not forward declared.",
                           state_name.name_ptr);
    }
    type_to_use = *state_type;
    m_symbol_types.set(state_name, *state_type);
  } else {
    type_to_use = *existing_var;
    if (state_type) {
      typecheck(form, *existing_var, *state_type,
                fmt::format("type of state {}", state_name.name_ptr));
    }
  }

  auto sym_val = env->function_env()->alloc_val<SymbolVal>(state_name.name_ptr, type_to_use);

  std::vector<symbol_info::ArgumentInfo> arg_info = {};
  if (code_value) {
    auto as_lambda = dynamic_cast<LambdaVal*>(code_value);
    if (as_lambda) {
      for (const auto& arg : as_lambda->lambda.params) {
        arg_info.push_back({.name = arg.name, .type = arg.type.base_type()});
      }
    }
  }

  m_symbol_info.add_state(state_name.name_ptr, state_parent_type.print(), false, arg_info, form,
                          docstring);
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
  //  docstring
  //  is_override
  //  named args for enter/exit/trans/post/event/code
  va_check(form, args,
           {goos::ObjectType::SYMBOL,
            goos::ObjectType::SYMBOL,
            {},
            goos::ObjectType::STRING,
            goos::ObjectType::SYMBOL},
           {
               {"enter", {true, {}}},
               {"exit", {true, {}}},
               {"trans", {true, {}}},
               {"post", {true, {}}},
               {"event", {true, {}}},
               {"code", {true, {}}},
           });

  std::string docstring = "";
  if (args.unnamed.size() == 5 && args.unnamed.at(3).is_string()) {
    docstring = args.unnamed.at(3).as_string()->data;
  }

  Val* code_value = NULL;
  Val* enter_value = NULL;

  // check parent
  auto& state_parent = args.unnamed.at(1).as_symbol();
  auto state_parent_type = m_ts.make_typespec(state_parent.name_ptr);
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

  auto& state_name = args.unnamed.at(0).as_symbol();

  MethodInfo child_method_info;
  if (!m_ts.try_lookup_method(state_parent.name_ptr, state_name.name_ptr, &child_method_info)) {
    throw_compiler_error(
        form, "Tried to define a virtual state {} for type {}, but the state was not declared.",
        state_name.name_ptr, state_parent.name_ptr);
  }

  MethodInfo parent_method_info;
  auto parent_of_parent_type = m_ts.lookup_type(state_parent.name_ptr)->get_parent();
  if (m_ts.try_lookup_method(parent_of_parent_type, state_name.name_ptr, &parent_method_info) &&
      args.unnamed.at(4).as_symbol() == "#f") {
    // need to call inherit state TODO
    auto inherit_state_func =
        compile_get_symbol_value(form, "inherit-state", env)->to_gpr(form, env);
    auto parents_state =
        compile_get_method_of_type(form, TypeSpec(parent_of_parent_type), state_name.name_ptr, env)
            ->to_gpr(form, env);
    compile_real_function_call(form, inherit_state_func, {state_object, parents_state}, env);
  }

  // call method set.
  // (method-set! sunken-elevator 22 (the-as function gp-0))
  auto method_set_func = compile_get_symbol_value(form, "method-set!", env)->to_gpr(form, env);
  auto type_obj = compile_get_symbol_value(form, state_parent.name_ptr, env)->to_gpr(form, env);
  auto method_id = compile_integer(child_method_info.id, env)->to_gpr(form, env);
  compile_real_function_call(form, method_set_func, {type_obj, method_id, state_object}, env);

  auto state_type_info = m_ts.get_type_of_type<StructureType>("state");

  // set the easy ones
  for (auto name : {"exit", "trans", "post", "event"}) {
    compile_state_handler_set(state_type_info, state_object, name, args, form, env, code_value,
                              enter_value);
  }

  compile_state_handler_set(state_type_info, state_object, "enter", args, form, env, code_value,
                            enter_value);
  compile_state_handler_set(state_type_info, state_object, "code", args, form, env, code_value,
                            enter_value);

  // typecheck state
  std::optional<TypeSpec> state_type;
  if (code_value && enter_value) {
    state_type = get_state_type_from_enter_and_code(enter_value->type(), code_value->type(),
                                                    state_parent_type, m_ts);
  } else if (code_value || enter_value) {
    state_type = get_state_type_from_func(code_value ? code_value->type() : enter_value->type(),
                                          state_parent_type);
  }

  if (state_type) {
    if (state_type !=
        child_method_info.type.substitute_for_method_call(state_parent_type.base_type())) {
      throw_compiler_error(
          form, "Virtual state {} of {} was declared as {}, but got {}. First declared in type {}.",
          state_name.name_ptr, state_parent.name_ptr, child_method_info.type.print(),
          state_type->print(), child_method_info.defined_in_type);
    }
  }

  std::vector<symbol_info::ArgumentInfo> arg_info = {};
  if (code_value) {
    auto as_lambda = dynamic_cast<LambdaVal*>(code_value);
    if (as_lambda) {
      for (const auto& arg : as_lambda->lambda.params) {
        arg_info.push_back({.name = arg.name, .type = arg.type.base_type()});
      }
    }
  }

  m_symbol_info.add_state(state_name.name_ptr, state_parent_type.print(), true, arg_info, form,
                          docstring);

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
  enter_state_func->set_type(state_to_go_function(state->type(), TypeSpec("object")));

  std::vector<RegVal*> function_arguments;
  for (size_t i = 2; i < args.unnamed.size(); i++) {
    function_arguments.push_back(compile_error_guard(args.unnamed.at(i), env)->to_gpr(form, env));
  }

  // typechecking here will make sure the go is possible.
  return compile_real_function_call(form, enter_state_func->to_gpr(form, env), function_arguments,
                                    env);
}
