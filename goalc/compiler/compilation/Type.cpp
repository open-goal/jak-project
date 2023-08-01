#include "common/log/log.h"
#include "common/type_system/defenum.h"
#include "common/type_system/deftype.h"
#include "common/type_system/state.h"
#include "common/util/math_util.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/emitter/CallingConvention.h"

#include "third-party/fmt/core.h"

namespace {

/*!
 * Given a method id, get the offset in bytes from the GOAL type to the method pointer.
 */
int get_offset_of_method(int id) {
  // todo - something that looks at the type system?
  // this will need changing if the layout of type ever changes.
  return 16 + 4 * id;
}
}  // namespace

/*!
 * Given a type and method name (known at compile time), get the method, from the given type object.
 * To do method lookup, the given type must be the same as, or a child of, the given compile time
 * type.
 */
RegVal* Compiler::compile_get_method_of_type(const goos::Object& form,
                                             const TypeSpec& compile_time_type,
                                             RegVal* type,
                                             const std::string& method_name,
                                             Env* env) {
  auto info = m_ts.lookup_method(compile_time_type.base_type(), method_name);
  info.type = info.type.substitute_for_method_call(compile_time_type.base_type());
  auto offset_of_method = get_offset_of_method(info.id);
  ASSERT(type->type() == TypeSpec("type"));

  auto fe = env->function_env();

  MemLoadInfo load_info;
  load_info.sign_extend = false;
  load_info.size = POINTER_SIZE;

  auto loc_type = m_ts.make_pointer_typespec(info.type);
  auto loc = fe->alloc_val<MemoryOffsetConstantVal>(loc_type, type, offset_of_method);
  auto di = m_ts.get_deref_info(loc_type);
  ASSERT(di.can_deref);
  ASSERT(di.mem_deref);
  ASSERT(di.sign_extend == false);
  ASSERT(di.load_size == 4);

  auto deref = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
  return deref->to_reg(form, env);
}

/*!
 * Look up a method from the type, with the type specified at compile time.
 * This can be used for method calls where the type can't be found at run time, but is known at
 * compile time. (non-virtual method call)
 */
RegVal* Compiler::compile_get_method_of_type(const goos::Object& form,
                                             const TypeSpec& compile_time_type,
                                             const std::string& method_name,
                                             Env* env) {
  auto typ = compile_get_symbol_value(form, compile_time_type.base_type(), env)->to_gpr(form, env);
  return compile_get_method_of_type(form, compile_time_type, typ, method_name, env);
}

/*!
 * Given an object, get a method. If at compile time we know it's a basic, we use its runtime
 * type to look up the method at runtime (virtual call).  If we don't know it's a basic, we get the
 * method from the compile-time type. (fixed type non-virtual call)
 */
RegVal* Compiler::compile_get_method_of_object(const goos::Object& form,
                                               RegVal* object,
                                               const std::string& method_name,
                                               Env* env,
                                               bool error_message_function_or_method) {
  auto& compile_time_type = object->type();
  MethodInfo method_info;
  if (!m_ts.try_lookup_method(compile_time_type.base_type(), method_name, &method_info)) {
    if (error_message_function_or_method) {
      throw_compiler_error(form, "No method or function named {} for type {}", method_name,
                           compile_time_type.print());
    } else {
      throw_compiler_error(form, "Type {} has no method {}", compile_time_type.print(),
                           method_name);
    }
  }

  method_info.type = method_info.type.substitute_for_method_call(compile_time_type.base_type());
  auto fe = env->function_env();

  RegVal* runtime_type = nullptr;
  if (m_ts.should_use_virtual_methods(compile_time_type, method_info.id)) {
    runtime_type = fe->make_gpr(m_ts.make_typespec("type"));
    MemLoadInfo info;
    info.size = 4;
    info.sign_extend = false;
    info.reg = RegClass::GPR_64;
    env->emit_ir<IR_LoadConstOffset>(form, runtime_type, -4, object, info);
  } else {
    // can't look up at runtime
    runtime_type =
        compile_get_symbol_value(form, compile_time_type.base_type(), env)->to_gpr(form, env);
  }

  auto offset_of_method = get_offset_of_method(method_info.id);
  MemLoadInfo load_info;
  load_info.sign_extend = false;
  load_info.size = POINTER_SIZE;

  auto loc_type = m_ts.make_pointer_typespec(method_info.type);
  auto loc = fe->alloc_val<MemoryOffsetConstantVal>(loc_type, runtime_type, offset_of_method);
  auto di = m_ts.get_deref_info(loc_type);
  ASSERT(di.can_deref);
  ASSERT(di.mem_deref);
  ASSERT(di.sign_extend == false);
  ASSERT(di.load_size == 4);

  auto deref = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
  return deref->to_reg(form, env);
}

Val* Compiler::compile_format_string(const goos::Object& form,
                                     Env* env,
                                     const std::string& fmt_template,
                                     std::vector<RegVal*> args,
                                     const std::string& out_stream) {
  // Add first two format args
  args.insert(args.begin(),
              compile_string(fmt_template, env, env->function_env()->segment)->to_gpr(form, env));
  args.insert(args.begin(), compile_get_sym_obj(out_stream, env)->to_gpr(form, env));

  // generate code in the method_env
  auto format_function = compile_get_symbol_value(form, "_format", env)->to_gpr(form, env);
  return compile_real_function_call(form, format_function, args, env);
}

void Compiler::generate_field_description(const goos::Object& form,
                                          StructureType* type,
                                          Env* env,
                                          RegVal* reg,
                                          const Field& f,
                                          int tab_count) {
  std::string str_template;
  std::string tabs;
  for (int i = 0; i < tab_count; i++) {
    tabs += "~T";
  }
  std::vector<RegVal*> format_args = {};
  if (f.name() == "type" && f.offset() == 0) {
    // type
    return;
  } else if (f.is_array() && !f.is_dynamic()) {
    // Arrays
    str_template += fmt::format("{}{}[{}] @ #x~X~%", tabs, f.name(), f.array_size());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (f.is_dynamic()) {
    // Dynamic Field
    str_template += fmt::format("{}{}[0] @ #x~X~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (m_ts.tc(m_ts.make_typespec("basic"), f.type()) ||
             m_ts.tc(m_ts.make_typespec("binteger"), f.type()) ||
             m_ts.tc(m_ts.make_typespec("pair"), f.type()) ||
             m_ts.tc(m_ts.make_typespec("symbol"), f.type())) {
    // basic, binteger, pair
    str_template += fmt::format("{}{}: ~A~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (m_ts.tc(m_ts.make_typespec("structure"), f.type())) {
    // Structure
    str_template += fmt::format("{}{}: #<{} @ #x~X>~%", tabs, f.name(), f.type().print());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (f.type() == TypeSpec("seconds")) {
    // seconds
    str_template += fmt::format("{}{}: (seconds ~e)~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (m_ts.tc(m_ts.make_typespec("integer"), f.type())) {
    // Integer
    if (m_ts.lookup_type(f.type())->get_load_size() > 8) {
      str_template += fmt::format("{}{}: <cannot-print>~%", tabs, f.name());
    } else {
      str_template += fmt::format("{}{}: ~D~%", tabs, f.name());
      format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
    }

  } else if (f.type() == TypeSpec("meters")) {
    // meters
    str_template += fmt::format("{}{}: (meters ~m)~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (f.type() == TypeSpec("degrees")) {
    // degrees
    str_template += fmt::format("{}{}: (degrees ~r)~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (m_ts.tc(m_ts.make_typespec("float"), f.type())) {
    // Float
    str_template += fmt::format("{}{}: ~f~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else if (m_ts.tc(m_ts.make_typespec("pointer"), f.type())) {
    // Pointers
    str_template += fmt::format("{}{}: #x~X~%", tabs, f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(form, env));
  } else {
    // Otherwise, we havn't implemented it!
    str_template += fmt::format("{}{}: Undefined!~%", tabs, f.name());
  }

  compile_format_string(form, env, str_template, format_args);
}

Val* Compiler::generate_inspector_for_structure_type(const goos::Object& form,
                                                     Env* env,
                                                     StructureType* structure_type) {
  // Create a function environment to hold the code for the inspect method. The name is just for
  // debugging.
  auto method_env = std::make_unique<FunctionEnv>(
      env, "autogenerated-inspect-method-of-" + structure_type->get_name(), &m_goos.reader);
  // put the method in the debug segment.
  method_env->set_segment(DEBUG_SEGMENT);

  // Create a register which will hold the input to the inspect method
  auto input_arg = method_env->make_gpr(structure_type->get_name());
  // "Constrain" this register to be the register that the function argument is passed in
  IRegConstraint constraint;
  constraint.instr_idx = 0;             // constraint at the start of the function
  constraint.ireg = input_arg->ireg();  // constrain this register
  constraint.desired_register = emitter::gRegInfo.get_gpr_arg_reg(0);  // to the first argument
  method_env->constrain(constraint);
  // Inform the compiler that `input`'s value will be written to `rdi` (first arg register)
  method_env->emit_ir<IR_ValueReset>(form, std::vector<RegVal*>{input_arg});

  auto input = method_env->make_gpr(structure_type->get_name());
  method_env->emit_ir<IR_RegSet>(form, input, input_arg);

  // there's a special case for children of process.
  if (m_ts.fully_defined_type_exists("process") &&
      m_ts.tc(TypeSpec("process"), TypeSpec(structure_type->get_name()))) {
    // first, call the inspect method of our parent type.
    auto parent_type_name = structure_type->get_parent();
    auto parent_inspect =
        compile_get_method_of_type(form, TypeSpec(parent_type_name), "inspect", method_env.get());
    std::vector<RegVal*> args = {input};
    compile_real_function_call(form, parent_inspect, args, method_env.get(), parent_type_name);
    auto parent_type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(parent_type_name));
    if (!parent_type_info) {
      throw_compiler_error(form, "Got an invalid parent type in process inspect method");
    }

    for (size_t i = parent_type_info->fields().size(); i < structure_type->fields().size(); i++) {
      generate_field_description(form, structure_type, method_env.get(), input,
                                 structure_type->fields().at(i), 2);
    }

  } else {
    RegVal* type_name = nullptr;
    if (dynamic_cast<BasicType*>(structure_type)) {
      type_name = get_field_of_structure(structure_type, input, "type", method_env.get())
                      ->to_gpr(form, method_env.get());
    } else {
      type_name = compile_get_sym_obj(structure_type->get_name(), method_env.get())
                      ->to_gpr(form, method_env.get());
    }
    compile_format_string(form, method_env.get(), "[~8x] ~A~%", {input, type_name});

    for (const Field& f : structure_type->fields()) {
      generate_field_description(form, structure_type, method_env.get(), input, f, 1);
    }
  }

  method_env->emit_ir<IR_Return>(form, method_env->make_gpr(input->type()), input,
                                 emitter::gRegInfo.get_gpr_ret_reg());

  // add this function to the object file
  auto fe = env->function_env();
  auto method = fe->alloc_val<LambdaVal>(m_ts.make_typespec("function"), false);
  method->func = method_env.get();
  auto obj_env_inspect = method_env->file_env();
  obj_env_inspect->add_function(std::move(method_env));

  // call method-set!
  auto type_obj =
      compile_get_symbol_value(form, structure_type->get_name(), env)->to_gpr(form, env);
  auto id_val = compile_integer(m_ts.lookup_method(structure_type->get_name(), "inspect").id, env)
                    ->to_gpr(form, env);
  auto method_set_val = compile_get_symbol_value(form, "method-set!", env)->to_gpr(form, env);
  return compile_real_function_call(form, method_set_val,
                                    {type_obj, id_val, method->to_gpr(form, env)}, env);
}

Val* Compiler::generate_inspector_for_bitfield_type(const goos::Object& form,
                                                    Env* env,
                                                    BitFieldType* bitfield_type) {
  bool bitfield_128 = bitfield_type->get_load_size() == 16;
  // Create a function environment to hold the code for the inspect method. The name is just for
  // debugging.
  auto method_env = std::make_unique<FunctionEnv>(
      env, "autogenerated-inspect-method-of-" + bitfield_type->get_name(), &m_goos.reader);
  // put the method in the debug segment.
  method_env->set_segment(DEBUG_SEGMENT);

  // Create a register which will hold the input to the inspect method
  auto input_arg = method_env->make_gpr(bitfield_type->get_name());
  // "Constrain" this register to be the register that the function argument is passed in
  IRegConstraint constraint;
  constraint.instr_idx = 0;             // constraint at the start of the function
  constraint.ireg = input_arg->ireg();  // constrain this register
  if (bitfield_128) {
    constraint.desired_register = emitter::gRegInfo.get_xmm_arg_reg(0);  // to the first argument
  } else {
    constraint.desired_register = emitter::gRegInfo.get_gpr_arg_reg(0);  // to the first argument
  }

  method_env->constrain(constraint);
  // Inform the compiler that `input`'s value will be written to `rdi` (first arg register)
  method_env->emit_ir<IR_ValueReset>(form, std::vector<RegVal*>{input_arg});

  auto input = method_env->make_gpr(bitfield_type->get_name());
  method_env->emit_ir<IR_RegSet>(form, input, input_arg);

  RegVal* type_name = compile_get_sym_obj(bitfield_type->get_name(), method_env.get())
                          ->to_gpr(form, method_env.get());
  compile_format_string(form, method_env.get(), "[~8x] ~A~%", {input, type_name});

  for (const BitField& bf : bitfield_type->fields()) {
    std::string str_template;
    std::vector<RegVal*> format_args = {};
    str_template += fmt::format("~T{}: ~D | 0x~X | 0b~B~%", bf.name());
    auto value = get_field_of_bitfield(bitfield_type, input, bf.name(), method_env.get())
                     ->to_gpr(form, method_env.get());
    format_args.push_back(value);
    format_args.push_back(value);
    format_args.push_back(value);
    compile_format_string(form, method_env.get(), str_template, format_args);
  }

  if (bitfield_128) {
    method_env->emit_ir<IR_Return>(form, method_env->make_ireg(input->type(), RegClass::INT_128),
                                   input, emitter::gRegInfo.get_gpr_ret_reg());
  } else {
    method_env->emit_ir<IR_Return>(form, method_env->make_gpr(input->type()), input,
                                   emitter::gRegInfo.get_gpr_ret_reg());
  }

  // add this function to the object file
  auto fe = env->function_env();
  auto method = fe->alloc_val<LambdaVal>(m_ts.make_typespec("function"), false);
  method->func = method_env.get();
  auto obj_env_inspect = method_env->file_env();
  obj_env_inspect->add_function(std::move(method_env));

  // call method-set!
  auto type_obj = compile_get_symbol_value(form, bitfield_type->get_name(), env)->to_gpr(form, env);
  auto id_val = compile_integer(m_ts.lookup_method(bitfield_type->get_name(), "inspect").id, env)
                    ->to_gpr(form, env);
  auto method_set_val = compile_get_symbol_value(form, "method-set!", env)->to_gpr(form, env);
  return compile_real_function_call(form, method_set_val,
                                    {type_obj, id_val, method->to_gpr(form, env)}, env);
}

/*!
 * Compile a (deftype ... form)
 */
Val* Compiler::compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  // parse the type definition and add to the type system
  auto result = parse_deftype(rest, &m_ts, &m_global_constants);

  // look up the type name
  auto kv = m_symbol_types.find(result.type.base_type());
  if (kv != m_symbol_types.end() && kv->second.base_type() != "type") {
    // we already have something that's not a type with the same name, this is bad.
    lg::print("[Warning] deftype will redefine {} from {} to a type.\n", result.type.base_type(),
              kv->second.print());
  }
  // remember that this is a type
  m_symbol_types[result.type.base_type()] = m_ts.make_typespec("type");

  // add declared states
  for (auto& state : result.type_info->get_states_declared_for_type()) {
    auto existing_type = m_symbol_types.find(state.first);
    if (existing_type != m_symbol_types.end() && existing_type->second != state.second) {
      if (m_throw_on_define_extern_redefinition) {
        throw_compiler_error(form, "deftype would redefine the type of state {} from {} to {}.",
                             state.first, existing_type->second.print(), state.second.print());
      } else {
        print_compiler_warning(
            "[Warning] deftype has redefined the type of state {}\npreviously: {}\nnow: "
            "{}\n",
            state.first.c_str(), existing_type->second.print().c_str(),
            state.second.print().c_str());
      }
    }

    m_symbol_types[state.first] = state.second;
    m_symbol_info.add_fwd_dec(state.first, form);
  }

  if (result.create_runtime_type) {
    // get the new method of type object. this is new_type in kscheme.cpp
    auto new_type_method = compile_get_method_of_type(form, m_ts.make_typespec("type"), "new", env);
    // call (new 'type 'type-name parent-type flags)
    auto new_type_symbol = compile_get_sym_obj(result.type.base_type(), env)->to_gpr(form, env);
    auto parent_type =
        compile_get_symbol_value(form, result.type_info->get_parent(), env)->to_gpr(form, env);
    auto flags_int = compile_integer(result.flags.flag, env)->to_gpr(form, env);
    compile_real_function_call(form, new_type_method, {new_type_symbol, parent_type, flags_int},
                               env);
  }

  // Auto-generate (inspect) method
  if (result.type_info->gen_inspect()) {
    auto as_structure_type = dynamic_cast<StructureType*>(result.type_info);
    if (as_structure_type) {  // generate the inspect method
      generate_inspector_for_structure_type(form, env, as_structure_type);
    } else {
      auto as_bitfield_type = dynamic_cast<BitFieldType*>(result.type_info);
      if (as_bitfield_type && as_bitfield_type->get_load_size() <= 8) {  // Avoid 128-bit bitfields
        generate_inspector_for_bitfield_type(form, env, as_bitfield_type);
      }
    }
  }

  m_symbol_info.add_type(result.type.base_type(), form);

  // return none, making the value of (deftype..) unusable
  return get_none();
}

/*!
 * Compile a (defmethod ...) form
 */
Val* Compiler::compile_defmethod(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto fe = env->function_env();
  auto* rest = &_rest;

  auto& method_name = pair_car(*rest);
  rest = &pair_cdr(*rest);
  auto& type_name = pair_car(*rest);
  rest = &pair_cdr(*rest);
  auto& arg_list = pair_car(*rest);
  auto body = &pair_cdr(*rest);

  if (!method_name.is_symbol()) {
    throw_compiler_error(form, "Method name must be a symbol, got {}", method_name.print());
  }
  if (!type_name.is_symbol()) {
    throw_compiler_error(form, "Method type must be a symbol, got {}", method_name.print());
  }

  auto place = fe->alloc_val<LambdaVal>(get_none()->type(), false);
  auto& lambda = place->lambda;
  auto lambda_ts = m_ts.make_typespec("function");

  // parse the argument list. todo, we could check the type of the first argument here?
  for_each_in_list(arg_list, [&](const goos::Object& o) {
    if (o.is_symbol()) {
      // if it has no type, assume object.
      lambda.params.push_back({symbol_string(o), m_ts.make_typespec("object")});
      lambda_ts.add_arg(m_ts.make_typespec("object"));
    } else {
      // type of argument is specified
      auto param_args = get_va(o, o);
      va_check(o, param_args, {goos::ObjectType::SYMBOL, {}}, {});

      GoalArg parm;
      parm.name = symbol_string(param_args.unnamed.at(0));
      parm.type = parse_typespec(param_args.unnamed.at(1), env);
      // before substituting _type_
      lambda_ts.add_arg(parm.type);

      // replace _type_ as needed for inside this function.
      parm.type = parm.type.substitute_for_method_call(symbol_string(type_name));
      lambda.params.push_back(parm);
    }
  });
  ASSERT(lambda.params.size() == lambda_ts.arg_count());
  // todo, verify argument list types (check that first arg is _type_ for methods that aren't "new")
  lambda.debug_name = fmt::format("(method {} {})", method_name.print(), type_name.print());

  std::optional<std::string> docstring;
  if (body->as_pair()->car.is_string() && !body->as_pair()->cdr.is_empty_list()) {
    docstring = pair_car(*body).as_string()->data;
    body = &pair_cdr(*body);
  }

  lambda.body = *body;
  place->func = nullptr;

  auto new_func_env = std::make_unique<FunctionEnv>(env, lambda.debug_name, &m_goos.reader);
  new_func_env->set_segment(env->function_env()->segment_for_static_data());
  new_func_env->method_of_type_name = symbol_string(type_name);

  // set up arguments
  if (lambda.params.size() > 8) {
    throw_compiler_error(form, "Methods cannot have more than 8 arguments");
  }
  std::vector<RegVal*> reset_args_for_coloring;
  std::vector<TypeSpec> arg_types;
  for (auto& parm : lambda.params) {
    arg_types.push_back(parm.type);
  }
  auto arg_regs = get_arg_registers(m_ts, arg_types);

  for (u32 i = 0; i < lambda.params.size(); i++) {
    IRegConstraint constr;
    constr.instr_idx = 0;  // constraint at function start
    auto ireg_arg = new_func_env->make_ireg(
        lambda.params.at(i).type, arg_regs.at(i).is_gpr() ? RegClass::GPR_64 : RegClass::INT_128);
    ireg_arg->mark_as_settable();
    constr.ireg = ireg_arg->ireg();
    constr.desired_register = arg_regs.at(i);

    new_func_env->constrain(constr);
    reset_args_for_coloring.push_back(ireg_arg);
  }

  auto method_info = m_ts.lookup_method(symbol_string(type_name), symbol_string(method_name));
  auto behavior = method_info.type.try_get_tag("behavior");
  if (behavior) {
    auto self_var = new_func_env->make_gpr(m_ts.make_typespec(*behavior));
    IRegConstraint constr;
    constr.contrain_everywhere = true;
    constr.desired_register = emitter::gRegInfo.get_process_reg();
    constr.ireg = self_var->ireg();
    self_var->set_rlet_constraint(constr.desired_register);
    new_func_env->constrain(constr);

    if (new_func_env->params.find("self") != new_func_env->params.end()) {
      throw_compiler_error(form, "Cannot have an argument named self in a behavior");
    }
    new_func_env->params["self"] = self_var;
    reset_args_for_coloring.push_back(self_var);
    lambda_ts.add_new_tag("behavior", *behavior);
  }

  place->func = new_func_env.get();

  // nasty function block env setup
  auto return_reg = new_func_env->make_gpr(get_none()->type());
  auto func_block_env = new_func_env->alloc_env<BlockEnv>(new_func_env.get(), "#f");
  func_block_env->return_value = return_reg;
  func_block_env->end_label = Label(new_func_env.get());
  func_block_env->emit_ir<IR_ValueReset>(form, reset_args_for_coloring);

  for (u32 i = 0; i < lambda.params.size(); i++) {
    auto ireg = new_func_env->make_ireg(
        lambda.params.at(i).type, arg_regs.at(i).is_gpr() ? RegClass::GPR_64 : RegClass::INT_128);
    ireg->mark_as_settable();
    if (!new_func_env->params.insert({lambda.params.at(i).name, ireg}).second) {
      throw_compiler_error(form, "defmethod has multiple arguments named {}",
                           lambda.params.at(i).name);
    }
    new_func_env->emit_ir<IR_RegSet>(form, ireg, reset_args_for_coloring.at(i));
  }

  // compile the function!
  Val* result = nullptr;
  bool first_thing = true;
  for_each_in_list(lambda.body, [&](const goos::Object& o) {
    result = compile_error_guard(o, func_block_env);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(o, func_block_env);
    }
    if (first_thing) {
      first_thing = false;
      // you could probably cheat and do a (begin (blorp) (declare ...)) to get around this.
      new_func_env->settings.is_set = true;
    }
  });

  if (new_func_env->is_asm_func) {
    // don't add return automatically!
    lambda_ts.add_arg(new_func_env->asm_func_return_type);
  } else if (result && !dynamic_cast<None*>(result)) {
    RegVal* final_result;
    emitter::Register ret_hw_reg = emitter::gRegInfo.get_gpr_ret_reg();
    if (m_ts.lookup_type(result->type())->get_load_size() == 16) {
      ret_hw_reg = emitter::gRegInfo.get_xmm_ret_reg();
      final_result = result->to_xmm128(form, new_func_env.get());
      return_reg->change_class(RegClass::INT_128);
    } else {
      final_result = result->to_gpr(form, new_func_env.get());
    }

    func_block_env->return_types.push_back(final_result->type());

    for (const auto& possible_type : func_block_env->return_types) {
      if (possible_type != TypeSpec("none") &&
          m_ts.lookup_type(possible_type)->get_load_size() == 16) {
        return_reg->change_class(RegClass::INT_128);
        break;
      }
    }

    new_func_env->emit_ir<IR_Return>(form, return_reg, final_result, ret_hw_reg);

    auto return_type = m_ts.lowest_common_ancestor(func_block_env->return_types);
    lambda_ts.add_arg(return_type);
  } else {
    lambda_ts.add_arg(m_ts.make_typespec("none"));
  }
  func_block_env->end_label.idx = new_func_env->code().size();
  new_func_env->emit_ir<IR_Null>(form);
  new_func_env->finish();

  auto obj_env = new_func_env->file_env();
  ASSERT(obj_env);
  if (new_func_env->settings.save_code) {
    obj_env->add_function(std::move(new_func_env));
  }
  place->set_type(lambda_ts);

  auto info = m_ts.define_method(symbol_string(type_name), symbol_string(method_name), lambda_ts,
                                 docstring);
  auto type_obj = compile_get_symbol_value(form, symbol_string(type_name), env)->to_gpr(form, env);
  auto id_val = compile_integer(info.id, env)->to_gpr(form, env);
  auto method_val = place->to_gpr(form, env);
  auto method_set_val = compile_get_symbol_value(form, "method-set!", env)->to_gpr(form, env);

  m_symbol_info.add_method(symbol_string(method_name), lambda.params, info, form);
  return compile_real_function_call(form, method_set_val, {type_obj, id_val, method_val}, env);
}

/*!
 * Given a type, object, and field name, get the value of the field.
 */
Val* Compiler::get_field_of_structure(const StructureType* type,
                                      Val* object,
                                      const std::string& field_name,
                                      Env* env) {
  auto fe = env->function_env();
  Val* result = nullptr;
  int offset = -type->get_offset();
  auto field = m_ts.lookup_field_info(type->get_name(), field_name);
  if (field.needs_deref) {
    TypeSpec loc_type = m_ts.make_pointer_typespec(field.type);
    auto loc =
        fe->alloc_val<MemoryOffsetConstantVal>(loc_type, object, field.field.offset() + offset);
    auto di = m_ts.get_deref_info(loc_type);
    ASSERT(di.can_deref);
    ASSERT(di.mem_deref);
    result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
    result->mark_as_settable();
  } else {
    const auto& type_for_offset =
        field.type.base_type() == "inline-array" ? field.type.get_single_arg() : field.type;
    auto field_type_info = m_ts.lookup_type(type_for_offset);
    result = fe->alloc_val<MemoryOffsetConstantVal>(
        field.type, object, field.field.offset() + offset + field_type_info->get_offset());
    result->mark_as_settable();
  }
  return result;
}

Val* Compiler::get_field_of_bitfield(const BitFieldType* type,
                                     Val* object,
                                     const std::string& field_name,
                                     Env* env) {
  auto fe = env->function_env();
  Val* result = nullptr;
  auto bitfield_info = m_ts.lookup_bitfield_info(type->get_name(), field_name);
  result = fe->alloc_val<BitFieldVal>(bitfield_info.result_type, object, bitfield_info.offset,
                                      bitfield_info.size, bitfield_info.sign_extend,
                                      type->get_load_size() == 16);
  return result;
}

/*!
 * Compile the (-> ...) form.
 * This is kind of a mess because of the huge number of things you can do with this form:
 * - dereference a pointer
 * - Access a field of a type
 * - Access an element of an array or inline-array
 * and they nest in confusing and ambiguous ways.
 * The current behavior is that there is exactly one dereference performed per thing in the list.
 * so if field x has type (pointer y), (-> obj x) gives a (pointer y), not a y.
 *
 * The result of this should give something that has enough information to read/write the original
 * location. Otherwise set! or & won't work.
 */
Val* Compiler::compile_deref(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto fe = env->function_env();
  if (_rest.is_empty_list()) {
    throw_compiler_error(form, "-> must get at least one argument");
  }

  auto& first_arg = pair_car(_rest);
  auto rest = &pair_cdr(_rest);

  // eval the first thing
  auto result = compile_error_guard(first_arg, env);

  if (rest->is_empty_list()) {
    // one argument, do a pointer deref
    auto deref_info = m_ts.get_deref_info(result->type());
    if (!deref_info.can_deref) {
      throw_compiler_error(form, "Cannot dereference a {}.", result->type().print());
    }

    if (deref_info.mem_deref) {
      result =
          fe->alloc_val<MemoryDerefVal>(deref_info.result_type, result, MemLoadInfo(deref_info));
      result->mark_as_settable();
    } else {
      ASSERT(false);
    }
    return result;
  }

  // compound, is field access/nested access
  while (!rest->is_empty_list()) {
    auto field_obj = pair_car(*rest);
    rest = &pair_cdr(*rest);
    auto type_info = m_ts.lookup_type_allow_partial_def(result->type());

    // attempt to treat it as a field. May not succeed if we're actually an array.
    if (field_obj.is_symbol()) {
      auto field_name = symbol_string(field_obj);
      auto struct_type = dynamic_cast<StructureType*>(type_info);

      if (struct_type) {
        if (result->type().base_type() == "array") {
          // this is an ugly hack, but I don't know a better way. For things that are both
          // array-indexable and a structure, we treat it like a structure only if the
          // deref thing is one of the field names. Otherwise, array.
          if (field_name == "content-type" || field_name == "length" ||
              field_name == "allocated-length" || field_name == "type" || field_name == "data") {
            // if accessing data, give the more specific pointer type.
            if (field_name == "data" && result->type().has_single_arg()) {
              auto elt_type = m_ts.make_pointer_typespec(result->type().get_single_arg());
              result = get_field_of_structure(struct_type, result, field_name, env);
              result->set_type(elt_type);
            } else {
              // otherwise, deref as normal
              result = get_field_of_structure(struct_type, result, field_name, env);
            }
            continue;
          }
        } else {
          const auto& in_type = result->type();
          result = get_field_of_structure(struct_type, result, field_name, env);

          // special case (-> <state> enter) should return the appropriate function type.
          if (in_type.arg_count() > 0 && in_type.base_type() == "state") {
            if (field_name == "enter" || field_name == "code") {
              result->set_type(state_to_go_function(in_type, TypeSpec("none")));
            }
          }

          continue;
        }
      }

      auto bitfield_type = dynamic_cast<BitFieldType*>(type_info);
      if (bitfield_type) {
        auto bitfield_info = m_ts.lookup_bitfield_info(type_info->get_name(), field_name);
        result = fe->alloc_val<BitFieldVal>(bitfield_info.result_type, result, bitfield_info.offset,
                                            bitfield_info.size, bitfield_info.sign_extend,
                                            type_info->get_load_size() == 16);
        continue;
      }
    }

    int64_t constant_index_value;
    RegVal* index_value = nullptr;
    auto idx_val = get_constant_integer_or_variable(field_obj, env);
    bool has_constant_idx = idx_val.is_constant();
    if (has_constant_idx) {
      constant_index_value = idx_val.constant;
    }

    if (!has_constant_idx) {
      index_value = idx_val.val->to_gpr(form, env);
      if (!is_integer(index_value->type())) {
        throw_compiler_error(form, "Cannot use -> with field {}.", field_obj.print());
      }
    }

    if (result->type().base_type() == "inline-array") {
      if (!result->type().has_single_arg()) {
        throw_compiler_error(form, "Cannot dereference an inline-array with type {}",
                             result->type().print());
      }
      auto di = m_ts.get_deref_info(result->type());
      ASSERT(di.can_deref);
      if (has_constant_idx) {
        result = fe->alloc_val<MemoryOffsetConstantVal>(di.result_type, result,
                                                        di.stride * constant_index_value);
      } else {
        // todo - use shifts if possible?
        RegVal* offset = fe->make_gpr(TypeSpec("int"));
        compile_constant_product(form, offset, index_value, di.stride, env);
        result = fe->alloc_val<MemoryOffsetVal>(di.result_type, result, offset);
      }
    } else if (result->type().base_type() == "pointer") {
      if (!result->type().has_single_arg()) {
        throw_compiler_error(form, "Cannot dereference a pointer with type {}",
                             result->type().print());
      }
      auto di = m_ts.get_deref_info(result->type());
      ASSERT(di.mem_deref);
      ASSERT(di.can_deref);
      Val* loc = nullptr;
      if (has_constant_idx) {
        loc = fe->alloc_val<MemoryOffsetConstantVal>(result->type(), result,
                                                     constant_index_value * di.stride);
      } else {
        RegVal* offset = fe->make_gpr(TypeSpec("int"));
        compile_constant_product(form, offset, index_value, di.stride, env);
        loc = fe->alloc_val<MemoryOffsetVal>(result->type(), result, offset);
      }
      result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
      result->mark_as_settable();
    } else if (result->type().base_type() == "array") {
      // accessing an array. this is a bit weird.
      if (!result->type().has_single_arg()) {
        throw_compiler_error(form, "The array type {} is invalid or cannot be indexed.",
                             result->type().print());
      }
      // the (pointer element-type)
      auto loc_type = m_ts.make_pointer_typespec(result->type().get_single_arg());
      // figure out how to access this...
      auto di = m_ts.get_deref_info(loc_type);
      // and the result
      auto base_type = di.result_type;
      ASSERT(base_type == result->type().get_single_arg());
      ASSERT(di.mem_deref);
      ASSERT(di.can_deref);
      Val* loc = nullptr;

      if (has_constant_idx) {
        loc = fe->alloc_val<MemoryOffsetConstantVal>(
            loc_type, result, ARRAY_DATA_OFFSET + di.stride * constant_index_value);
      } else {
        // the total offset is 12 + stride * idx
        auto arr_off = compile_integer(ARRAY_DATA_OFFSET, env)->to_gpr(form, env);
        RegVal* offset = fe->make_gpr(TypeSpec("int"));
        compile_constant_product(form, offset, index_value, di.stride, env);
        env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::ADD_64, offset, arr_off);

        // create a location to deref (so we can do address-of and get this), with pointer type
        loc = fe->alloc_val<MemoryOffsetVal>(loc_type, result, offset);
      }

      // and result type.
      result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
      // array values should be settable
      result->mark_as_settable();
    } else {
      throw_compiler_error(form, "Cannot access array of type {}.", result->type().print());
    }
  }
  return result;
}

TypeSpec coerce_to_stack_spill_type(const TypeSpec& in) {
  if (in == TypeSpec("int")) {
    return TypeSpec("int64");
  } else if (in == TypeSpec("uint")) {
    return TypeSpec("uint64");
  } else {
    return in;
  }
}

/*!
 * Compile the (& x) form.
 */
Val* Compiler::compile_addr_of(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto loc = compile_error_guard(args.unnamed.at(0), env);
  auto as_mem_deref = dynamic_cast<MemoryDerefVal*>(loc);
  if (as_mem_deref) {
    return as_mem_deref->base;
  }

  // for now, we will only allow taking the address for something that's already in a register,
  // like a function parameter or a lexical variable (declared in a let).
  // This avoids weird things like taking the address of a constant or some other weird temporary -
  // we could spill it to the stack and let you do this, but most of the time it's probably not what
  // you wanted to do.
  auto as_reg = dynamic_cast<RegVal*>(loc);
  if (as_reg) {
    // so we can take the address
    as_reg->force_on_stack();
    auto result =
        env->make_gpr(m_ts.make_pointer_typespec(coerce_to_stack_spill_type(as_reg->type())));
    env->emit_ir<IR_RegValAddr>(form, result, as_reg);
    return result;
  }

  throw_compiler_error(form, "Cannot take the address of {}.", loc->print());
  return nullptr;
}

/*!
 * Compile the (the-as x y) form. Like a reinterpret cast.
 * Will always produce a alias (so setting the result of this affects the base).
 */
Val* Compiler::compile_the_as(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto desired_ts = parse_typespec(args.unnamed.at(0), env);
  auto base = compile_error_guard(args.unnamed.at(1), env);
  auto result = env->function_env()->alloc_val<AliasVal>(desired_ts, base);
  if (base->settable()) {
    result->mark_as_settable();
  }
  return result;
}

/*!
 * Compile the (the x y) form. Like reinterpret case, but numbers (int bint float) will be
 * converted. In the case of numeric version it won't alias.  But all other cases alias, which is
 * confusing.
 */
Val* Compiler::compile_the(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto desired_ts = parse_typespec(args.unnamed.at(0), env);
  auto base = compile_error_guard(args.unnamed.at(1), env);

  if (is_number(base->type())) {
    if (m_ts.tc(m_ts.make_typespec("binteger"), desired_ts)) {
      return number_to_binteger(form, base, env);
    }

    if (m_ts.tc(m_ts.make_typespec("integer"), desired_ts)) {
      auto result = number_to_integer(form, base, env);
      if (result != base) {
        result->set_type(desired_ts);
        return result;
      } else {
        result = env->function_env()->alloc_val<AliasVal>(desired_ts, base);
        return result;
      }
    }

    if (m_ts.tc(m_ts.make_typespec("float"), desired_ts)) {
      return number_to_float(form, base, env);
    }
  }

  auto result = env->function_env()->alloc_val<AliasVal>(desired_ts, base);
  if (base->settable()) {
    result->mark_as_settable();
  }
  return result;
}

/*!
 * Debug util (print-type x) to compile x then print the type name at compile time.
 */
Val* Compiler::compile_print_type(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto result = compile(args.unnamed.at(0), env)->to_reg(form, env);
  lg::print("[TYPE] {} {}\n", result->type().print(), result->print());
  return result;
}

/*!
 * New on global or debug heap.
 */
Val* Compiler::compile_heap_new(const goos::Object& form,
                                const std::string& allocation,
                                const goos::Object& type,
                                const goos::Object* rest,
                                Env* env) {
  bool making_boxed_array = unquote(type).as_symbol()->name == "boxed-array";
  TypeSpec main_type;
  if (!making_boxed_array) {
    main_type = parse_typespec(unquote(type), env);
  }

  if (main_type == TypeSpec("inline-array") || main_type == TypeSpec("array")) {
    bool is_inline = main_type == TypeSpec("inline-array");
    auto elt_type = quoted_sym_as_string(pair_car(*rest));
    rest = &pair_cdr(*rest);

    auto count_obj = pair_car(*rest);
    rest = &pair_cdr(*rest);
    // try to get the size as a compile time constant.
    auto cv = get_constant_integer_or_variable(count_obj, env);
    bool is_constant_size = cv.is_constant();

    if (!rest->is_empty_list()) {
      // got extra arguments
      throw_compiler_error(form, "new array form got more arguments than expected");
    }

    auto ts = is_inline && m_ts.lookup_type(elt_type)->is_reference()
                  ? m_ts.make_inline_array_typespec(elt_type)
                  : m_ts.make_pointer_typespec(elt_type);
    auto info = m_ts.get_deref_info(ts);
    if (!info.can_deref) {
      throw_compiler_error(form, "Cannot make an {} of {}\n", main_type.print(), ts.print());
    }

    auto malloc_func = compile_get_symbol_value(form, "malloc", env)->to_reg(form, env);
    std::vector<RegVal*> args;
    args.push_back(compile_get_sym_obj(allocation, env)->to_reg(form, env));

    if (is_constant_size) {
      auto array_size = cv.constant * info.stride;
      args.push_back(compile_integer(array_size, env)->to_reg(form, env));
    } else {
      auto array_size = compile_integer(info.stride, env)->to_reg(form, env);
      env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::IMUL_32, array_size,
                                   cv.val->to_gpr(form, env));
      args.push_back(array_size);
    }

    auto new_array = compile_real_function_call(form, malloc_func, args, env);
    new_array->set_type(ts);
    return new_array;
  } else {
    bool got_content_type = false;  // for boxed array
    std::string content_type;       // for boxed array.
    if (making_boxed_array) {
      main_type = TypeSpec("array");
    }

    if (!m_ts.lookup_type(main_type)->is_reference()) {
      throw_compiler_error(form, "Cannot heap allocate the value type {}.", main_type.print());
    }
    std::vector<RegVal*> args;
    // allocation
    args.push_back(compile_get_sym_obj(allocation, env)->to_reg(form, env));
    // type
    args.push_back(compile_get_symbol_value(form, main_type.base_type(), env)->to_reg(form, env));
    // the other arguments
    for_each_in_list(*rest, [&](const goos::Object& o) {
      if (making_boxed_array && !got_content_type) {
        got_content_type = true;
        if (o.is_symbol()) {
          content_type = o.as_symbol()->name;
          args.push_back(compile_get_symbol_value(form, content_type, env)->to_reg(form, env));
        } else {
          throw_compiler_error(form, "Invalid boxed-array type {}", o.print());
        }
      } else {
        args.push_back(compile_error_guard(o, env)->to_reg(form, env));
      }
    });

    auto new_method = compile_get_method_of_type(form, main_type, "new", env);
    auto new_obj = compile_real_function_call(form, new_method, args, env);
    if (making_boxed_array) {
      // TODO - handle array subtypes here as well?
      new_obj->set_type(m_ts.make_array_typespec("array", m_ts.make_typespec(content_type)));
    } else {
      new_obj->set_type(main_type);
    }

    return new_obj;
  }
}

Val* Compiler::compile_static_new(const goos::Object& form,
                                  const goos::Object& type,
                                  const goos::Object* rest,
                                  Env* env) {
  auto unquoted_type = unquote(type);
  const auto& sym_name = unquoted_type.as_symbol()->name;
  // Check if the type is an array or a subtype of 'array'
  bool is_array = sym_name == "boxed-array" || sym_name == "array" || sym_name == "inline-array";
  if (!is_array) {
    const auto type_of_object = parse_typespec(unquoted_type, env);
    is_array = m_ts.typecheck_and_throw(TypeSpec("array"), type_of_object, "", false, false, false);
  }
  if (unquoted_type.is_symbol() && is_array) {
    auto fe = env->function_env();
    auto sr = compile_static(form, env);
    auto result = fe->alloc_val<StaticVal>(sr.reference(), sr.typespec());
    return result;
  } else {
    const auto type_of_object = parse_typespec(unquoted_type, env);
    if (is_structure(type_of_object)) {
      return compile_new_static_structure_or_basic(form, type_of_object, *rest, env,
                                                   env->function_env()->segment_for_static_data());
    }

    if (is_bitfield(type_of_object)) {
      return compile_bitfield_definition(form, type_of_object, *rest, true, env);
    }
  }

  throw_compiler_error(form, "Cannot allocate a static object of type {}", type.print());
  return get_none();
}

Val* Compiler::compile_stack_new(const goos::Object& form,
                                 const goos::Object& type,
                                 const goos::Object* rest,
                                 Env* env,
                                 bool call_constructor,
                                 bool use_singleton) {
  auto type_of_object = parse_typespec(unquote(type), env);
  auto fe = env->function_env();
  auto st_type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(type_of_object));
  if (st_type_info && st_type_info->is_always_stack_singleton()) {
    use_singleton = true;
    if (call_constructor) {
      throw_compiler_error(
          form, "Stack-singleton types must be created on the stack with stack-no-clear");
    }
  }
  if (type_of_object == TypeSpec("inline-array") || type_of_object == TypeSpec("array")) {
    if (call_constructor) {
      throw_compiler_error(form, "Constructing stack arrays is not yet supported");
    }
    if (use_singleton) {
      throw_compiler_error(form, "Singleton stack arrays are not yet supported");
    }
    bool is_inline = type_of_object == TypeSpec("inline-array");
    auto elt_type = quoted_sym_as_string(pair_car(*rest));
    rest = &pair_cdr(*rest);

    auto count_obj = pair_car(*rest);
    rest = &pair_cdr(*rest);
    // try to get the size as a compile time constant.
    int64_t constant_count = get_constant_integer_or_error(count_obj, env);

    if (constant_count <= 0) {
      throw_compiler_error(form, "Cannot create a stack array with size {}", constant_count);
    }

    if (!rest->is_empty_list()) {
      // got extra arguments
      throw_compiler_error(form, "New array form got more arguments than expected");
    }

    auto ts = is_inline ? m_ts.make_inline_array_typespec(elt_type)
                        : m_ts.make_pointer_typespec(elt_type);
    auto info = m_ts.get_deref_info(ts);
    if (!info.can_deref) {
      throw_compiler_error(form, "Cannot make an {} of {}\n", type_of_object.print(), ts.print());
    }
    auto type_info = m_ts.lookup_type_allow_partial_def(ts.get_single_arg());
    if (!m_ts.lookup_type_allow_partial_def(elt_type)->is_reference()) {
      // not a reference type
      m_ts.lookup_type(elt_type);  // should be fully defined
      int size_in_bytes = info.stride * constant_count;
      auto addr = fe->allocate_aligned_stack_variable(ts, size_in_bytes,
                                                      type_info->get_in_memory_alignment());
      return addr;
    }

    if (is_inline) {
      m_ts.lookup_type(elt_type);  // should be fully defined
    }

    int stride = is_inline ? align(type_info->get_size_in_memory(),
                                   type_info->get_inline_array_stride_alignment())
                           : 4;
    ASSERT(stride == info.stride);

    int size_in_bytes = info.stride * constant_count;
    auto addr = fe->allocate_aligned_stack_variable(ts, size_in_bytes, 16);
    return addr;
  } else {
    auto ti = m_ts.lookup_type(type_of_object);

    if (!ti->is_reference()) {
      throw_compiler_error(form, "Cannot stack allocate the value type {}.",
                           type_of_object.print());
    }
    auto ti_as_struct = dynamic_cast<StructureType*>(ti);
    ASSERT(ti_as_struct);

    if (ti_as_struct->is_dynamic()) {
      throw_compiler_error(form, "Cannot stack allocate the dynamic type {}.",
                           type_of_object.print());
    }
    std::vector<RegVal*> args;
    // allocation
    RegVal* mem;
    if (use_singleton) {
      mem = fe->allocate_stack_singleton(type_of_object, ti->get_size_in_memory(), 16)
                ->to_gpr(form, env);
    } else {
      mem = fe->allocate_aligned_stack_variable(type_of_object, ti->get_size_in_memory(), 16)
                ->to_gpr(form, env);
    }

    if (call_constructor) {
      if (use_singleton) {
        throw_compiler_error(form, "Constructing stack singletons is not yet supported");
      }
      // the new method actual takes a "symbol" according the type system. So we have to cheat it.
      mem->set_type(TypeSpec("symbol"));
      args.push_back(mem);
      // type
      args.push_back(
          compile_get_symbol_value(form, type_of_object.base_type(), env)->to_reg(form, env));
      // the other arguments
      for_each_in_list(*rest, [&](const goos::Object& o) {
        args.push_back(compile_error_guard(o, env)->to_reg(form, env));
      });

      auto new_method = compile_get_method_of_type(form, type_of_object, "new", env);
      auto new_obj = compile_real_function_call(form, new_method, args, env);
      new_obj->set_type(type_of_object);
      return new_obj;
    } else {
      if (ti->get_offset()) {
        throw std::runtime_error("Cannot stack allocate with no constructor for a " +
                                 ti->get_name());
      } else {
        mem->set_type(type_of_object);
        return mem;
      }
    }
  }
}

Val* Compiler::compile_new(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto allocation = quoted_sym_as_string(pair_car(_rest));
  auto rest = &pair_cdr(_rest);

  auto type = pair_car(*rest);
  rest = &pair_cdr(*rest);

  if (allocation == "global" || allocation == "debug" || allocation == "process" ||
      allocation == "process-level-heap" || allocation == "loading-level") {
    // allocate on a named heap
    return compile_heap_new(form, allocation, type, rest, env);
  } else if (allocation == "static") {
    // put in code.
    return compile_static_new(form, type, rest, env);
  } else if (allocation == "stack") {
    return compile_stack_new(form, type, rest, env, true, false);
  } else if (allocation == "stack-no-clear") {
    return compile_stack_new(form, type, rest, env, false, false);
  } else if (allocation == "stack-singleton-no-clear") {
    return compile_stack_new(form, type, rest, env, false, true);
  }

  throw_compiler_error(form, "Unsupported new form");
  return get_none();
}

Val* Compiler::compile_car(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto fe = env->function_env();
  auto pair = compile_error_guard(args.unnamed.at(0), env);
  if (pair->type() != m_ts.make_typespec("object")) {
    typecheck(form, m_ts.make_typespec("pair"), pair->type(), "Type of argument to car");
  }
  auto result = fe->alloc_val<PairEntryVal>(m_ts.make_typespec("object"), pair, true);
  result->mark_as_settable();
  return result;
}

Val* Compiler::compile_cdr(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto fe = env->function_env();
  auto pair = compile_error_guard(args.unnamed.at(0), env);
  if (pair->type() != m_ts.make_typespec("object")) {
    typecheck(form, m_ts.make_typespec("pair"), pair->type(), "Type of argument to cdr");
  }
  auto result = fe->alloc_val<PairEntryVal>(m_ts.make_typespec("pair"), pair, false);
  result->mark_as_settable();
  return result;
}

Val* Compiler::compile_method_of_type(const goos::Object& form,
                                      const goos::Object& rest,
                                      Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {goos::ObjectType::SYMBOL}}, {});

  auto arg = args.unnamed.at(0);
  auto method_name = symbol_string(args.unnamed.at(1));

  // in order to do proper method lookup, we peek at the symbol that the user provided and see if
  // its a type name
  if (arg.is_symbol()) {
    if (m_ts.fully_defined_type_exists(symbol_string(arg))) {
      return compile_get_method_of_type(form, m_ts.make_typespec(symbol_string(arg)), method_name,
                                        env);
    } else if (m_ts.partially_defined_type_exists(symbol_string(arg))) {
      throw_compiler_error(form,
                           "The method form is ambiguous when used on a forward declared type.");
    }
  }

  // if the user didn't provide a symbol, but instead some expression that gives us a type, then use
  // that, and do method lookup as if it was a plain object.
  // this will let you do (method-of-type <something-complicated> inspect) and get the inspect
  // method, with the proper type, from the given type's method table.
  auto user_type = compile_error_guard(arg, env)->to_gpr(form, env);
  if (user_type->type() == TypeSpec("type")) {
    return compile_get_method_of_type(form, TypeSpec("object"), user_type, method_name, env);
  }

  throw_compiler_error(form, "Cannot get method of type {}: the type is invalid", arg.print());
  return get_none();
}

Val* Compiler::compile_method_of_object(const goos::Object& form,
                                        const goos::Object& rest,
                                        Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {goos::ObjectType::SYMBOL}}, {});

  auto arg = args.unnamed.at(0);
  auto method_name = symbol_string(args.unnamed.at(1));

  auto obj = compile_error_guard(arg, env)->to_gpr(form, env);
  return compile_get_method_of_object(form, obj, method_name, env);
}

Val* Compiler::compile_declare_type(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL}, {});

  auto kind = symbol_string(args.unnamed.at(1));
  auto type_name = symbol_string(args.unnamed.at(0));

  m_ts.forward_declare_type_as(type_name, kind);

  auto existing_type = m_symbol_types.find(type_name);
  if (existing_type != m_symbol_types.end() && existing_type->second != TypeSpec("type")) {
    throw_compiler_error(form, "Cannot forward declare {} as a type: it is already a {}", type_name,
                         existing_type->second.print());
  }
  m_symbol_types[type_name] = TypeSpec("type");

  return get_none();
}

Val* Compiler::compile_none(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  return get_none();
}

Val* Compiler::compile_defenum(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  parse_defenum(rest, &m_ts, {});
  return get_none();
}

u64 Compiler::enum_lookup(const goos::Object& form,
                          const EnumType* e,
                          const goos::Object& rest,
                          bool throw_on_error,
                          bool* success) {
  *success = true;
  if (e->is_bitfield()) {
    uint64_t value = 0;
    for_each_in_list(rest, [&](const goos::Object& o) {
      auto kv = e->entries().find(symbol_string(o));
      if (kv == e->entries().end()) {
        if (throw_on_error) {
          throw_compiler_error(form, "The value {} was not found in enum.", o.print());
        } else {
          *success = false;
          return;
        }
      }
      value |= ((u64)1 << (u64)kv->second);
    });

    return value;
  } else {
    uint64_t value = 0;
    bool got = false;
    for_each_in_list(rest, [&](const goos::Object& o) {
      if (got) {
        if (throw_on_error) {
          throw_compiler_error(form, "Invalid enum lookup.");
        } else {
          *success = false;
          return;
        }
      }
      auto kv = e->entries().find(symbol_string(o));
      if (kv == e->entries().end()) {
        if (throw_on_error) {
          throw_compiler_error(form, "The value {} was not found in enum.", o.print());
        } else {
          *success = false;
          return;
        }
      }
      value = kv->second;
      got = true;
    });

    if (!got) {
      if (throw_on_error) {
        throw_compiler_error(form, "Invalid enum lookup.");
      } else {
        *success = false;
      }
    }

    return value;
  }
}

Val* Compiler::compile_enum_lookup(const goos::Object& form,
                                   const EnumType* e,
                                   const goos::Object& rest,
                                   Env* env) {
  bool success;
  u64 value = enum_lookup(form, e, rest, true, &success);
  ASSERT(success);
  auto result = compile_integer(value, env);
  result->set_type(TypeSpec(e->get_name()));
  return result;
}

int Compiler::get_size_for_size_of(const goos::Object& form, const goos::Object& rest) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  auto type_to_look_for = args.unnamed.at(0).as_symbol()->name;

  if (!m_ts.fully_defined_type_exists(type_to_look_for)) {
    throw_compiler_error(
        form, "The type or enum {} given to size-of could not be found, or was not fully defined",
        args.unnamed.at(0).print());
  }

  auto type = m_ts.lookup_type(type_to_look_for);
  auto as_value = dynamic_cast<ValueType*>(type);
  auto as_structure = dynamic_cast<StructureType*>(type);

  if (as_value) {
    return as_value->get_load_size();

  } else if (as_structure) {
    return as_structure->get_size_in_memory();

  } else {
    throw_compiler_error(form, "The type {} does not have a size.", args.unnamed.at(0).print());
    return -1;
  }
}

Val* Compiler::compile_size_of(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_integer(get_size_for_size_of(form, rest), env);
}

Compiler::ConstPropResult Compiler::const_prop_size_of(const goos::Object& form,
                                                       const goos::Object& rest,
                                                       Env* /*env*/) {
  return {goos::Object::make_integer(get_size_for_size_of(form, rest)), false};
}

Val* Compiler::compile_psize_of(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_integer((get_size_for_size_of(form, rest) + 0xf) & ~0xf, env);
}
