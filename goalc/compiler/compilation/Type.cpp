#include "goalc/compiler/Compiler.h"
#include "third-party/fmt/core.h"
#include "common/type_system/deftype.h"

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
 * Given a type and method name (known at compile time), get the method.
 * This can be used for method calls where the type is unknown at run time (non-virtual method call)
 */
RegVal* Compiler::compile_get_method_of_type(const TypeSpec& type,
                                             const std::string& method_name,
                                             Env* env) {
  auto info = m_ts.lookup_method(type.base_type(), method_name);
  info.type = info.type.substitute_for_method_call(type.base_type());
  auto offset_of_method = get_offset_of_method(info.id);

  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto typ = compile_get_symbol_value(type.base_type(), env)->to_gpr(env);
  MemLoadInfo load_info;
  load_info.sign_extend = false;
  load_info.size = POINTER_SIZE;

  auto loc_type = m_ts.make_pointer_typespec(info.type);
  auto loc = fe->alloc_val<MemoryOffsetConstantVal>(loc_type, typ, offset_of_method);
  auto di = m_ts.get_deref_info(loc_type);
  assert(di.can_deref);
  assert(di.mem_deref);
  assert(di.sign_extend == false);
  assert(di.load_size == 4);

  auto deref = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
  return deref->to_reg(env);
}

/*!
 * Given an object, get a method. If at compile time we know it's a basic, we use its runtime
 * type to look up the method at runtime (virtual call).  If we don't know it's a basic, we get the
 * method from the compile-time type. (fixed type non-virtual call)
 */
RegVal* Compiler::compile_get_method_of_object(RegVal* object,
                                               const std::string& method_name,
                                               Env* env) {
  auto& compile_time_type = object->type();
  auto method_info = m_ts.lookup_method(compile_time_type.base_type(), method_name);
  method_info.type = method_info.type.substitute_for_method_call(compile_time_type.base_type());
  auto fe = get_parent_env_of_type<FunctionEnv>(env);

  RegVal* runtime_type = nullptr;
  if (is_basic(compile_time_type)) {
    runtime_type = fe->make_gpr(m_ts.make_typespec("type"));
    MemLoadInfo info;
    info.size = 4;
    info.sign_extend = false;
    info.reg = RegKind::GPR_64;
    env->emit(std::make_unique<IR_LoadConstOffset>(runtime_type, -4, object, info));
  } else {
    // can't look up at runtime
    runtime_type = compile_get_symbol_value(compile_time_type.base_type(), env)->to_gpr(env);
  }

  auto offset_of_method = get_offset_of_method(method_info.id);
  MemLoadInfo load_info;
  load_info.sign_extend = false;
  load_info.size = POINTER_SIZE;

  auto loc_type = m_ts.make_pointer_typespec(method_info.type);
  auto loc = fe->alloc_val<MemoryOffsetConstantVal>(loc_type, runtime_type, offset_of_method);
  auto di = m_ts.get_deref_info(loc_type);
  assert(di.can_deref);
  assert(di.mem_deref);
  assert(di.sign_extend == false);
  assert(di.load_size == 4);

  auto deref = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
  return deref->to_reg(env);
}

Val* Compiler::compile_format_string(const goos::Object& form,
                                     Env* env,
                                     const std::string& fmt_template,
                                     std::vector<RegVal*> args,
                                     const std::string& out_stream) {
  // Add first two format args
  args.insert(args.begin(),
              compile_string(fmt_template, env, get_parent_env_of_type<FunctionEnv>(env)->segment)
                  ->to_gpr(env));
  args.insert(args.begin(), compile_get_sym_obj(out_stream, env)->to_gpr(env));

  // generate code in the method_env
  auto format_function = compile_get_symbol_value("_format", env)->to_gpr(env);
  return compile_real_function_call(form, format_function, args, env);
}

void Compiler::generate_field_description(const goos::Object& form,
                                          StructureType* type,
                                          Env* env,
                                          RegVal* reg,
                                          const Field& f) {
  std::string str_template;
  std::vector<RegVal*> format_args = {};
  if (m_ts.typecheck(m_ts.make_typespec("type"), f.type(), "", false, false)) {
    // type
    return;
  } else if (f.is_array() && !f.is_dynamic()) {
    // Arrays
    str_template += fmt::format("~T{}[{}] @ #x~X~%", f.name(), f.array_size());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else if (f.is_dynamic()) {
    // Dynamic Field
    str_template += fmt::format("~T{}[0] @ #x~X~%", f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else if (f.is_dynamic()) {
    // Structure
    str_template += fmt::format("~T{}: #<{} @ #x~X>~%", f.name(), f.type().print());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else if (m_ts.typecheck(m_ts.make_typespec("basic"), f.type(), "", false, false) ||
             m_ts.typecheck(m_ts.make_typespec("binteger"), f.type(), "", false, false) ||
             m_ts.typecheck(m_ts.make_typespec("pair"), f.type(), "", false, false)) {
    // basic, binteger, pair
    str_template += fmt::format("~T{}: ~A~%", f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else if (m_ts.typecheck(m_ts.make_typespec("integer"), f.type(), "", false, false)) {
    // Integer
    if (f.type().print() == "uint128") {
      str_template += fmt::format("~T{}: <cannot-print>~%", f.name());
    } else {
      str_template += fmt::format("~T{}: ~D~%", f.name());
      format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
    }

  } else if (m_ts.typecheck(m_ts.make_typespec("float"), f.type(), "", false, false)) {
    // Float
    str_template += fmt::format("~T{}: ~f~%", f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else if (m_ts.typecheck(m_ts.make_typespec("pointer"), f.type(), "", false, false)) {
    // Pointers
    str_template += fmt::format("~T{}: #x~X~%", f.name());
    format_args.push_back(get_field_of_structure(type, reg, f.name(), env)->to_gpr(env));
  } else {
    // Otherwise, we havn't implemented it!
    str_template += fmt::format("~T{}: Undefined!~%", f.name());
  }

  compile_format_string(form, env, str_template, format_args);
}

Val* Compiler::generate_inspector_for_type(const goos::Object& form, Env* env, Type* type) {
  auto as_structure_type = dynamic_cast<StructureType*>(type);
  if (!as_structure_type) {  // generate the inspect method
    return get_none();
  }

  StructureType* structured_type = as_structure_type;

  // Create a function environment to hold the code for the inspect method. The name is just for
  // debugging.
  auto method_env =
      std::make_unique<FunctionEnv>(env, "autogenerated-inspect-method-of-" + type->get_name());
  // put the method in the debug segment.
  method_env->set_segment(DEBUG_SEGMENT);

  // Create a register which will hold the input to the inspect method
  auto input = method_env->make_ireg(structured_type->get_name(), emitter::RegKind::GPR);
  // "Constraint" this register to be the register that the function argument is passed in
  IRegConstraint constraint;
  constraint.instr_idx = 0;         // constraint at the start of the function
  constraint.ireg = input->ireg();  // constrain this register
  constraint.desired_register = emitter::gRegInfo.get_arg_reg(0);  // to the first argument
  method_env->constrain(constraint);
  // Inform the compiler that `input`'s value will be written to `rdi` (first arg register)
  method_env->emit(std::make_unique<IR_FunctionStart>(std::vector<RegVal*>{input}));

  RegVal* type_name = nullptr;
  if (dynamic_cast<BasicType*>(structured_type)) {
    type_name = get_field_of_structure(structured_type, input, "type", method_env.get())
                    ->to_gpr(method_env.get());
  } else {
    type_name = compile_get_sym_obj(structured_type->get_name(), method_env.get())
                    ->to_gpr(method_env.get());
  }
  compile_format_string(form, method_env.get(), "[~8x] ~A~%", {input, type_name});

  for (const Field& f : structured_type->fields()) {
    generate_field_description(form, structured_type, method_env.get(), input, f);
  }

  method_env->emit(std::make_unique<IR_Return>(method_env->make_gpr(input->type()), input));

  // add this function to the object file
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto method = fe->alloc_val<LambdaVal>(m_ts.make_typespec("function"));
  method->func = method_env.get();
  auto obj_env_inspect = get_parent_env_of_type<FileEnv>(method_env.get());
  obj_env_inspect->add_function(std::move(method_env));

  // call method-set!
  auto type_obj = compile_get_symbol_value(structured_type->get_name(), env)->to_gpr(env);
  auto id_val = compile_integer(m_ts.lookup_method(structured_type->get_name(), "inspect").id, env)
                    ->to_gpr(env);
  auto method_set_val = compile_get_symbol_value("method-set!", env)->to_gpr(env);
  return compile_real_function_call(form, method_set_val, {type_obj, id_val, method->to_gpr(env)},
                                    env);
}

/*!
 * Compile a (deftype ... form)
 */
Val* Compiler::compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  // parse the type definition and add to the type system
  auto result = parse_deftype(rest, &m_ts);

  // look up the type name
  auto kv = m_symbol_types.find(result.type.base_type());
  if (kv != m_symbol_types.end() && kv->second.base_type() != "type") {
    // we already have something that's not a type with the same name, this is bad.
    fmt::print("[Warning] deftype will redefined {} from {} to a type.\n", result.type.base_type(),
               kv->second.print());
  }
  // remember that this is a type
  m_symbol_types[result.type.base_type()] = m_ts.make_typespec("type");

  if (result.create_runtime_type) {
    // get the new method of type object. this is new_type in kscheme.cpp
    auto new_type_method = compile_get_method_of_type(m_ts.make_typespec("type"), "new", env);
    // call (new 'type 'type-name parent-type flags)
    auto new_type_symbol = compile_get_sym_obj(result.type.base_type(), env)->to_gpr(env);
    auto parent_type = compile_get_symbol_value(result.type_info->get_parent(), env)->to_gpr(env);
    auto flags_int = compile_integer(result.flags.flag, env)->to_gpr(env);
    compile_real_function_call(form, new_type_method, {new_type_symbol, parent_type, flags_int},
                               env);
  }

  // Auto-generate (inspect) method
  generate_inspector_for_type(form, env, result.type_info);

  // return none, making the value of (deftype..) unusable
  return get_none();
}

/*!
 * Compile a (defmethod ...) form
 */
Val* Compiler::compile_defmethod(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto* rest = &_rest;

  auto& method_name = pair_car(*rest);
  rest = &pair_cdr(*rest);
  auto& type_name = pair_car(*rest);
  rest = &pair_cdr(*rest);
  auto& arg_list = pair_car(*rest);
  auto body = &pair_cdr(*rest);

  if (!method_name.is_symbol()) {
    throw_compile_error(form, "method name must be a symbol, got " + method_name.print());
  }
  if (!type_name.is_symbol()) {
    throw_compile_error(form, "method type must be a symbol, got " + method_name.print());
  }

  auto place = fe->alloc_val<LambdaVal>(get_none()->type());
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
      va_check(o, param_args, {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL}, {});

      GoalArg parm;
      parm.name = symbol_string(param_args.unnamed.at(0));
      parm.type = parse_typespec(param_args.unnamed.at(1));
      // before substituting _type_
      lambda_ts.add_arg(parm.type);

      // replace _type_ as needed for inside this function.
      parm.type = parm.type.substitute_for_method_call(symbol_string(type_name));
      lambda.params.push_back(parm);
    }
  });
  assert(lambda.params.size() == lambda_ts.arg_count());
  // todo, verify argument list types (check that first arg is _type_ for methods that aren't "new")
  lambda.debug_name = fmt::format("(method {} {})", method_name.print(), type_name.print());

  // skip docstring
  if (body->as_pair()->car.is_string() && !body->as_pair()->cdr.is_empty_list()) {
    body = &pair_cdr(*body);
  }

  lambda.body = *body;
  place->func = nullptr;

  auto new_func_env = std::make_unique<FunctionEnv>(env, lambda.debug_name);
  new_func_env->set_segment(MAIN_SEGMENT);  // todo, how do we set debug?
  new_func_env->method_of_type_name = symbol_string(type_name);

  // set up arguments
  if (lambda.params.size() > 8) {
    throw_compile_error(form, "Methods cannot have more than 8 arguments");
  }
  std::vector<RegVal*> args_for_coloring;
  for (u32 i = 0; i < lambda.params.size(); i++) {
    IRegConstraint constr;
    constr.instr_idx = 0;  // constraint at function start
    auto ireg = new_func_env->make_ireg(lambda.params.at(i).type, emitter::RegKind::GPR);
    constr.ireg = ireg->ireg();
    constr.desired_register = emitter::gRegInfo.get_arg_reg(i);
    new_func_env->params[lambda.params.at(i).name] = ireg;
    new_func_env->constrain(constr);
    args_for_coloring.push_back(ireg);
  }

  place->func = new_func_env.get();

  // nasty function block env setup
  auto return_reg = new_func_env->make_ireg(get_none()->type(), emitter::RegKind::GPR);
  auto func_block_env = new_func_env->alloc_env<BlockEnv>(new_func_env.get(), "#f");
  func_block_env->return_value = return_reg;
  func_block_env->end_label = Label(new_func_env.get());
  func_block_env->emit(std::make_unique<IR_FunctionStart>(args_for_coloring));

  // compile the function!
  Val* result = nullptr;
  bool first_thing = true;
  for_each_in_list(lambda.body, [&](const goos::Object& o) {
    result = compile_error_guard(o, func_block_env);
    if (first_thing) {
      first_thing = false;
      // you could probably cheat and do a (begin (blorp) (declare ...)) to get around this.
      new_func_env->settings.is_set = true;
    }
  });

  if (result && !dynamic_cast<None*>(result)) {
    auto final_result = result->to_gpr(new_func_env.get());
    new_func_env->emit(std::make_unique<IR_Return>(return_reg, final_result));
    lambda_ts.add_arg(final_result->type());
  } else {
    lambda_ts.add_arg(m_ts.make_typespec("none"));
  }
  func_block_env->end_label.idx = new_func_env->code().size();
  new_func_env->emit(std::make_unique<IR_Null>());
  new_func_env->finish();

  auto obj_env = get_parent_env_of_type<FileEnv>(new_func_env.get());
  assert(obj_env);
  if (new_func_env->settings.save_code) {
    obj_env->add_function(std::move(new_func_env));
  }
  place->set_type(lambda_ts);

  auto info =
      m_ts.add_method(symbol_string(type_name), symbol_string(method_name), lambda_ts, false);
  auto type_obj = compile_get_symbol_value(symbol_string(type_name), env)->to_gpr(env);
  auto id_val = compile_integer(info.id, env)->to_gpr(env);
  auto method_val = place->to_gpr(env);
  auto method_set_val = compile_get_symbol_value("method-set!", env)->to_gpr(env);
  return compile_real_function_call(form, method_set_val, {type_obj, id_val, method_val}, env);
}

/*!
 * Given a type, object, and field name, get the value of the field.
 */
Val* Compiler::get_field_of_structure(const StructureType* type,
                                      Val* object,
                                      const std::string& field_name,
                                      Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  Val* result = nullptr;
  int offset = -type->get_offset();
  auto field = m_ts.lookup_field_info(type->get_name(), field_name);
  if (field.needs_deref) {
    TypeSpec loc_type = m_ts.make_pointer_typespec(field.type);
    auto loc =
        fe->alloc_val<MemoryOffsetConstantVal>(loc_type, object, field.field.offset() + offset);
    auto di = m_ts.get_deref_info(loc_type);
    assert(di.can_deref);
    assert(di.mem_deref);
    result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
    result->mark_as_settable();
  } else {
    result =
        fe->alloc_val<MemoryOffsetConstantVal>(field.type, object, field.field.offset() + offset);
    result->mark_as_settable();
  }
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
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  if (_rest.is_empty_list()) {
    throw_compile_error(form, "-> must get at least one argument");
  }

  auto& first_arg = pair_car(_rest);
  auto rest = &pair_cdr(_rest);

  // eval the first thing
  auto result = compile_error_guard(first_arg, env);

  if (rest->is_empty_list()) {
    // one argument, do a pointer deref
    auto deref_info = m_ts.get_deref_info(result->type());
    if (!deref_info.can_deref) {
      throw_compile_error(form, "Cannot dereference a " + result->type().print());
    }

    if (deref_info.mem_deref) {
      result =
          fe->alloc_val<MemoryDerefVal>(deref_info.result_type, result, MemLoadInfo(deref_info));
      result->mark_as_settable();
    } else {
      assert(false);
    }
    return result;
  }

  // compound, is field access/nested access
  while (!rest->is_empty_list()) {
    auto field_obj = pair_car(*rest);
    rest = &pair_cdr(*rest);
    auto type_info = m_ts.lookup_type(result->type());

    // attempt to treat it as a field. May not succeed if we're actually an array.
    if (field_obj.is_symbol()) {
      auto field_name = symbol_string(field_obj);
      auto struct_type = dynamic_cast<StructureType*>(type_info);

      if (struct_type) {
        result = get_field_of_structure(struct_type, result, field_name, env);
        continue;
      }

      // todo try bitfield
    }

    auto index_value = compile_error_guard(field_obj, env)->to_gpr(env);
    if (!is_integer(index_value->type())) {
      throw_compile_error(form, "cannot use -> with " + field_obj.print());
    }

    if (result->type().base_type() == "inline-array") {
      assert(false);
    } else if (result->type().base_type() == "pointer") {
      auto di = m_ts.get_deref_info(result->type());
      auto base_type = di.result_type;
      assert(di.mem_deref);
      assert(di.can_deref);
      auto offset = compile_integer(di.stride, env)->to_gpr(env);
      // todo, check for integer and avoid runtime multiply
      env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::IMUL_32, offset, index_value));
      auto loc = fe->alloc_val<MemoryOffsetVal>(result->type(), result, offset);
      result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
      result->mark_as_settable();
    } else {
      throw_compile_error(form, "can't access array of type " + result->type().print());
    }
  }
  return result;
}

/*!
 * Compile the (& x) form.
 */
Val* Compiler::compile_addr_of(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto loc = compile_error_guard(args.unnamed.at(0), env);
  auto as_mem_deref = dynamic_cast<MemoryDerefVal*>(loc);
  if (!as_mem_deref) {
    throw_compile_error(form, "Cannot take the address of this");
  }
  return as_mem_deref->base;
}

/*!
 * Compile the (the-as x y) form. Like a reinterpret cast.
 * Will always produce a alias (so setting the result of this affects the base).
 */
Val* Compiler::compile_the_as(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto desired_ts = parse_typespec(args.unnamed.at(0));
  auto base = compile_error_guard(args.unnamed.at(1), env);
  auto result = get_parent_env_of_type<FunctionEnv>(env)->alloc_val<AliasVal>(desired_ts, base);
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
  auto desired_ts = parse_typespec(args.unnamed.at(0));
  auto base = compile_error_guard(args.unnamed.at(1), env);

  if (is_number(base->type())) {
    if (m_ts.typecheck(m_ts.make_typespec("binteger"), desired_ts, "", false, false)) {
      return number_to_binteger(base, env);
    }

    if (m_ts.typecheck(m_ts.make_typespec("integer"), desired_ts, "", false, false)) {
      auto result = number_to_integer(base, env);
      result->set_type(desired_ts);
      return result;
    }

    if (m_ts.typecheck(m_ts.make_typespec("float"), desired_ts, "", false, false)) {
      return number_to_float(base, env);
    }
  }

  auto result = get_parent_env_of_type<FunctionEnv>(env)->alloc_val<AliasVal>(desired_ts, base);
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
  fmt::print("[TYPE] {}\n", compile(args.unnamed.at(0), env)->type().print());
  return get_none();
}

Val* Compiler::compile_new(const goos::Object& form, const goos::Object& _rest, Env* env) {
  // todo - support compound types.

  auto allocation = quoted_sym_as_string(pair_car(_rest));
  auto rest = &pair_cdr(_rest);

  // auto type_of_obj = get_base_typespec(quoted_sym_as_string(pair_car(rest)));
  auto type_as_string = quoted_sym_as_string(pair_car(*rest));
  rest = &pair_cdr(*rest);

  if (allocation == "global" || allocation == "debug") {
    // allocate on a named heap

    if (type_as_string == "inline-array" || type_as_string == "array") {
      bool is_inline = type_as_string == "inline-array";
      auto elt_type = quoted_sym_as_string(pair_car(*rest));
      rest = &pair_cdr(*rest);

      auto count_obj = pair_car(*rest);
      rest = &pair_cdr(*rest);
      // try to get the size as a compile time constant.
      int64_t constant_count = 0;
      bool is_constant_size = try_getting_constant_integer(count_obj, &constant_count, env);

      if (!rest->is_empty_list()) {
        // got extra arguments
        throw_compile_error(form, "new array form got more arguments than expected");
      }

      auto ts = is_inline ? m_ts.make_inline_array_typespec(elt_type)
                          : m_ts.make_pointer_typespec(elt_type);
      auto info = m_ts.get_deref_info(ts);
      if (!info.can_deref) {
        throw_compile_error(form,
                            fmt::format("Cannot make an {} of {}\n", type_as_string, ts.print()));
      }

      auto malloc_func = compile_get_symbol_value("malloc", env)->to_reg(env);
      std::vector<RegVal*> args;
      args.push_back(compile_get_sym_obj(allocation, env)->to_reg(env));

      if (is_constant_size) {
        auto array_size = constant_count * info.stride;
        args.push_back(compile_integer(array_size, env)->to_reg(env));
      } else {
        auto array_size = compile_integer(info.stride, env)->to_reg(env);
        env->emit(
            std::make_unique<IR_IntegerMath>(IntegerMathKind::IMUL_32, array_size,
                                             compile_error_guard(count_obj, env)->to_gpr(env)));
        args.push_back(array_size);
      }

      auto array = compile_real_function_call(form, malloc_func, args, env);
      array->set_type(ts);
      return array;
    } else {
      auto type_of_obj = m_ts.make_typespec(type_as_string);
      std::vector<RegVal*> args;
      // allocation
      args.push_back(compile_get_sym_obj(allocation, env)->to_reg(env));
      // type
      args.push_back(compile_get_symbol_value(type_of_obj.base_type(), env)->to_reg(env));
      // the other arguments
      for_each_in_list(*rest, [&](const goos::Object& o) {
        args.push_back(compile_error_guard(o, env)->to_reg(env));
      });

      auto new_method = compile_get_method_of_type(type_of_obj, "new", env);
      auto new_obj = compile_real_function_call(form, new_method, args, env);
      new_obj->set_type(type_of_obj);
      return new_obj;
    }
  } else if (allocation == "static") {
    auto type_of_object = m_ts.make_typespec(type_as_string);
    if (is_structure(type_of_object)) {
      return compile_new_static_structure_or_basic(form, type_of_object, *rest, env);
    }
  }

  throw_compile_error(form, "unsupported new form");
  return get_none();
}

Val* Compiler::compile_car(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
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
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto pair = compile_error_guard(args.unnamed.at(0), env);
  if (pair->type() != m_ts.make_typespec("object")) {
    typecheck(form, m_ts.make_typespec("pair"), pair->type(), "Type of argument to cdr");
  }
  auto result = fe->alloc_val<PairEntryVal>(m_ts.make_typespec("object"), pair, false);
  result->mark_as_settable();
  return result;
}

// todo, consider splitting into method-of-object and method-of-type?
Val* Compiler::compile_method(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {goos::ObjectType::SYMBOL}}, {});

  auto arg = args.unnamed.at(0);
  auto method_name = symbol_string(args.unnamed.at(1));

  if (arg.is_symbol()) {
    if (m_ts.fully_defined_type_exists(symbol_string(arg))) {
      return compile_get_method_of_type(m_ts.make_typespec(symbol_string(arg)), method_name, env);
    } else if (m_ts.partially_defined_type_exists(symbol_string(arg))) {
      throw_compile_error(form,
                          "The method form is ambiguous when used on a forward declared type.");
    }
  }

  auto obj = compile_error_guard(arg, env)->to_gpr(env);
  return compile_get_method_of_object(obj, method_name, env);
}

Val* Compiler::compile_declare_type(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL}, {});

  auto kind = symbol_string(args.unnamed.at(1));
  auto type_name = symbol_string(args.unnamed.at(0));

  if (kind == "basic") {
    m_ts.forward_declare_type_as_basic(type_name);
  } else if (kind == "structure") {
    m_ts.forward_declare_type_as_structure(type_name);
  } else {
    throw_compile_error(form, "Invalid declare-type form");
  }

  return get_none();
}

Val* Compiler::compile_none(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  return get_none();
}