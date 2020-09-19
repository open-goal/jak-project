#include "goalc/compiler/Compiler.h"
#include "common/type_system/deftype.h"

namespace {
int get_offset_of_method(int id) {
  // todo - something that looks at the type system?
  // this will need changing if the layout of type ever changes.
  return 16 + 4 * id;
}
}  // namespace

RegVal* Compiler::compile_get_method_of_type(const TypeSpec& type,
                                             const std::string& method_name,
                                             Env* env) {
  auto info = m_ts.lookup_method(type.base_type(), method_name);
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

RegVal* Compiler::compile_get_method_of_object(RegVal* object,
                                               const std::string& method_name,
                                               Env* env) {
  auto& compile_time_type = object->type();
  auto method_info = m_ts.lookup_method(compile_time_type.base_type(), method_name);
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

Val* Compiler::compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  auto result = parse_deftype(rest, &m_ts);

  auto kv = m_symbol_types.find(result.type.base_type());
  if (kv != m_symbol_types.end() && kv->second.base_type() != "type") {
    fmt::print("[Warning] deftype will redefined {} from {} to a type.\n", result.type.base_type(),
               kv->second.print());
  }
  m_symbol_types[result.type.base_type()] = m_ts.make_typespec("type");

  auto new_type_method = compile_get_method_of_type(m_ts.make_typespec("type"), "new", env);
  auto new_type_symbol = compile_get_sym_obj(result.type.base_type(), env)->to_gpr(env);
  auto parent_type = compile_get_symbol_value(result.type_info->get_parent(), env)->to_gpr(env);
  auto flags_int = compile_integer(result.flags.flag, env)->to_gpr(env);
  return compile_real_function_call(form, new_type_method,
                                    {new_type_symbol, parent_type, flags_int}, env);
}

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

  // parse the argument list.
  for_each_in_list(arg_list, [&](const goos::Object& o) {
    if (o.is_symbol()) {
      // if it has no type, assume object.
      lambda.params.push_back({symbol_string(o), m_ts.make_typespec("object")});
      lambda_ts.add_arg(m_ts.make_typespec("object"));
    } else {
      auto param_args = get_va(o, o);
      va_check(o, param_args, {goos::ObjectType::SYMBOL, goos::ObjectType::SYMBOL}, {});

      GoalArg parm;
      parm.name = symbol_string(param_args.unnamed.at(0));
      parm.type = parse_typespec(param_args.unnamed.at(1));
      lambda_ts.add_arg(parm.type);
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
  assert(lambda.params.size() < 8);  // todo graceful error
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
  if (result) {
    auto final_result = result->to_gpr(new_func_env.get());
    new_func_env->emit(std::make_unique<IR_Return>(return_reg, final_result));
    printf("return type is %s from %s from %s\n", final_result->type().print().c_str(),
           final_result->print().c_str(), result->print().c_str());
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

  auto info = m_ts.add_method(symbol_string(type_name), symbol_string(method_name), lambda_ts);
  auto type_obj = compile_get_symbol_value(symbol_string(type_name), env)->to_gpr(env);
  auto id_val = compile_integer(info.id, env)->to_gpr(env);
  auto method_val = place->to_gpr(env);
  auto method_set_val = compile_get_symbol_value("method-set!", env)->to_gpr(env);
  return compile_real_function_call(form, method_set_val, {type_obj, id_val, method_val}, env);
}

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
        int offset = -struct_type->get_offset();
        auto field = m_ts.lookup_field_info(type_info->get_name(), field_name);
        if (field.needs_deref) {
          TypeSpec loc_type = m_ts.make_pointer_typespec(field.type);
          auto loc = fe->alloc_val<MemoryOffsetConstantVal>(loc_type, result,
                                                            field.field.offset() + offset);
          auto di = m_ts.get_deref_info(loc_type);
          assert(di.can_deref);
          assert(di.mem_deref);
          result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
        } else {
          result = fe->alloc_val<MemoryOffsetConstantVal>(field.type, result,
                                                          field.field.offset() + offset);
          // assert(false);
        }
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
      env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::IMUL_32, offset, index_value));
      auto loc = fe->alloc_val<MemoryOffsetVal>(result->type(), result, offset);
      result = fe->alloc_val<MemoryDerefVal>(di.result_type, loc, MemLoadInfo(di));
    } else {
      throw_compile_error(form, "can't access array of type " + result->type().print());
    }
  }
  return result;
}

Val* Compiler::compile_the_as(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto desired_ts = parse_typespec(args.unnamed.at(0));
  auto base = compile_error_guard(args.unnamed.at(1), env);
  return get_parent_env_of_type<FunctionEnv>(env)->alloc_val<AliasVal>(desired_ts, base);
}

Val* Compiler::compile_the(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto desired_ts = parse_typespec(args.unnamed.at(0));
  auto base = compile_error_guard(args.unnamed.at(1), env);

  if (is_number(base->type())) {
    if (m_ts.typecheck(m_ts.make_typespec("binteger"), desired_ts, "", false, false)) {
      throw std::runtime_error("the convert to binteger not yet supported");
    }

    if (m_ts.typecheck(m_ts.make_typespec("integer"), desired_ts, "", false, false)) {
      throw std::runtime_error("the convert to integer not yet supported");
    }

    if (m_ts.typecheck(m_ts.make_typespec("float"), desired_ts, "", false, false)) {
      throw std::runtime_error("the convert to float not yet supported");
    }
  }

  return get_parent_env_of_type<FunctionEnv>(env)->alloc_val<AliasVal>(desired_ts, base);
}

Val* Compiler::compile_print_type(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  fmt::print("[TYPE] {}\n", compile(args.unnamed.at(0), env)->type().print());
  return get_none();
}

Val* Compiler::compile_new(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto allocation = quoted_sym_as_string(pair_car(_rest));
  auto rest = &pair_cdr(_rest);

  // auto type_of_obj = get_base_typespec(quoted_sym_as_string(pair_car(rest)));
  auto type_as_string = quoted_sym_as_string(pair_car(*rest));
  rest = &pair_cdr(*rest);

  if (allocation == "global" || allocation == "debug") {
    if (type_as_string == "inline-array") {
      assert(false);
    } else if (type_as_string == "array") {
      assert(false);
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
    assert(false);
  }

  throw_compile_error(form, "unsupported new form");
  return get_none();
}

Val* Compiler::compile_car(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<PairEntryVal>(m_ts.make_typespec("object"),
                                     compile_error_guard(args.unnamed.at(0), env), true);
}

Val* Compiler::compile_cdr(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<PairEntryVal>(m_ts.make_typespec("object"),
                                     compile_error_guard(args.unnamed.at(0), env), false);
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
    }
  }

  auto obj = compile_error_guard(arg, env)->to_gpr(env);
  return compile_get_method_of_object(obj, method_name, env);
}
