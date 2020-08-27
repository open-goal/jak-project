/*!
 * @file GoalFunctionForms.cpp
 * Utilities related to functions.
 */

#include "Goal.h"
#include "GoalLambda.h"
#include "util.h"
#include "logger/Logger.h"

/*!
 * Compile "inline", a form which makes a function call inline if possible, and errors otherwise.
 */
std::shared_ptr<Place> Goal::compile_inline(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  (void)env;
  auto args = goos.get_uneval_args(form, rest, 2);
  if (args.has_rest || args.unnamed_args.size() < 1 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid inline");
  }

  auto function_name = args.unnamed_args.front();
  if (function_name.type != SYMBOL) {
    throw_compile_error(form, "invalid inline, must give a symbol");
  }

  auto kv = inlineable_functions.find(function_name.as_symbol());
  if (kv == inlineable_functions.end()) {
    throw_compile_error(form, "couldn't find definition to inline");
  }

  if (kv->second->func && !kv->second->func->settings.allow_inline) {
    throw_compile_error(form, "not allowed to inline");
  }

  return kv->second;
}

/*!
 * Get the preference to inline in the given environment - return false if no preference set.
 */
static bool get_inline_preference(std::shared_ptr<GoalEnv> env) {
  auto inline_env = get_parent_env_of_type<WithInlineEnv>(env);
  if (inline_env) {
    return inline_env->inline_preference;
  } else {
    return false;
  }
}

/*!
 * Compile a real x86 function call helper,
 */
std::shared_ptr<Place> Goal::compile_real_function_call(const Object& form,
                                                        std::shared_ptr<Place> function,
                                                        std::vector<std::shared_ptr<Place>> args,
                                                        std::shared_ptr<GoalEnv> env) {
  TypeSpec return_ts;
  if (function->type.ts_args.empty()) {
    // if the type system doesn't know what the function will return, just make it object.
    // the user is responsible for getting this right.
    return_ts = get_base_typespec("object");
    //    gLogger.log(MSG_WARN, "[Warning] Function call could not determine return type: %s\n",
    //    const_cast<Object&>(form).print().c_str());
    // todo, should this be a warning?  not a great thing if we don't know what a function will
    // return?
  } else {
    return_ts = function->type.ts_args.front();
  }

  auto return_reg = env->alloc_reg(return_ts);

  for (auto& arg : args) {
    // note: this has to be done in here, because we might want to const prop across lexical envs.
    arg = resolve_to_gpr(arg, env);
  }

  // check arg count:
  if (!function->type.ts_args.empty()) {
    if (function->type.ts_args.size() - 1 != args.size()) {
      throw_compile_error(form, "invalid number of arguments to function call: got " +
                                    std::to_string(args.size()) + " and expected " +
                                    std::to_string(function->type.ts_args.size() - 1));
    }
    for (uint32_t i = 0; i < args.size(); i++) {
      typecheck_base_only(form, function->type.ts_args.at(i + 1), args.at(i)->type,
                          "function argument");
    }
  }

  // set args (introducing a move here makes coloring more likely to be possible)
  std::vector<std::shared_ptr<Place>> arg_outs;
  for (auto& arg : args) {
    arg_outs.push_back(env->alloc_reg(arg->type));
    env->emit(make_unique<IR_Set>(arg_outs.back(), arg));
  }

  env->emit(
      make_unique<IR_FunctionCall>(env->alloc_reg(function->type), function, return_reg, arg_outs));
  return return_reg;
}

/*!
 * Compile a function or method call.  This includes real function calls, inline function calls,
 * automatic inline function calls, immediate application of lambda, method calls of basics, and
 * method calls of structures.
 */
std::shared_ptr<Place> Goal::compile_function_or_method_call(const Object& form,
                                                             std::shared_ptr<GoalEnv> env) {
  Object f = form;

  // get args in a list
  auto args = goos.get_uneval_args_no_rest(form, form, 9);  // 8 args + function max

  auto uneval_head = args.unnamed_args.front();
  auto head = get_none();  // will hold function object to call

  // determine if this call should be automatically inlined.
  // this logic will not trigger for a manually inlined call [using the (inline func) form]
  bool auto_inline = false;
  if (uneval_head.type == SYMBOL) {
    // we can only auto-inline the function if its name is explicit.
    // look it up:
    auto kv = inlineable_functions.find(as_symbol_obj(uneval_head));
    if (kv != inlineable_functions.end()) {
      // it's inlinable.  However, we do not always inline an inlinable function by default
      if (kv->second->func ==
              nullptr ||  // only-inline, we must inline it as there is no code generated for it
          kv->second->func->settings
              .inline_by_default ||  // inline when possible, so we should inline
          (kv->second->func->settings.allow_inline &&
           get_inline_preference(env))) {  // inline is allowed, and we prefer it locally
        auto_inline = true;
        head = kv->second;
      }
    }
  }

  bool is_method_call = false;
  if (!auto_inline) {
    // if auto-inlining failed, we must get the thing to call in a different way.
    if (uneval_head.type == SYMBOL) {
      if (is_local_symbol(uneval_head, env) ||
          symbol_types.find(as_symbol_obj(uneval_head)) != symbol_types.end()) {
        // the local environment (mlets, lexicals, constants, globals) defines this symbol.
        // this will "win" over a method name lookup, so we should compile as normal
        head = compile_error_guard(args.unnamed_args.front(), env);
      } else {
        // we don't think compiling the head give us a function, so it's either a method or an error
        is_method_call = true;
      }
    } else {
      // the head is some expression. Could be something like (inline my-func) or (-> obj
      // func-ptr-field) in either case, compile it - and it can't be a method call.
      head = compile_error_guard(args.unnamed_args.front(), env);
    }
  }

  if (!is_method_call) {
    // typecheck that we got a function
    auto f_type = get_base_typespec("function");
    if (!head->type.typecheck_base_only(f_type, types)) {
      throw_compile_error(
          form, "function call head does not evaluate to a function! " + head->type.print());
    }
  }

  // compile arguments
  std::vector<std::shared_ptr<Place>> eval_args;
  for (uint32_t i = 1; i < args.unnamed_args.size(); i++) {
    auto intermediate = compile_error_guard(args.unnamed_args.at(i), env);
    eval_args.push_back(resolve_to_gpr_or_xmm(intermediate, env));
  }

  // see if its an "immediate" application. This happens in three cases:
  // 1). the user directly puts a (lambda ...) form in the head (like with a (let) macro)
  // 2). the user used a (inline my-func) to grab the LambdaPlace of the function.
  // 3). the auto-inlining above looked up the LambdaPlace of an inlinable_function.

  // note that an inlineable function looked up by symbol or other way WILL NOT cast to a
  // LambdaPlace! so this cast will only succeed if the auto-inliner succeeded, or the user has
  // passed use explicitly a lambda either with the lambda form, or with the (inline ...) form.
  std::shared_ptr<LambdaPlace> head_as_lambda = nullptr;
  if (!is_method_call) {
    head_as_lambda = std::dynamic_pointer_cast<LambdaPlace>(head);
  }

  if (head_as_lambda) {
    // inline the function!

    // check args are ok
    if (head_as_lambda->lambda.params.size() != eval_args.size()) {
      throw_compile_error(form, "invalid argument count");
    }

    // construct a lexical environment
    auto lexical_env = std::make_shared<LexicalEnv>();
    lexical_env->parent = env;

    std::shared_ptr<GoalEnv> compile_env = lexical_env;

    // if needed create a label env.
    // we don't want a separate label env with lets, but we do in other cases.
    if (auto_inline) {
      // TODO - this misses the case of (inline func)!
      compile_env = std::make_shared<LabelEnv>(lexical_env);
    }

    // check arg types
    if (!head->type.ts_args.empty()) {
      if (head->type.ts_args.size() - 1 != eval_args.size()) {
        throw_compile_error(form, "invalid number of arguments to function call (inline)");
      }
      for (uint32_t i = 0; i < eval_args.size(); i++) {
        typecheck_base_only(form, head->type.ts_args.at(i + 1), eval_args.at(i)->type,
                            "function (inline) argument");
      }
    }

    // copy args...
    for (uint32_t i = 0; i < eval_args.size(); i++) {
      auto copy = env->alloc_reg(eval_args.at(i)->type);
      env->emit(make_unique<IR_Set>(copy, eval_args.at(i)));
      lexical_env->vars[head_as_lambda->lambda.params.at(i).name] = copy;
    }

    // compile inline!
    bool first_thing = true;
    std::shared_ptr<Place> result = get_none();
    for_each_in_list(head_as_lambda->lambda.body, [&](Object o) {
      result = compile_error_guard(o, compile_env);
      if (first_thing) {
        first_thing = false;
        lexical_env->settings.is_set = true;
      }
    });

    // this doesn't require a return type.
    return result;
  } else {
    // not an inline call
    if (is_method_call) {
      // determine the method to call by looking at the type of first argument
      if (eval_args.empty()) {
        throw_compile_error(form, "0 argument method call is impossible to figure out");
      }
      head = compile_get_method_of_object(eval_args.front(), symbol_string(uneval_head), env);
    }

    // convert the head to a GPR
    auto head_as_gpr = std::dynamic_pointer_cast<GprPlace>(resolve_to_gpr(head, env));
    if (head_as_gpr) {
      return compile_real_function_call(form, head_as_gpr, eval_args, env);
    } else {
      throw_compile_error(form, "can't figure out this function call!");
    }
  }

  throw_compile_error(form, "call_function_or_method unreachable");
  return get_none();
}

std::shared_ptr<Place> Goal::compile_defmethod(const Object& form,
                                               Object rest,
                                               std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 3);
  if (!args.named_args.empty() || args.unnamed_args.size() != 3) {
    throw_compile_error(form, "invalid defmethod");
  }

  TypeSpec lambda_ts = get_base_typespec("function");
  // temp return typespec
  lambda_ts.ts_args.push_back(get_base_typespec("none"));
  // temp for now
  auto place = std::make_shared<LambdaPlace>(get_none()->type);

  // Build Lambda Object
  GoalLambda& lambda = place->lambda;
  // todo get the correct function type

  auto arg_name = args.unnamed_args.at(0);
  auto arg_type = args.unnamed_args.at(1);

  if (arg_name.type != SYMBOL) {
    throw_compile_error(form, "defmethod method name must be a symbol");
  }

  if (arg_type.type != SYMBOL) {
    throw_compile_error(form, "defmethod type name must be a symbol");
  }

  auto body = args.unnamed_args.at(2);
  if (body.type == EMPTY_LIST) {
    throw_compile_error(form, "defmethod had an empty body!");
  }

  for_each_in_list(body, [&](Object o) {
    if (o.type == SYMBOL) {
      lambda.params.emplace_back(o.as_symbol()->name, get_base_typespec("object"));
      lambda_ts.ts_args.push_back(get_base_typespec("object"));
    } else {
      auto param_args = goos.get_uneval_args(o, o, 3);
      if (param_args.unnamed_args.size() >= 3 || param_args.unnamed_args.size() < 1 ||
          param_args.has_rest || !param_args.named_args.empty()) {
        throw_compile_error(o, "invalid lambda parameter");
      }

      GoalLambdaParam parm;
      if (param_args.unnamed_args.front().type != SYMBOL) {
        throw_compile_error(o, "invalid lambda parameter");
      }
      parm.name = param_args.unnamed_args.front().as_symbol()->name;

      if (param_args.unnamed_args.size() >= 2) {
        parm.type = TypeSpec(param_args.unnamed_args[1], types);  // todo improve
      } else {
        parm.type = get_base_typespec("object");
      }

      // printf("set arg type to %s\n", parm.type.print().c_str());

      if (param_args.unnamed_args.size() >= 3) {
        parm.default_value = param_args.unnamed_args[2];
        parm.has_default = true;
      }

      lambda.params.push_back(parm);
      lambda_ts.ts_args.push_back(parm.type);
    }
  });

  assert(lambda.params.size() + 1 == lambda_ts.ts_args.size());
  if (!args.has_rest) {
    throw_compile_error(form, "lambda must have a body");
  }

  // skip docstring
  if (args.rest.as_pair()->car.type == STRING && args.rest.as_pair()->cdr.type != EMPTY_LIST) {
    args.rest = args.rest.as_pair()->cdr;
  }

  lambda.body = args.rest;
  place->func = nullptr;

  auto new_func_env = std::make_shared<FunctionEnv>(place->print());
  new_func_env->method_of_type_name = arg_type.as_symbol()->name;
  new_func_env->parent = env;
  new_func_env->segment = MAIN_SEGMENT;  // todo not this
  // set up arguments
  assert(lambda.params.size() < 8);  // todo, this should be more graceful
  for (uint32_t i = 0; i < lambda.params.size(); i++) {
    RegConstraint constr;
    constr.instr_id = 0;
    constr.var_id = new_func_env->vars.size();
    constr.ass.kind = REGISTER;
    constr.ass.reg_id = ARG_REGS[i];
    new_func_env->params[lambda.params.at(i).name] =
        new_func_env->alloc_reg(lambda.params.at(i).type);
    new_func_env->constrain_reg(constr);
  }

  place->func = new_func_env;
  new_func_env->emit(make_unique<IR_FunctionBegin>(place));
  auto return_reg = new_func_env->alloc_reg(get_none()->type);

  auto func_block_env = std::make_shared<BlockEnv>(new_func_env, "#f");
  func_block_env->return_value = return_reg;
  auto label = std::make_shared<Label>(new_func_env.get());
  func_block_env->end_label = label;

  // auto return_ir = std::make_shared<IR_Return>(compile_error_guard(body_with_begin,
  // new_func_env), return_reg);
  std::shared_ptr<Place> result = get_none();
  bool first_thing = true;
  for_each_in_list(lambda.body, [&](Object o) {
    result = compile_error_guard(o, func_block_env);
    if (first_thing) {
      first_thing = false;
      new_func_env->settings.is_set = true;
    }
  });
  auto return_ir = make_unique<IR_Return>(resolve_to_gpr(result, func_block_env), return_reg);
  return_reg->type = return_ir->value->type;
  lambda_ts.ts_args.at(0) = return_ir->value->type;

  new_func_env->emit(std::move(return_ir));
  func_block_env->end_label->idx = new_func_env->code.size();
  new_func_env->emit(make_unique<IR_Null>());
  new_func_env->finish();
  auto obj_env = get_parent_env_of_type<ObjectFileEnv>(new_func_env);
  assert(obj_env);

  if (new_func_env->settings.save_code) {
    obj_env->functions.push_back(new_func_env);
  }

  place->type = lambda_ts;

  auto id = types.add_method(arg_type.as_symbol()->name, arg_name.as_symbol()->name, lambda_ts);
  return compile_real_function_call(form, compile_get_sym_val("method-set!", env),
                                    {compile_get_sym_val(arg_type.as_symbol()->name, env),
                                     compile_integer_to_gpr(id, env), resolve_to_gpr(place, env)},
                                    env);
}

std::shared_ptr<Place> Goal::compile_lambda(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  (void)env;

  // Get Args
  auto args = goos.get_uneval_args(form, rest, 1);

  std::unordered_set<std::string> keywords = {"name", "inline-only"};  // also class, type, friends
  if (!args.check_count(1) || !args.check_keywords(keywords)) {
    throw_compile_error(form, "invalid lambda arguments, bad keyword or count");
  }

  TypeSpec lambda_ts = get_base_typespec("function");
  // temp return typespec
  lambda_ts.ts_args.push_back(get_base_typespec("none"));
  // temp for now
  auto place = std::make_shared<LambdaPlace>(get_none()->type);

  // Build Lambda Object
  GoalLambda& lambda = place->lambda;
  // todo get the correct function type

  for_each_in_list(args.unnamed_args.front(), [&](Object o) {
    if (o.type == SYMBOL) {
      lambda.params.emplace_back(o.as_symbol()->name, get_base_typespec("object"));
      lambda_ts.ts_args.push_back(get_base_typespec("object"));
    } else {
      auto param_args = goos.get_uneval_args(o, o, 3);
      if (param_args.unnamed_args.size() >= 3 || param_args.unnamed_args.size() < 1 ||
          param_args.has_rest || !param_args.named_args.empty()) {
        printf("bad %d %d %d %d\n", param_args.unnamed_args.size() >= 3,
               param_args.unnamed_args.size() < 1, param_args.has_rest,
               !param_args.named_args.empty());
        printf("args %s\n", param_args.print().c_str());
        throw_compile_error(o, "invalid lambda parameter 2");
      }

      GoalLambdaParam parm;
      if (param_args.unnamed_args.front().type != SYMBOL) {
        throw_compile_error(o, "invalid lambda parameter 3");
      }
      parm.name = param_args.unnamed_args.front().as_symbol()->name;

      if (param_args.unnamed_args.size() >= 2) {
        // parm.type = TypeSpec(param_args.unnamed_args[1], types); // todo improve
        parm.type = compile_typespec(param_args.unnamed_args[1]);
      } else {
        parm.type = get_base_typespec("object");
      }

      if (param_args.unnamed_args.size() >= 3) {
        parm.default_value = param_args.unnamed_args[2];
        parm.has_default = true;
      }

      lambda.params.push_back(parm);
      lambda_ts.ts_args.push_back(parm.type);
    }
  });

  assert(lambda.params.size() + 1 == lambda_ts.ts_args.size());

  auto name_kv = args.named_args.find("name");
  if (name_kv != args.named_args.end()) {
    if (name_kv->second.type != SYMBOL) {
      throw_compile_error(form, "lambda name must be a symbol");
    }

    lambda.name = name_kv->second.as_symbol()->name;
  }

  if (!args.has_rest) {
    throw_compile_error(form, "lambda must have a body");
  }
  lambda.body = args.rest;
  place->func = nullptr;

  bool inline_only = false;
  auto inline_only_kv = args.named_args.find("inline-only");
  if (inline_only_kv != args.named_args.end() && inline_only_kv->second.type == SYMBOL &&
      inline_only_kv->second.as_symbol()->name == "#t") {
    inline_only = true;
  }

  // Compile lambda
  if (!inline_only) {
    // printf("COMPILE LAMBDA WITH BODY %s\n", lambda.body.print().c_str());
    // Object body_with_begin = PairObject::make_new(SymbolObject::make_new(goos.reader.symbolTable,
    // "begin"), lambda.body);
    auto new_func_env = std::make_shared<FunctionEnv>(place->print());
    new_func_env->parent = env;
    new_func_env->segment = DEBUG_SEGMENT;  // todo not this
    // set up arguments
    assert(lambda.params.size() < 8);  // todo, this should be more graceful
    for (uint32_t i = 0; i < lambda.params.size(); i++) {
      RegConstraint constr;
      constr.instr_id = 0;
      constr.var_id = new_func_env->vars.size();
      constr.ass.kind = REGISTER;
      constr.ass.reg_id = ARG_REGS[i];
      new_func_env->params[lambda.params.at(i).name] =
          new_func_env->alloc_reg(lambda.params.at(i).type);
      //      printf("add lc\n");
      new_func_env->constrain_reg(constr);
    }

    place->func = new_func_env;
    new_func_env->emit(make_unique<IR_FunctionBegin>(place));
    auto return_reg = new_func_env->alloc_reg(get_none()->type);

    // create a block env so we can use "return-from #f" to return from the function
    auto func_block_env = std::make_shared<BlockEnv>(new_func_env, "#f");
    func_block_env->return_value = return_reg;
    auto label = std::make_shared<Label>(new_func_env.get());
    func_block_env->end_label = label;

    // auto return_ir = std::make_shared<IR_Return>(compile_error_guard(body_with_begin,
    // new_func_env), return_reg);
    std::shared_ptr<Place> result = get_none();
    bool first_thing = true;
    for_each_in_list(lambda.body, [&](Object o) {
      result = compile_error_guard(o, func_block_env);
      if (first_thing) {
        first_thing = false;
        new_func_env->settings.is_set = true;
      }
    });
    auto return_ir = make_unique<IR_Return>(resolve_to_gpr(result, func_block_env), return_reg);

    return_reg->type = return_ir->value->type;
    lambda_ts.ts_args.at(0) = return_ir->value->type;

    new_func_env->emit(std::move(return_ir));
    func_block_env->end_label->idx = new_func_env->code.size();
    new_func_env->emit(make_unique<IR_Null>());
    new_func_env->finish();
    auto obj_env = get_parent_env_of_type<ObjectFileEnv>(new_func_env);
    assert(obj_env);

    if (new_func_env->settings.save_code) {
      obj_env->functions.push_back(new_func_env);
    }

    //    printf("FUNCTION:\n");
    //    for(auto& c : new_func_env->code) {
    //      printf("%s\n", c->print().c_str());
    //    }
  }

  place->type = lambda_ts;
  return place;
}

std::shared_ptr<Place> Goal::compile_declare(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env) {
  auto& settings = get_parent_env_of_type<DeclareEnv>(env)->settings;

  if (settings.is_set) {
    throw_compile_error(form, "function has multiple declares");
  }
  settings.is_set = true;

  for_each_in_list(rest, [&](Object o) {
    if (o.type != PAIR) {
      throw_compile_error(o, "invalid declare specification");
    }

    auto first = o.as_pair()->car;
    auto rrest = o.as_pair()->cdr;

    if (first.type != SYMBOL) {
      throw_compile_error(first, "invalid declare specification, expected a symbol");
    }

    if (first.as_symbol()->name == "inline") {
      if (rrest.type != EMPTY_LIST) {
        throw_compile_error(first, "invalid inline declare");
      }
      settings.allow_inline = true;
      settings.inline_by_default = true;
      settings.save_code = true;
    } else if (first.as_symbol()->name == "allow-inline") {
      if (rrest.type != EMPTY_LIST) {
        throw_compile_error(first, "invalid allow-inline declare");
      }
      settings.allow_inline = true;
      settings.inline_by_default = false;
      settings.save_code = true;
    } else if (first.as_symbol()->name == "asm-func") {
      get_parent_env_of_type<FunctionEnv>(env)->is_asm_func = true;
    }

    else {
      throw_compile_error(first, "unrecognized declare statement");
    }
  });
  return get_none();
}

std::shared_ptr<Place> Goal::compile_with_inline(const Object& form,
                                                 Object rest,
                                                 std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 1);
  if (!args.has_rest || args.unnamed_args.size() < 1 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid with-inline form");
  }

  auto setting = args.unnamed_args.front();
  if (setting.type != SYMBOL) {
    throw_compile_error(form, "with-inline invalid setting");
  }

  bool inline_preference = false;

  if (setting.as_symbol()->name == "#t") {
    inline_preference = true;
  } else if (setting.as_symbol()->name == "#f") {
    inline_preference = false;
  } else {
    throw_compile_error(form, "with-inline can only be set to #t or #f");
  }

  auto new_env = std::make_shared<WithInlineEnv>(inline_preference);
  new_env->parent = env;

  auto result = get_none();
  for_each_in_list(args.rest, [&](Object o) { result = compile_error_guard(o, new_env); });

  return result;
}

static std::string reg_names[] = {
    "rax",  "rcx",  "rdx",  "rbx",  "rsp",   "rbp",   "rsi",   "rdi",   "r8",    "r9",    "r10",
    "r11",  "r12",  "r13",  "r14",  "r15",   "xmm0",  "xmm1",  "xmm2",  "xmm3",  "xmm4",  "xmm5",
    "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};

ColoringAssignment Goal::reg_name_to_ca(Object& name) {
  if (name.type != SYMBOL) {
    throw_compile_error(name, "invalid register name");
  }

  auto nas = name.as_symbol();

  for (int i = 0; i < 32; i++) {
    if (nas->name == reg_names[i]) {
      ColoringAssignment ca;
      ca.kind = AssignmentKind::REGISTER;
      ca.reg_id = i;
      return ca;
    }
  }

  throw_compile_error(name, "unknown register name");

  return {};
}

std::shared_ptr<Place> Goal::compile_rlet(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 1);
  if (!args.has_rest || args.unnamed_args.size() < 1 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid rlet form");
  }

  auto defs = args.unnamed_args.front();

  auto body = args.rest;

  auto lenv = std::make_shared<LexicalEnv>();
  lenv->parent = env;

  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  std::unordered_set<std::string> allowed_args = {"reg", "type"};

  std::vector<RegConstraint> constraints;
  uint32_t start_idx = fenv->code.size();

  for_each_in_list(defs, [&](Object o) {
    // (new-place [:reg old-place] [:type type-spec] [:class reg-type] [:bind #f|lexical|lambda])
    auto def_args = goos.get_uneval_args_no_rest(o, o, 1);

    if (def_args.unnamed_args.size() != 1 || !def_args.check_keywords(allowed_args)) {
      throw_compile_error(o, "invalid rleg def");
    }

    // get the name of the new place
    auto new_place_name = def_args.unnamed_args.front();
    if (new_place_name.type != SYMBOL)
      throw_compile_error(new_place_name, "invalid place name");

    // get the type of the new place
    TypeSpec ts = get_base_typespec("object");
    auto type_kv = def_args.named_args.find("type");
    if (type_kv != def_args.named_args.end()) {
      ts = compile_typespec(type_kv->second);
    }

    // alloc a gpr:
    auto new_place_reg = env->alloc_reg(ts);

    auto reg_kv = def_args.named_args.find("reg");
    if (reg_kv != def_args.named_args.end()) {
      RegConstraint constraint;
      //      constraint.var_id = fenv->vars.size() - 1;
      constraint.var_id = std::dynamic_pointer_cast<GprPlace>(new_place_reg)->identity;
      constraint.ass = reg_name_to_ca(reg_kv->second);
      constraint.instr_id = -1;  // to be set later.
      constraints.push_back(constraint);
    }

    lenv->vars[new_place_name.as_symbol()->name] = new_place_reg;
  });

  auto result = get_none();
  for_each_in_list(args.rest, [&](Object o) { result = compile_error_guard(o, lenv); });

  //  for(uint32_t i = start_idx; i < fenv->code.size(); i++) {
  //    for(auto c : constraints) {
  //      c.instr_id = i;
  //      fenv->constrain_reg(c);
  //    }
  //  }

  for (auto c : constraints) {
    c.instr_id = start_idx;
    //    printf("add rlc\n");
    fenv->constrain_reg(c);
  }

  return result;
}

std::shared_ptr<Place> Goal::compile_mlet(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 1);
  if (!args.has_rest || args.unnamed_args.size() < 1 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid mlet form");
  }

  // CREATE ENV
  auto menv = std::make_shared<SymbolMacroEnv>(env);
  auto defs = args.unnamed_args.front();

  for_each_in_list(defs, [&](Object o) {
    auto def_args = goos.get_uneval_args_no_rest(o, o, 2);
    if (def_args.unnamed_args.size() != 2 || !def_args.named_args.empty()) {
      throw_compile_error(o, "invalid symbol macro definition");
    }
    if (def_args.unnamed_args[0].type != SYMBOL) {
      throw_compile_error(o, "invalid name for symbol macro");
    }

    menv->macros[def_args.unnamed_args[0].as_symbol()] = def_args.unnamed_args[1];
  });

  auto result = get_none();
  for_each_in_list(args.rest, [&](Object o) { result = compile_error_guard(o, menv); });

  return result;
}

std::shared_ptr<Place> Goal::compile_get_ra_ptr(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env) {
  (void)form;
  expect_empty_list(rest);
  auto result =
      env->alloc_reg(TypeSpec(get_base_typespec("pointer").type, {get_base_typespec("uint64")}));
  env->emit(make_unique<IR_GetReturnAddressPointer>(result));
  return result;
}