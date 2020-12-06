/*!
 * @file Function.cpp
 * Calling and defining functions, lambdas, and inlining.
 */

#include "goalc/compiler/Compiler.h"
#include "third-party/fmt/core.h"

namespace {
/*!
 * Get the preference to inline of the given environment.
 */
bool get_inline_preference(Env* env) {
  auto ile = get_parent_env_of_type<WithInlineEnv>(env);
  if (ile) {
    return ile->inline_preference;
  } else {
    return false;
  }
}

/*!
 * Hacky function to seek past arguments to get a goos::Object containing the body of a lambda.
 */
const goos::Object& get_lambda_body(const goos::Object& def) {
  auto* iter = &def;
  while (true) {
    auto car = iter->as_pair()->car;
    if (car.is_symbol() && car.as_symbol()->name.at(0) == ':') {
      iter = &iter->as_pair()->cdr;
      iter = &iter->as_pair()->cdr;
    } else {
      assert(car.is_list());
      return iter->as_pair()->cdr;
    }
  }
}
}  // namespace

/*!
 * The (inline my-func) form is like my-func, except my-func will be inlined instead of called,
 * when used in a function call. This only works for immediaate function calls, you can't "save"
 * an (inline my-func) into a function pointer.
 *
 * If inlining is not possible (function disallows inlining or didn't save its code), throw an
 * error.
 */
Val* Compiler::compile_inline(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL}, {});

  auto kv = m_inlineable_functions.find(args.unnamed.at(0).as_symbol());
  if (kv == m_inlineable_functions.end()) {
    throw_compiler_error(form, "Cannot inline {} because the function's code could not be found.",
                         args.unnamed.at(0).print());
  }

  if (kv->second->func && !kv->second->func->settings.allow_inline) {
    throw_compiler_error(form,
                         "Cannot inline {} because inlining of this function was disallowed.");
  }
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<InlinedLambdaVal>(kv->second->type(), kv->second);
}

/*!
 * Compile a lambda. This is used for real lambdas, lets, and defuns. So there are a million
 * confusing special cases...
 */
Val* Compiler::compile_lambda(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto obj_env = get_parent_env_of_type<FileEnv>(env);
  auto args = get_va(form, rest);
  if (args.unnamed.empty() || !args.unnamed.front().is_list() ||
      !args.only_contains_named({"name", "inline-only", "segment"})) {
    throw_compiler_error(form, "Invalid lambda form");
  }

  auto place = fe->alloc_val<LambdaVal>(get_none()->type());
  auto& lambda = place->lambda;
  auto lambda_ts = m_ts.make_typespec("function");

  // parse the argument list.
  for_each_in_list(args.unnamed.front(), [&](const goos::Object& o) {
    if (o.is_symbol()) {
      // if it has no type, assume object.
      lambda.params.push_back({symbol_string(o), m_ts.make_typespec("object")});
      lambda_ts.add_arg(m_ts.make_typespec("object"));
    } else {
      auto param_args = get_va(o, o);
      va_check(o, param_args, {goos::ObjectType::SYMBOL, {}}, {});

      GoalArg parm;
      parm.name = symbol_string(param_args.unnamed.at(0));
      parm.type = parse_typespec(param_args.unnamed.at(1));

      lambda.params.push_back(parm);
      lambda_ts.add_arg(parm.type);
    }
  });
  assert(lambda.params.size() == lambda_ts.arg_count());

  // optional name for debugging (defun sets this)
  if (args.has_named("name")) {
    lambda.debug_name = symbol_string(args.get_named("name"));
  }

  lambda.body = get_lambda_body(rest);  // first is the argument list, rest is body
  place->func = nullptr;

  bool inline_only =
      args.has_named("inline-only") && symbol_string(args.get_named("inline-only")) != "#f";

  // pick default segment to store function in.
  int segment = MAIN_SEGMENT;
  if (fe->segment == DEBUG_SEGMENT) {
    // make anonymous lambdas in debug functions also go to debug
    segment = DEBUG_SEGMENT;
  }

  // override default segment.
  if (args.has_named("segment")) {
    auto segment_name = symbol_string(args.get_named("segment"));
    if (segment_name == "main") {
      segment = MAIN_SEGMENT;
    } else if (segment_name == "debug") {
      segment = DEBUG_SEGMENT;
    } else {
      throw_compiler_error(form, "Segment {} was not recognized in lambda option.", segment_name);
    }
  }

  if (!inline_only) {
    // compile a function! First create a unique name...
    std::string function_name = lambda.debug_name;
    if (function_name.empty()) {
      function_name = obj_env->get_anon_function_name();
    }
    auto new_func_env = std::make_unique<FunctionEnv>(env, function_name);
    new_func_env->set_segment(segment);

    // set up arguments
    if (lambda.params.size() >= 8) {
      throw_compiler_error(form,
                           "Cannot generate an x86-64 function for a lambda with {} parameters.  "
                           "The current limit is 8.",
                           lambda.params.size());
    }

    // set up argument register constraints.
    std::vector<RegVal*> args_for_coloring;
    for (u32 i = 0; i < lambda.params.size(); i++) {
      IRegConstraint constr;
      constr.instr_idx = 0;  // constraint at function start
      auto ireg = new_func_env->make_ireg(lambda.params.at(i).type, emitter::RegKind::GPR);
      ireg->mark_as_settable();
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

    // compile the function, iterating through the body.
    Val* result = nullptr;
    bool first_thing = true;
    for_each_in_list(lambda.body, [&](const goos::Object& o) {
      result = compile_error_guard(o, func_block_env);
      if (!dynamic_cast<None*>(result)) {
        result = result->to_reg(func_block_env);
      }
      if (first_thing) {
        first_thing = false;
        // you could cheat and do a (begin (blorp) (declare ...)) to get around this.
        // but I see no strong reason why "declare"s need to go at the beginning, so no reason
        // to make this better.
        new_func_env->settings.is_set = true;
      }
    });

    if (new_func_env->is_asm_func) {
      // don't add return automatically!
      lambda_ts.add_arg(new_func_env->asm_func_return_type);
    } else if (result && !dynamic_cast<None*>(result)) {
      // got a result, so to_gpr it and return it.
      auto final_result = result->to_gpr(new_func_env.get());
      new_func_env->emit(std::make_unique<IR_Return>(return_reg, final_result));
      func_block_env->return_types.push_back(final_result->type());
      auto return_type = m_ts.lowest_common_ancestor(func_block_env->return_types);
      lambda_ts.add_arg(return_type);
    } else {
      // empty body or returning none, return none
      lambda_ts.add_arg(m_ts.make_typespec("none"));
    }
    // put null instruction at the end so jumps to the end have somewhere to go.
    func_block_env->end_label.idx = new_func_env->code().size();
    new_func_env->emit(std::make_unique<IR_Null>());
    new_func_env->finish();

    // save our code for possible inlining
    assert(obj_env);
    if (new_func_env->settings.save_code) {
      obj_env->add_function(std::move(new_func_env));
    }
  }

  place->set_type(lambda_ts);
  return place;
}

/*!
 * Compile a form which should be either a function call (possibly inline) or method call.
 * Note - calling method "new" isn't handled by this.
 * Again, there are way too many special cases here.
 */
Val* Compiler::compile_function_or_method_call(const goos::Object& form, Env* env) {
  goos::Object f = form;
  auto fe = get_parent_env_of_type<FunctionEnv>(env);

  auto args = get_va(form, form);
  auto uneval_head = args.unnamed.at(0);
  Val* head = get_none();

  // determine if this call should be automatically inlined.
  // this logic will not trigger for a manually inlined call [using the (inline func) form]
  bool auto_inline = false;
  if (uneval_head.is_symbol()) {
    // we can only auto-inline the function if its name is explicitly given.
    // look it up:
    auto kv = m_inlineable_functions.find(uneval_head.as_symbol());
    if (kv != m_inlineable_functions.end()) {
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
    if (uneval_head.is_symbol()) {
      if (is_local_symbol(uneval_head, env) ||
          m_symbol_types.find(symbol_string(uneval_head)) != m_symbol_types.end()) {
        // the local environment (mlets, lexicals, constants, globals) defines this symbol.
        // this will "win" over a method name lookup, so we should compile as normal
        head = compile_error_guard(args.unnamed.front(), env);
      } else {
        // we don't think compiling the head give us a function, so it's either a method or an error
        is_method_call = true;
      }
    } else {
      // the head is some expression. Could be something like (inline my-func) or (-> obj
      // func-ptr-field) in either case, compile it - and it can't be a method call.
      head = compile_error_guard(args.unnamed.front(), env);
    }
  }

  if (!is_method_call) {
    // typecheck that we got a function
    typecheck(form, m_ts.make_typespec("function"), head->type(), "Function call head");
  }

  // see if its an "immediate" application. This happens in three cases:
  // 1). the user directly puts a (lambda ...) form in the head (like with a (let) macro)
  // 2). the user used a (inline my-func) to grab the LambdaPlace of the function.
  // 3). the auto-inlining above looked up the LambdaPlace of an inlinable_function.

  // note that an inlineable function looked up by symbol or other way WILL NOT cast to a
  // LambdaPlace! so this cast will only succeed if the auto-inliner succeeded, or the user has
  // passed use explicitly a lambda either with the lambda form, or with the (inline ...) form.
  LambdaVal* head_as_lambda = nullptr;
  bool got_inlined_lambda = false;
  if (!is_method_call) {
    // try directly as a lambda
    head_as_lambda = dynamic_cast<LambdaVal*>(head);

    if (!head_as_lambda) {
      // nope, so try as an (inline x)
      auto head_as_inlined_lambda = dynamic_cast<InlinedLambdaVal*>(head);
      if (head_as_inlined_lambda) {
        // yes, remember the lambda that contains and flag that we're inlining.
        head_as_lambda = head_as_inlined_lambda->lv;
        got_inlined_lambda = true;
      }
    }
  }

  // no lambda (not inlining or immediate), and not a method call, so we should actually get
  // the function pointer.
  if (!head_as_lambda && !is_method_call) {
    head = head->to_gpr(env);
  }

  // compile arguments
  std::vector<RegVal*> eval_args;
  for (uint32_t i = 1; i < args.unnamed.size(); i++) {
    auto intermediate = compile_error_guard(args.unnamed.at(i), env);
    // todo, are the eval/to_reg'd in batches?
    eval_args.push_back(intermediate->to_reg(env));
  }

  if (head_as_lambda) {
    // inline/immediate the function!

    // check args are ok
    if (head_as_lambda->lambda.params.size() != eval_args.size()) {
      throw_compiler_error(form, "Expected {} arguments but got {} for inlined lambda.",
                           head_as_lambda->lambda.params.size(), eval_args.size());
    }

    // construct a lexical environment
    auto lexical_env = fe->alloc_env<LexicalEnv>(env);

    Env* inlined_compile_env = lexical_env;

    // if need to, create a label env.
    // we don't want a separate label env with lets, but we do for inlined functions.
    // either inlined through the auto-inliner, or through an explicit (inline x) form.
    if (auto_inline || got_inlined_lambda) {
      inlined_compile_env = fe->alloc_env<LabelEnv>(lexical_env);
    }

    // check arg types
    if (!head->type().arg_count()) {
      if (head->type().arg_count() - 1 != eval_args.size()) {
        throw_compiler_error(form,
                             "Expected {} arguments for an inlined lambda with type {} but got {}.",
                             head->type().arg_count() - 1, head->type().print(), eval_args.size());
      }
      // immediate lambdas (lets) will have all types as the most general object by default
      // inlined functions will have real types that are checked...
      for (uint32_t i = 0; i < eval_args.size(); i++) {
        typecheck(form, head->type().get_arg(i), eval_args.at(i)->type(),
                  "function/lambda (inline/immediate) argument");
      }
    }

    // copy args...
    for (uint32_t i = 0; i < eval_args.size(); i++) {
      // note, inlined functions will get a more specific type if possible
      // todo, is this right?
      auto type = eval_args.at(i)->type();
      auto copy = env->make_ireg(type, get_preferred_reg_kind(type));
      env->emit(std::make_unique<IR_RegSet>(copy, eval_args.at(i)));
      copy->mark_as_settable();
      lexical_env->vars[head_as_lambda->lambda.params.at(i).name] = copy;
    }

    // setup env
    BlockEnv* inlined_block_env = nullptr;
    RegVal* result_reg_if_return_from = nullptr;
    if (auto_inline || got_inlined_lambda) {
      inlined_block_env = fe->alloc_env<BlockEnv>(inlined_compile_env, "#f");
      result_reg_if_return_from =
          inlined_compile_env->make_ireg(get_none()->type(), emitter::RegKind::GPR);
      inlined_block_env->return_value = result_reg_if_return_from;
      inlined_block_env->end_label = Label(fe);
      inlined_compile_env = inlined_block_env;
    }

    // compile inline!
    bool first_thing = true;
    Val* result = get_none();
    for_each_in_list(head_as_lambda->lambda.body, [&](const goos::Object& o) {
      result = compile_error_guard(o, inlined_compile_env);
      if (!dynamic_cast<None*>(result)) {
        result = result->to_reg(inlined_compile_env);
      }
      if (first_thing) {
        first_thing = false;
        lexical_env->settings.is_set = true;
      }
    });

    // ignore the user specified return type and return the most specific type.
    // todo - does this make sense for an inline function? Should we check the return type?

    if (inlined_block_env && !inlined_block_env->return_types.empty()) {
      // there were return froms used in the function, so we fall back to using the separate
      // return gpr.
      if (!dynamic_cast<None*>(result)) {
        auto final_result = result->to_gpr(inlined_compile_env);
        inlined_compile_env->emit(
            std::make_unique<IR_RegSet>(result_reg_if_return_from, final_result));
        inlined_block_env->return_types.push_back(final_result->type());
        auto return_type = m_ts.lowest_common_ancestor(inlined_block_env->return_types);
        inlined_block_env->return_value->set_type(return_type);
      } else {
        inlined_block_env->return_value->set_type(get_none()->type());
      }

      inlined_compile_env->emit(std::make_unique<IR_Null>());
      inlined_block_env->end_label.idx = inlined_block_env->end_label.func->code().size();
      return inlined_block_env->return_value;
    }

    inlined_compile_env->emit(std::make_unique<IR_Null>());
    return result;
  } else {
    // not an inlined/immediate, it's a real function call.
    // todo, this order is extremely likely to be wrong, we should get the method way earlier.
    if (is_method_call) {
      // method needs at least one argument to tell what we're calling the method on.
      if (eval_args.empty()) {
        throw_compiler_error(form, "Unrecognized symbol {} as head of form.", uneval_head.print());
      }
      // get the method function pointer
      head = compile_get_method_of_object(form, eval_args.front(), symbol_string(uneval_head), env);
      fmt::format("method of object {} {}\n", head->print(), head->type().print());
    }

    // convert the head to a GPR (if function, this is already done)
    auto head_as_gpr = head->to_gpr(env);
    if (head_as_gpr) {
      // method calls have special rules for typing _type_ arguments.
      if (is_method_call) {
        return compile_real_function_call(form, head_as_gpr, eval_args, env,
                                          eval_args.front()->type().base_type());
      } else {
        return compile_real_function_call(form, head_as_gpr, eval_args, env);
      }

    } else {
      throw_compiler_error(form, "Invalid function call! Possibly a compiler bug.");
    }
  }

  assert(false);
  return get_none();
}

namespace {
/*!
 * Is the given typespec for a varargs function? Assumes typespec is a function to begin with.
 */
bool is_varargs_function(const TypeSpec& ts) {
  return ts.arg_count() >= 2 && ts.get_arg(0).print() == "_varargs_";
}
}  // namespace

/*!
 * Do a real x86-64 function call.
 */
Val* Compiler::compile_real_function_call(const goos::Object& form,
                                          RegVal* function,
                                          const std::vector<RegVal*>& args,
                                          Env* env,
                                          const std::string& method_type_name) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  fe->require_aligned_stack();
  TypeSpec return_ts;
  if (function->type().arg_count() == 0) {
    // if the type system doesn't know what the function will return, don't allow it to be called
    throw_compiler_error(
        form, "This function call has unknown argument and return types and cannot be called.");
  } else {
    return_ts = function->type().last_arg();
  }

  auto return_reg = env->make_ireg(return_ts, emitter::RegKind::GPR);

  // check arg count:
  if (function->type().arg_count() && !is_varargs_function(function->type())) {
    if (function->type().arg_count() - 1 != args.size()) {
      throw_compiler_error(form,
                           "Expected {} arguments but got {} for a real function call on type {}.",
                           function->type().arg_count() - 1, args.size(), function->type().print());
    }
    for (uint32_t i = 0; i < args.size(); i++) {
      if (method_type_name.empty()) {
        typecheck(form, function->type().get_arg(i), args.at(i)->type(),
                  fmt::format("function argument {}", i));
      } else {
        typecheck(form, function->type().get_arg(i).substitute_for_method_call(method_type_name),
                  args.at(i)->type(), fmt::format("function argument {}", i));
      }
    }
  }

  if (args.size() > 8) {
    throw_compiler_error(form, "Function call cannot use more than 8 parameters.");
  }

  // set args (introducing a move here makes coloring more likely to be possible)
  std::vector<RegVal*> arg_outs;
  for (auto& arg : args) {
    arg_outs.push_back(env->make_ireg(arg->type(), emitter::RegKind::GPR));
    arg_outs.back()->mark_as_settable();
    env->emit(std::make_unique<IR_RegSet>(arg_outs.back(), arg));
  }

  // todo, there's probably a more efficient way to do this.
  auto temp_function = fe->make_gpr(function->type());
  env->emit(std::make_unique<IR_RegSet>(temp_function, function));
  env->emit(std::make_unique<IR_FunctionCall>(temp_function, return_reg, arg_outs));

  if (m_settings.emit_move_after_return) {
    auto result_reg = env->make_gpr(return_reg->type());
    env->emit(std::make_unique<IR_RegSet>(result_reg, return_reg));
    return result_reg;
  } else {
    return return_reg;
  }
}

/*!
 * A (declare ...) form can be used to configure settings inside a function.
 * Currently there aren't many useful settings, but more may be added in the future.
 */
Val* Compiler::compile_declare(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto& settings = get_parent_env_of_type<DeclareEnv>(env)->settings;

  if (settings.is_set) {
    throw_compiler_error(form, "Function cannot have multiple declares");
  }
  settings.is_set = true;

  for_each_in_list(rest, [&](const goos::Object& o) {
    if (!o.is_pair()) {
      throw_compiler_error(o, "Invalid declare specification.");
    }

    auto first = o.as_pair()->car;
    auto rrest = &o.as_pair()->cdr;

    if (!first.is_symbol()) {
      throw_compiler_error(
          first, "Invalid declare option specification, expected a symbol, but got {} instead.",
          first.print());
    }

    if (first.as_symbol()->name == "inline") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid inline declare, no options were expected.");
      }
      settings.allow_inline = true;
      settings.inline_by_default = true;
      settings.save_code = true;
    } else if (first.as_symbol()->name == "allow-inline") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid allow-inline declare");
      }
      settings.allow_inline = true;
      settings.inline_by_default = false;
      settings.save_code = true;
    } else if (first.as_symbol()->name == "asm-func") {
      auto fe = get_parent_env_of_type<FunctionEnv>(env);
      fe->is_asm_func = true;
      if (!rrest->is_pair()) {
        throw_compiler_error(
            form, "Declare asm-func must provide the function's return type as an argument.");
      }
      fe->asm_func_return_type = parse_typespec(rrest->as_pair()->car);
      if (!rrest->as_pair()->cdr.is_empty_list()) {
        throw_compiler_error(first, "Invalid asm-func declare");
      }
    } else if (first.as_symbol()->name == "print-asm") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid print-asm declare");
      }
      settings.print_asm = true;

    } else if (first.as_symbol()->name == "allow-saved-regs") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid allow-saved-regs declare");
      }
      auto fe = get_parent_env_of_type<FunctionEnv>(env);
      fe->asm_func_saved_regs = true;

    } else {
      throw_compiler_error(first, "Unrecognized declare option {}.", first.print());
    }
  });
  return get_none();
}
