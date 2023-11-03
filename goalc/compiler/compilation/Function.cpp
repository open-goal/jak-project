/*!
 * @file Function.cpp
 * Calling and defining functions, lambdas, and inlining.
 */

#include "goalc/compiler/Compiler.h"
#include "goalc/emitter/CallingConvention.h"

#include "third-party/fmt/core.h"

namespace {

/*!
 * Hacky function to seek past arguments to get a goos::Object containing the body of a lambda.
 */
const goos::Object& get_lambda_body(const goos::Object& def) {
  auto* iter = &def;
  while (true) {
    auto car = iter->as_pair()->car;
    if (car.is_symbol() && car.as_symbol().name_ptr[0] == ':') {
      iter = &iter->as_pair()->cdr;
      iter = &iter->as_pair()->cdr;
    } else {
      ASSERT(car.is_list());
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
 * If inlining is not possible (function didn't save its code), throw an error.
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

  auto fe = env->function_env();
  return fe->alloc_val<InlinedLambdaVal>(kv->second.type, kv->second);
}

Val* Compiler::compile_local_vars(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto fe = env->function_env();

  for_each_in_list(rest, [&](const goos::Object& o) {
    if (o.is_symbol()) {
      // if it has no type, assume object.
      auto name = o.as_symbol();
      if (fe->params.find(name) != fe->params.end()) {
        throw_compiler_error(form, "Cannot declare a local named {}, this already exists.",
                             name.name_ptr);
      }
      auto ireg = fe->make_ireg(m_ts.make_typespec("object"), RegClass::GPR_64);
      ireg->mark_as_settable();
      fe->params[name] = ireg;
    } else {
      auto param_args = get_va(o, o);
      va_check(o, param_args, {goos::ObjectType::SYMBOL, {}}, {});
      auto name = param_args.unnamed.at(0).as_symbol();
      auto type = parse_typespec(param_args.unnamed.at(1), env);

      if (fe->params.find(name) != fe->params.end()) {
        throw_compiler_error(form, "Cannot declare a local named {}, this already exists.",
                             name.name_ptr);
      }

      if (m_ts.tc(TypeSpec("float"), type)) {
        auto ireg = fe->make_ireg(type, RegClass::FLOAT);
        ireg->mark_as_settable();
        fe->params[name] = ireg;
      } else if (m_ts.tc(TypeSpec("int128"), type) || m_ts.tc(TypeSpec("uint128"), type)) {
        auto ireg = fe->make_ireg(type, RegClass::INT_128);
        ireg->mark_as_settable();
        fe->params[name] = ireg;
      } else {
        auto ireg = fe->make_ireg(type, RegClass::GPR_64);
        ireg->mark_as_settable();
        fe->params[name] = ireg;
      }
    }
  });

  return get_none();
}

/*!
 * Compile a lambda. This is used for real lambdas, lets, and defuns. So there are a million
 * confusing special cases...
 */
Val* Compiler::compile_lambda(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto fe = env->function_env();
  auto obj_env = env->file_env();
  auto args = get_va(form, rest);
  if (args.unnamed.empty() || !args.unnamed.front().is_list() ||
      !args.only_contains_named({"name", "segment", "behavior", "immediate"})) {
    throw_compiler_error(form, "Invalid lambda form");
  }

  bool immediate =
      args.has_named("immediate") && symbol_string(args.get_named("immediate")) != "#f";

  // allocate this lambda from the object file environment. This makes it safe for this to hold
  // on to references to this as an inlineable function even if the enclosing function fails.
  // for example, the top-level may (define some-func (lambda...)) and even if top-level fails,
  // we keep around a reference to some-func to be possibly inlined.
  auto place = obj_env->alloc_val<LambdaVal>(get_none()->type(), immediate);
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
      parm.type = parse_typespec(param_args.unnamed.at(1), env);

      lambda.params.push_back(parm);
      lambda_ts.add_arg(parm.type);
    }
  });
  ASSERT(lambda.params.size() == lambda_ts.arg_count());

  // optional name for debugging (defun sets this)
  if (args.has_named("name")) {
    lambda.debug_name = symbol_string(args.get_named("name"));
  }

  lambda.body = get_lambda_body(rest);  // first is the argument list, rest is body
  place->func = nullptr;

  // pick default segment to store function in.
  int segment = fe->segment_for_static_data();

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

  if (!immediate) {
    // compile a function! First create a unique name...
    std::string function_name = lambda.debug_name;
    if (function_name.empty()) {
      function_name = obj_env->get_anon_function_name();
    }
    auto new_func_env = std::make_unique<FunctionEnv>(env, function_name, &m_goos.reader);
    new_func_env->set_segment(segment);

    // set up arguments
    if (lambda.params.size() > 8) {
      throw_compiler_error(form,
                           "Cannot generate a real function for a lambda with {} parameters.  "
                           "The current limit is 8.",
                           lambda.params.size());
    }

    // set up argument register constraints.
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

    if (args.has_named("behavior")) {
      const std::string behavior_type = symbol_string(args.get_named("behavior"));
      auto self_var = new_func_env->make_gpr(m_ts.make_typespec(behavior_type));
      self_var->mark_as_settable();
      IRegConstraint constr;
      constr.contrain_everywhere = true;
      constr.desired_register = emitter::gRegInfo.get_process_reg();
      constr.ireg = self_var->ireg();
      self_var->set_rlet_constraint(constr.desired_register);
      new_func_env->constrain(constr);

      if (new_func_env->params.find(m_goos.intern_ptr("self")) != new_func_env->params.end()) {
        throw_compiler_error(form, "Cannot have an argument named self in a behavior");
      }
      new_func_env->params[m_goos.intern_ptr("self")] = self_var;
      reset_args_for_coloring.push_back(self_var);
      lambda_ts.add_new_tag("behavior", behavior_type);
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
      if (!new_func_env->params.insert({m_goos.intern_ptr(lambda.params.at(i).name), ireg})
               .second) {
        throw_compiler_error(form, "lambda has multiple arguments named {}",
                             lambda.params.at(i).name);
      }
      new_func_env->emit_ir<IR_RegSet>(form, ireg, reset_args_for_coloring.at(i));
    }

    // compile the function, iterating through the body.
    Val* result = nullptr;
    bool first_thing = true;
    for_each_in_list(lambda.body, [&](const goos::Object& o) {
      result = compile_error_guard(o, func_block_env);
      if (!dynamic_cast<None*>(result)) {
        result = result->to_reg(o, func_block_env);
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
    } else if (result && !dynamic_cast<None*>(result) && result->type() != TypeSpec("none")) {
      // got a result, so to_gpr it and return it.

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
      // empty body or returning none, return none
      lambda_ts.add_arg(m_ts.make_typespec("none"));
    }
    // put null instruction at the end so jumps to the end have somewhere to go.
    func_block_env->end_label.idx = new_func_env->code().size();
    new_func_env->emit_ir<IR_Null>(form);
    new_func_env->finish();

    // save our code for possible inlining
    ASSERT(obj_env);
    if (new_func_env->settings.save_code) {
      obj_env->add_function(std::move(new_func_env));
    }
  } else {
    if (args.has_named("behavior")) {
      throw_compiler_error(form, "Inline behaviors are not yet implemented.");
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
  auto fe = env->function_env();

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
      if (kv->second.inline_by_default) {  // inline when possible, so we should inline
        auto_inline = true;
        auto* lv = env->function_env()->alloc_val<LambdaVal>(kv->second.type, false);
        lv->lambda = kv->second.lambda;
        head = lv;
      }
    }
  }

  bool is_method_call = false;
  if (!auto_inline) {
    // if auto-inlining failed, we must get the thing to call in a different way.
    if (uneval_head.is_symbol()) {
      if (uneval_head.as_symbol() == "inspect" || uneval_head.as_symbol() == "print") {
        is_method_call = true;
      } else {
        if (is_local_symbol(uneval_head, env) ||
            m_symbol_types.find(uneval_head.as_symbol()) != m_symbol_types.end()) {
          // the local environment (mlets, lexicals, constants, globals) defines this symbol.
          // this will "win" over a method name lookup, so we should compile as normal
          head = compile_error_guard(args.unnamed.front(), env);
        } else {
          // we don't think compiling the head give us a function, so it's either a method or an
          // error
          is_method_call = true;
        }
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
        head_as_lambda =
            env->function_env()->alloc_val<LambdaVal>(head_as_inlined_lambda->lv.type, false);
        head_as_lambda->lambda = head_as_inlined_lambda->lv.lambda;
        got_inlined_lambda = true;
      }
    } else {
      // we got a lambda: but we don't want to use immediates by default:
      if (!auto_inline && !head_as_lambda->is_immediate) {
        head_as_lambda = nullptr;
      }
    }
  }

  // no lambda (not inlining or immediate), and not a method call, so we should actually get
  // the function pointer.
  if (!head_as_lambda && !is_method_call) {
    head = head->to_gpr(form, env);
  }

  // compile arguments
  std::vector<RegVal*> eval_args;
  for (uint32_t i = 1; i < args.unnamed.size(); i++) {
    auto intermediate = compile_error_guard(args.unnamed.at(i), env);
    eval_args.push_back(intermediate->to_reg(args.unnamed.at(i), env));
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
      auto copy =
          env->make_ireg(type, m_ts.lookup_type_allow_partial_def(type)->get_preferred_reg_class());
      env->emit_ir<IR_RegSet>(form, copy, eval_args.at(i));
      copy->mark_as_settable();
      lexical_env->vars[m_goos.intern_ptr(head_as_lambda->lambda.params.at(i).name)] = copy;
    }

    // setup env
    BlockEnv* inlined_block_env = nullptr;
    RegVal* result_reg_if_return_from = nullptr;
    if (auto_inline || got_inlined_lambda) {
      inlined_block_env = fe->alloc_env<BlockEnv>(inlined_compile_env, "#f");
      RegClass ret_class = RegClass::GPR_64;
      if (head->type().last_arg() != TypeSpec("none") &&
          m_ts.lookup_type(head->type().last_arg())->get_load_size() == 16) {
        ret_class = RegClass::INT_128;
      }
      result_reg_if_return_from =
          inlined_compile_env->make_ireg(head->type().last_arg(), ret_class);

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
        result = result->to_reg(o, inlined_compile_env);
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
        auto final_result = result->to_reg(form, inlined_compile_env);
        inlined_block_env->return_types.push_back(final_result->type());

        for (const auto& possible_type : inlined_block_env->return_types) {
          if (possible_type != TypeSpec("none") &&
              m_ts.lookup_type(possible_type)->get_load_size() == 16) {
            result_reg_if_return_from->change_class(RegClass::INT_128);
          }
        }

        inlined_compile_env->emit_ir<IR_RegSet>(form, result_reg_if_return_from, final_result);

        auto return_type = m_ts.lowest_common_ancestor(inlined_block_env->return_types);
        inlined_block_env->return_value->set_type(return_type);
      } else {
        inlined_block_env->return_value->set_type(get_none()->type());
      }

      inlined_compile_env->emit_ir<IR_Null>(form);
      inlined_block_env->end_label.idx = inlined_block_env->end_label.func->code().size();
      return inlined_block_env->return_value;
    }

    inlined_compile_env->emit_ir<IR_Null>(form);
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
      head = compile_get_method_of_object(form, eval_args.front(), symbol_string(uneval_head), env,
                                          true);
    }

    // convert the head to a GPR (if function, this is already done)
    auto head_as_gpr = head->to_gpr(form, env);
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

  ASSERT(false);
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
  auto fe = env->function_env();
  fe->require_aligned_stack();
  TypeSpec return_ts;
  if (function->type().arg_count() == 0) {
    // if the type system doesn't know what the function will return, don't allow it to be called
    throw_compiler_error(
        form, "This function call has unknown argument and return types and cannot be called.");
  } else {
    return_ts = function->type().last_arg();
  }

  auto cc = get_function_calling_convention(function->type(), m_ts);
  RegClass ret_reg_class = RegClass::GPR_64;
  if (cc.return_reg && cc.return_reg->is_xmm()) {
    ret_reg_class = RegClass::INT_128;
  }

  auto return_reg = env->make_ireg(return_ts, ret_reg_class);

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
  for (int i = 0; i < (int)args.size(); i++) {
    const auto& arg = args.at(i);
    auto reg = cc.arg_regs.at(i);
    arg_outs.push_back(
        env->make_ireg(arg->type(), reg.is_xmm() ? RegClass::INT_128 : RegClass::GPR_64));
    arg_outs.back()->mark_as_settable();
    env->emit_ir<IR_RegSet>(form, arg_outs.back(), arg);
  }

  // todo, there's probably a more efficient way to do this.
  auto temp_function = fe->make_gpr(function->type());
  env->emit_ir<IR_RegSet>(form, temp_function, function);
  env->emit_ir<IR_FunctionCall>(form, temp_function, return_reg, arg_outs, cc.arg_regs,
                                cc.return_reg);

  if (m_settings.emit_move_after_return) {
    auto result_reg = env->make_ireg(return_reg->type(), ret_reg_class);
    env->emit_ir<IR_RegSet>(form, result_reg, return_reg);
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
  auto& settings = get_parent_env_of_type_slow<DeclareEnv>(env)->settings;

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

    if (first.as_symbol() == "inline") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid inline declare, no options were expected.");
      }
      settings.allow_inline = true;
      settings.inline_by_default = true;
      settings.save_code = true;
    } else if (first.as_symbol() == "allow-inline") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid allow-inline declare");
      }
      settings.allow_inline = true;
      settings.inline_by_default = false;
      settings.save_code = true;
    } else if (first.as_symbol() == "asm-func") {
      auto fe = env->function_env();
      fe->is_asm_func = true;
      if (!rrest->is_pair()) {
        throw_compiler_error(
            form, "Declare asm-func must provide the function's return type as an argument.");
      }
      fe->asm_func_return_type = parse_typespec(rrest->as_pair()->car, env);
      if (!rrest->as_pair()->cdr.is_empty_list()) {
        throw_compiler_error(first, "Invalid asm-func declare");
      }
    } else if (first.as_symbol() == "print-asm") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid print-asm declare");
      }
      settings.print_asm = true;

    } else if (first.as_symbol() == "allow-saved-regs") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid allow-saved-regs declare");
      }
      auto fe = env->function_env();
      fe->asm_func_saved_regs = true;

    } else {
      throw_compiler_error(first, "Unrecognized declare option {}.", first.print());
    }
  });
  return get_none();
}

Val* Compiler::compile_declare_file(const goos::Object& /*form*/,
                                    const goos::Object& rest,
                                    Env* env) {
  for_each_in_list(rest, [&](const goos::Object& o) {
    if (!o.is_pair()) {
      throw_compiler_error(o, "Invalid declare-file specification.");
    }

    auto first = o.as_pair()->car;
    auto rrest = &o.as_pair()->cdr;

    if (!first.is_symbol()) {
      throw_compiler_error(
          first, "Invalid declare option specification, expected a symbol, but got {} instead.",
          first.print());
    }

    if (first.as_symbol() == "debug") {
      if (!rrest->is_empty_list()) {
        throw_compiler_error(first, "Invalid debug declare");
      }
      if (!env->file_env()->is_debug_file()) {
        env->file_env()->set_debug_file();
        throw DebugFileDeclareException();
      }

    } else {
      throw_compiler_error(first, "Unrecognized declare-file option {}.", first.print());
    }
  });

  return get_none();
}
