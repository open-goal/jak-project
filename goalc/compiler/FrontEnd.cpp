#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"
#include "goalc/compiler/Val.h"
#include "goalc/compiler/Env.h"
#include "common/goos/Interpreter.h"
#include "common/type_system/deftype.h"
#include "third-party/fmt/format.h"
#include "goalc/compiler/StaticObject.h"

using namespace goos;

namespace {
const std::unordered_map<
    std::string,
    Val* (Compiler::*)(const goos::Object& form, const goos::Object& rest, Env* env)>
    goal_forms = {
        //        // inline asm
        //        {".ret", &Compiler::compile_asm},
        //        {".push", &Compiler::compile_asm},
        //        {".pop", &Compiler::compile_asm},
        //        {".jmp", &Compiler::compile_asm},
        //        {".sub", &Compiler::compile_asm},
        //        {".ret-reg", &Compiler::compile_asm},

        // BLOCK FORMS
        {"top-level", &Compiler::compile_top_level},
        {"begin", &Compiler::compile_begin},
        {"block", &Compiler::compile_block},
        {"return-from", &Compiler::compile_return_from},
        {"label", &Compiler::compile_label},
        {"goto", &Compiler::compile_goto},

        // COMPILER CONTROL
        {"gs", &Compiler::compile_gs},
        {":exit", &Compiler::compile_exit},
        {"asm-file", &Compiler::compile_asm_file},
        {"listen-to-target", &Compiler::compile_listen_to_target},
        {"reset-target", &Compiler::compile_reset_target},
        {":status", &Compiler::compile_poke},
        {"in-package", &Compiler::compile_in_package},

        // CONDITIONAL COMPILATION
        {"#cond", &Compiler::compile_gscond},
        {"defglobalconstant", &Compiler::compile_defglobalconstant},
        {"seval", &Compiler::compile_seval},

        // CONTROL FLOW
        {"cond", &Compiler::compile_cond},
        {"when-goto", &Compiler::compile_when_goto},

        // DEFINITION
        {"define", &Compiler::compile_define},
        {"define-extern", &Compiler::compile_define_extern},
        {"set!", &Compiler::compile_set},

        // TYPE
        {"deftype", &Compiler::compile_deftype},
        {"defmethod", &Compiler::compile_defmethod},
        //        {"defenum", &Compiler::compile_defenum},
        {"->", &Compiler::compile_deref},
        {"&", &Compiler::compile_addr_of},
        {"the-as", &Compiler::compile_the_as},
        {"the", &Compiler::compile_the},
        {"print-type", &Compiler::compile_print_type},
        {"new", &Compiler::compile_new},
        {"car", &Compiler::compile_car},
        {"cdr", &Compiler::compile_cdr},
        {"method", &Compiler::compile_method},

        // LAMBDA
        {"lambda", &Compiler::compile_lambda},
        {"declare", &Compiler::compile_declare},
        {"inline", &Compiler::compile_inline},
        //        {"with-inline", &Compiler::compile_with_inline},
        //        {"rlet", &Compiler::compile_rlet},
        //        {"get-ra-ptr", &Compiler::compile_get_ra_ptr},

        // MACRO
        {"quote", &Compiler::compile_quote},
        {"mlet", &Compiler::compile_mlet},
        //        {"defconstant", &Compiler::compile_defconstant},

        // OBJECT
        //        {"current-method-type", &Compiler::compile_current_method_type},

        // MATH
        {"+", &Compiler::compile_add},
        {"-", &Compiler::compile_sub},
        {"*", &Compiler::compile_mul},
        {"/", &Compiler::compile_div},
        {"shlv", &Compiler::compile_shlv},
        {"shrv", &Compiler::compile_shrv},
        {"sarv", &Compiler::compile_sarv},
        //        {"shl", &Compiler::compile_shl},
        //        {"shr", &Compiler::compile_shr},
        //        {"sar", &Compiler::compile_sar},
        {"mod", &Compiler::compile_mod},
        {"logior", &Compiler::compile_logior},
        {"logxor", &Compiler::compile_logxor},
        {"logand", &Compiler::compile_logand},
        {"lognot", &Compiler::compile_lognot},
        {"=", &Compiler::compile_condition_as_bool},
        {"!=", &Compiler::compile_condition_as_bool},
        {"eq?", &Compiler::compile_condition_as_bool},
        {"not", &Compiler::compile_condition_as_bool},
        {"<=", &Compiler::compile_condition_as_bool},
        {">=", &Compiler::compile_condition_as_bool},
        {"<", &Compiler::compile_condition_as_bool},
        {">", &Compiler::compile_condition_as_bool},
        {"&+", &Compiler::compile_pointer_add},

        // BUILDER (build-dgo/build-cgo?)
        {"build-dgos", &Compiler::compile_build_dgo},

        // UTIL
        {"set-config!", &Compiler::compile_set_config},
};
}

/*!
 * Highest level compile function
 */
Val* Compiler::compile(const goos::Object& code, Env* env) {
  switch (code.type) {
    case goos::ObjectType::PAIR:
      return compile_pair(code, env);
    case goos::ObjectType::INTEGER:
      return compile_integer(code, env);
    case goos::ObjectType::SYMBOL:
      return compile_symbol(code, env);
    case goos::ObjectType::STRING:
      return compile_string(code, env);
    case goos::ObjectType::FLOAT:
      return compile_float(code, env);
    default:
      ice("Don't know how to compile " + code.print());
  }
  return get_none();
}

/*!
 * Compile a pair/list.
 * Can be a compiler form, function call (possibly inlined), method call, immediate application of a
 * lambda, or a goos macro.
 * TODO - enums.
 */
Val* Compiler::compile_pair(const goos::Object& code, Env* env) {
  auto pair = code.as_pair();
  auto head = pair->car;
  auto rest = pair->cdr;

  if (head.is_symbol()) {
    auto head_sym = head.as_symbol();
    // first try as a goal compiler form
    auto kv_gfs = goal_forms.find(head_sym->name);
    if (kv_gfs != goal_forms.end()) {
      return ((*this).*(kv_gfs->second))(code, rest, env);
    }

    // next try as a macro
    goos::Object macro_obj;
    if (try_getting_macro_from_goos(head, &macro_obj)) {
      return compile_goos_macro(code, macro_obj, rest, env);
    }

    // try as an enum (not yet implemented)
  }

  // if none of the above cases worked, then treat it like a function/method call.
  return compile_function_or_method_call(code, env);
}

/*!
 * Compile an integer constant. Returns an IntegerConstantVal and emits no code.
 * These integer constants do not generate static data and are stored directly in the code
 * which is generated with to_gpr.
 * The type is always int.
 */
Val* Compiler::compile_integer(const goos::Object& code, Env* env) {
  assert(code.is_int());
  return compile_integer(code.integer_obj.value, env);
}

/*!
 * Compile an integer constant. Returns an IntegerConstantVal and emits no code.
 * These integer constants do not generate static data and are stored directly in the code
 * which is generated with to_gpr.
 * The type is always int.
 */
Val* Compiler::compile_integer(s64 value, Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<IntegerConstantVal>(m_ts.make_typespec("int"), value);
}

/*!
 * Get a SymbolVal representing a GOAL symbol object.
 */
SymbolVal* Compiler::compile_get_sym_obj(const std::string& name, Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<SymbolVal>(name, m_ts.make_typespec("symbol"));
}

/*!
 * Get a SymbolValueVal representing the value of a GOAL symbol.
 * Will throw a compilation error if the symbol wasn't previously defined.
 * TODO - determine sign extension behavior when loading symbol values.
 */
Val* Compiler::compile_get_symbol_value(const std::string& name, Env* env) {
  auto existing_symbol = m_symbol_types.find(name);
  if (existing_symbol == m_symbol_types.end()) {
    throw std::runtime_error("The symbol " + name + " was not defined");
  }

  auto ts = existing_symbol->second;
  auto sext = m_ts.lookup_type(ts)->get_load_signed();
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto sym = fe->alloc_val<SymbolVal>(name, m_ts.make_typespec("symbol"));
  auto re = fe->alloc_val<SymbolValueVal>(sym, ts, sext);
  return re;
}

/*!
 * Compile a symbol. Can get mlet macro symbols, local variables, constants, or symbols.
 */
Val* Compiler::compile_symbol(const goos::Object& form, Env* env) {
  auto name = symbol_string(form);

  // special case to get "nothing", used as a return value when nothing should be returned.
  if (name == "none") {
    return get_none();
  }

  // see if the symbol is defined in any enclosing symbol macro envs (mlet's).
  auto mlet_env = get_parent_env_of_type<SymbolMacroEnv>(env);
  while (mlet_env) {
    auto mlkv = mlet_env->macros.find(form.as_symbol());
    if (mlkv != mlet_env->macros.end()) {
      return compile_error_guard(mlkv->second, env);
    }
    mlet_env = get_parent_env_of_type<SymbolMacroEnv>(mlet_env->parent());
  }

  // see if it's a local variable
  auto lexical = env->lexical_lookup(form);
  if (lexical) {
    return lexical;
  }

  auto global_constant = m_global_constants.find(form.as_symbol());
  auto existing_symbol = m_symbol_types.find(form.as_symbol()->name);

  // see if it's a constant
  if (global_constant != m_global_constants.end()) {
    // check there is no symbol with the same name
    if (existing_symbol != m_symbol_types.end()) {
      throw_compile_error(form,
                          "symbol is both a runtime symbol and a global constant.  Something is "
                          "likely very wrong.");
    }

    // got a global constant
    return compile_error_guard(global_constant->second, env);
  }

  // none of those, so get a global symbol.
  return compile_get_symbol_value(name, env);
}

/*!
 * Compile a string constant. The constant is placed in the same segment as the parent function.
 */
Val* Compiler::compile_string(const goos::Object& form, Env* env) {
  return compile_string(form.as_string()->data, env,
                        get_parent_env_of_type<FunctionEnv>(env)->segment);
}

/*!
 * Compile a string constant and place it in the given segment.
 */
Val* Compiler::compile_string(const std::string& str, Env* env, int seg) {
  auto obj = std::make_unique<StaticString>(str, seg);
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto result = fe->alloc_val<StaticVal>(obj.get(), m_ts.make_typespec("string"));
  auto fie = get_parent_env_of_type<FileEnv>(env);
  fie->add_static(std::move(obj));
  return result;
}

/*!
 * Compile a floating point constant and place it in the same segment as the containing function.
 * Unlike integers, all floating point constants are stored separately as static data outside
 * of the code, at least in Jak 1.
 */
Val* Compiler::compile_float(const goos::Object& code, Env* env) {
  assert(code.is_float());
  return compile_float(code.float_obj.value, env,
                       get_parent_env_of_type<FunctionEnv>(env)->segment);
}

/*!
 * Compile a floating point constant and place it in given segment.
 * Unlike integers, all floating point constants are stored separately as static data outside
 * of the code, at least in Jak 1.
 */
Val* Compiler::compile_float(float value, Env* env, int seg) {
  auto obj = std::make_unique<StaticFloat>(value, seg);
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto result = fe->alloc_val<FloatConstantVal>(m_ts.make_typespec("float"), obj.get());
  auto fie = get_parent_env_of_type<FileEnv>(env);
  fie->add_static(std::move(obj));
  return result;
}

Val* Compiler::compile_pointer_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  typecheck(form, m_ts.make_typespec("pointer"), first->type(), "&+ first argument");
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  typecheck(form, m_ts.make_typespec("integer"), second->type(), "&+ second argument");
  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::ADD_64, result, second));
  return result;
}

/*!
 * Compile "top-level" form, which is equivalent to a begin.
 */
Val* Compiler::compile_top_level(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_begin(form, rest, env);
}

/*!
 * Compile "begin" form, which compiles each segment in a row.
 * TODO - determine if a GOAL begin matches this behavior for not "to_reg"ing anything.
 */
Val* Compiler::compile_begin(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  Val* result = get_none();
  for_each_in_list(rest, [&](const Object& o) {
    result = compile_error_guard(o, env);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(env);
    }
  });
  return result;
}

/*!
 * Compile "block" form. Code inside of a block can "return from" the block and provide a value
 * for the block. If this doesn't happen, the block takes on the value of the last thing in
 * the block.
 * TODO - determine if a GOAL block matches this behavior for not "to_reg"ing anything,
 *  and also using a gpr as a return value always.
 */
Val* Compiler::compile_block(const goos::Object& form, const goos::Object& _rest, Env* env) {
  auto rest = &_rest;
  auto name = pair_car(*rest);
  rest = &pair_cdr(*rest);

  if (!rest->is_pair()) {
    throw_compile_error(form, "Block form has an empty or invalid body");
  }

  auto fe = get_parent_env_of_type<FunctionEnv>(env);

  // create environment
  auto block_env = fe->alloc_env<BlockEnv>(env, symbol_string(name));

  // we need to create a return value register, as a "return-from" statement inside the block may
  // set it. for now it has a type of none, but we will set it after compiling the block.
  // TODO - determine if GOAL blocks _always_ return gprs, or if it's possible to return xmms.
  block_env->return_value = env->make_gpr(m_ts.make_typespec("none"));

  // create label to the end of the block (we don't yet know where it is...)
  block_env->end_label = Label(fe);

  // compile everything in the body
  Val* result = get_none();
  for_each_in_list(*rest, [&](const Object& o) {
    result = compile_error_guard(o, block_env);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(env);
    }
  });

  // if no return-from's were used, we can ignore the return_value register, and basically turn this
  // into a begin. this allows a block which returns a floating point value to return the value in
  // an xmm register, which is likely to eliminate a gpr->xmm move.
  // TODO - does this happen in GOAL?
  if (block_env->return_types.empty()) {
    return result;
  }

  // determine return type as the lowest common ancestor of the block's last form and any
  // return-from's
  auto& return_types = block_env->return_types;
  return_types.push_back(result->type());
  auto return_type = m_ts.lowest_common_ancestor(return_types);
  block_env->return_value->set_type(coerce_to_reg_type(return_type));

  // an IR to move the result of the block into the block's return register (if no return-from's are
  // taken)
  auto ir_move_rv = std::make_unique<IR_RegSet>(block_env->return_value, result->to_gpr(fe));

  // note - one drawback of doing this single pass is that a block always evaluates to a gpr.
  // so we may have an unneeded xmm -> gpr move that could have been an xmm -> xmm that could have
  // been eliminated.
  env->emit(std::move(ir_move_rv));

  // now we know the end of the block, so we set the label index to be on whatever comes after the
  // return move. functions always end with a "null" IR and "null" instruction, so this is safe.
  block_env->end_label.idx = block_env->end_label.func->code().size();

  return block_env->return_value;
}

/*!
 * Compile a "return-from" statement. These can be used to return from a block and give a value.
 * Note that there is a special "block" containing all code in a function called "#f", and you can
 * use (return-from #f value) to return early from an entire function.
 */
Val* Compiler::compile_return_from(const goos::Object& form, const goos::Object& _rest, Env* env) {
  const Object* rest = &_rest;
  auto block_name = symbol_string(pair_car(*rest));
  rest = &pair_cdr(*rest);
  auto value_expression = pair_car(*rest);
  expect_empty_list(pair_cdr(*rest));

  // evaluate expression to return
  auto result = compile_error_guard(value_expression, env);
  auto fe = get_parent_env_of_type<FunctionEnv>(env);

  // find block to return from
  auto block = dynamic_cast<BlockEnv*>(env->find_block(block_name));
  if (!block) {
    throw_compile_error(form,
                        "The return-from form was unable to find a block named " + block_name);
  }

  // move result into return register
  auto ir_move_rv = std::make_unique<IR_RegSet>(block->return_value, result->to_gpr(fe));

  // inform block of our possible return type
  block->return_types.push_back(result->type());

  env->emit(std::move(ir_move_rv));

  // jump to end of block (by label object)
  auto ir_jump = std::make_unique<IR_GotoLabel>(&block->end_label);
  env->emit(std::move(ir_jump));

  // In the real GOAL, there is likely a bug here where a non-none value is returned and to_gpr'd
  // todo, determine if we should replicate this bug and if it can have side effects.
  return get_none();
}

/*!
 * Compile a label form, which creates a named label that can be jumped to with goto.
 */
Val* Compiler::compile_label(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto label_name = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  // make sure we don't have a label with this name already
  // note that you cannot jump out of your label space - they are not nested like lexical scopes!
  // generally there is one label space per function. Inlined functions have their own label space
  // and they will not permit jumping outside of the inlined function into the caller because
  // that seems like a really bad idea.
  auto& labels = env->get_label_map();
  auto kv = labels.find(label_name);
  if (kv != labels.end()) {
    throw_compile_error(
        form, "There are two labels named " + label_name + " in the same label environment");
  }

  // make a label pointing to the end of the current function env. safe because we'll always add
  // a terminating "null" instruction at the end.
  auto func_env = get_parent_env_of_type<FunctionEnv>(env);
  labels[label_name] = Label(func_env, func_env->code().size());
  return get_none();
}

/*!
 * Compile a goto form, which unconditionally jumps to a label by name in the same label space.
 */
Val* Compiler::compile_goto(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  auto label_name = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  auto ir_goto = std::make_unique<IR_GotoLabel>();
  // this requires looking up the label by name after, as it may be a goto to a label which has not
  // yet been defined.

  // add this goto to the list of gotos to resolve after the function is done.
  // it's safe to have this reference, as the FunctionEnv also owns the goto.
  get_parent_env_of_type<FunctionEnv>(env)->unresolved_gotos.push_back({ir_goto.get(), label_name});
  env->emit(std::move(ir_goto));
  return get_none();
}

/*!
 * Convert an expression into a GoalCondition for use in a conditional branch.
 *
 * The reason for this design is to allow an optimization for
 * (if (< a b) ...) to be compiled without actually computing a true/false value for the (< a b)
 * expression. Instead, it will generate a cmp + jle sequence of instructions, which is much faster.
 * In particular, getting GOAL "true" requires a few instructions, so it's to avoid this when
 * possible.
 *
 * This can be applied to _any_ GOAL form, and will return a GoalCondition which can be used with a
 * Branch IR to branch if the condition is true/false.  When possible it applies the optimization
 * mentioned above, but will be fine in other cases too.  I believe the original GOAL compiler had a
 * similar system.
 *
 * Will branch if the condition is true and the invert flag is false.
 * Will branch if the condition is false and the invert flag is true.
 */
Condition Compiler::compile_condition(const goos::Object& condition, Env* env, bool invert) {
  Condition gc;

  // These are special conditions that can be optimized into a cmp + jxx instruction.
  const std::unordered_map<std::string, ConditionKind> conditions_inverted = {
      {"!=", ConditionKind::EQUAL},   {"eq?", ConditionKind::NOT_EQUAL},
      {"neq?", ConditionKind::EQUAL}, {"=", ConditionKind::NOT_EQUAL},
      {">", ConditionKind::LEQ},      {"<", ConditionKind::GEQ},
      {">=", ConditionKind::LT},      {"<=", ConditionKind::GT}};

  const std::unordered_map<std::string, ConditionKind> conditions_normal = {
      {"!=", ConditionKind::NOT_EQUAL},   {"eq?", ConditionKind::EQUAL},
      {"neq?", ConditionKind::NOT_EQUAL}, {"=", ConditionKind::EQUAL},
      {">", ConditionKind::GT},           {"<", ConditionKind::LT},
      {">=", ConditionKind::GEQ},         {"<=", ConditionKind::LEQ}};

  // possibly a form with an optimizable condition?
  if (condition.is_pair()) {
    auto first = pair_car(condition);
    auto rest = pair_cdr(condition);

    if (first.is_symbol()) {
      auto fas = first.as_symbol();

      // if there's a not, we can just try again to get an optimization with the invert flipped.
      if (fas->name == "not") {
        auto arg = pair_car(rest);
        if (!pair_cdr(rest).is_empty_list()) {
          throw_compile_error(condition, "A condition with \"not\" can have only one argument");
        }
        return compile_condition(arg, env, !invert);
      }

      auto& conditions = invert ? conditions_inverted : conditions_normal;
      auto nc_kv = conditions.find(fas->name);

      if (nc_kv != conditions.end()) {
        // it is an optimizable condition!
        gc.kind = nc_kv->second;

        // get args...
        auto args = get_va(rest, rest);
        va_check(rest, args, {{}, {}}, {});
        auto first_arg = compile_error_guard(args.unnamed.at(0), env);
        auto second_arg = compile_error_guard(args.unnamed.at(1), env);

        if (is_number(first_arg->type())) {
          // it's a numeric comparison, so we may need to coerce.
          auto math_mode = get_math_mode(first_arg->type());

          // there is no support for comparing bintegers, so we turn the binteger comparison into an
          // integer.
          if (is_binteger(first_arg->type())) {
            first_arg = number_to_integer(first_arg, env);
          }

          // convert second one to appropriate type as needed
          if (is_number(second_arg->type())) {
            second_arg = to_math_type(second_arg, math_mode, env);
          }
        }

        // use signed comparison only if first argument is a signed integer (or coerced binteger)
        // (floating point ignores this)
        gc.is_signed = is_singed_integer_or_binteger(first_arg->type());

        // pick between a floating point and an integer comparison.
        if (is_float(first_arg->type())) {
          gc.a = first_arg->to_xmm(env);
          gc.b = second_arg->to_xmm(env);
          gc.is_float = true;
        } else {
          gc.a = first_arg->to_gpr(env);
          gc.b = second_arg->to_gpr(env);
        }

        return gc;
      }
    }
  }

  // not something we can process more.  Just evaluate as normal and check if we get false.
  // todo - it's possible to optimize a false comparison because the false offset is zero
  gc.kind = invert ? ConditionKind::EQUAL : ConditionKind::NOT_EQUAL;
  gc.a = compile_error_guard(condition, env)->to_gpr(env);
  gc.b = compile_get_sym_obj("#f", env)->to_gpr(env);

  return gc;
}

/*!
 * Compile a comparison when we explicitly want a boolean result. This is used whenever a condition
 * _isn't_ used as a branch condition. Like (set! x (< 1 2))
 *
 * TODO, this could be optimized quite a bit.
 */
Val* Compiler::compile_condition_as_bool(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env) {
  (void)rest;
  auto c = compile_condition(form, env, true);
  auto result = compile_get_sym_obj("#f", env)->to_gpr(env);  // todo - can be optimized.
  Label label(get_parent_env_of_type<FunctionEnv>(env), -5);
  auto branch_ir = std::make_unique<IR_ConditionalBranch>(c, label);
  auto branch_ir_ref = branch_ir.get();
  env->emit(std::move(branch_ir));

  // move true
  env->emit(std::make_unique<IR_RegSet>(
      result, compile_get_sym_obj("#t", env)->to_gpr(env)));  // todo, can be optimized
  branch_ir_ref->label.idx = branch_ir_ref->label.func->code().size();
  branch_ir_ref->mark_as_resolved();

  return result;
}

/*!
 * The when-goto form is a better version of (if condition (goto x))
 * It compiles into a single conditional branch.
 */
Val* Compiler::compile_when_goto(const goos::Object& form, const goos::Object& _rest, Env* env) {
  (void)form;
  auto* rest = &_rest;
  auto condition_code = pair_car(*rest);
  rest = &pair_cdr(*rest);

  auto label = symbol_string(pair_car(*rest));
  expect_empty_list(pair_cdr(*rest));

  // compile as condition (will set flags register with a cmp instruction)
  auto condition = compile_condition(condition_code, env, false);
  auto branch = std::make_unique<IR_ConditionalBranch>(condition, Label());
  get_parent_env_of_type<FunctionEnv>(env)->unresolved_cond_gotos.push_back({branch.get(), label});
  env->emit(std::move(branch));
  return get_none();
}

/*!
 * The Scheme/Lisp "cond" form.
 * Works like you expect. Return type is the lowest common ancestor of all possible return values.
 * If no cases match and there's no else, returns #f.
 * TODO - how should the return type work if #f can possibly be returned?
 */
Val* Compiler::compile_cond(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto result = env->make_gpr(m_ts.make_typespec("object"));

  auto fenv = get_parent_env_of_type<FunctionEnv>(env);
  auto end_label = fenv->alloc_unnamed_label();
  end_label->func = fenv;
  end_label->idx = -3;  // placeholder

  bool got_else = false;

  std::vector<TypeSpec> case_result_types;

  for_each_in_list(rest, [&](const goos::Object& o) {
    auto test = pair_car(o);
    auto clauses = pair_cdr(o);

    if (got_else) {
      throw_compile_error(form, "cannot have anything after an else in a cond");
    }

    if (test.is_symbol() && symbol_string(test) == "else") {
      got_else = true;
    }

    if (got_else) {
      // just set the output to this.
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
        if (!dynamic_cast<None*>(case_result)) {
          case_result = case_result->to_reg(env);
        }
      });

      case_result_types.push_back(case_result->type());

      // optimization - if we get junk, don't bother moving it, just leave junk in return.
      if (!is_none(case_result)) {
        // todo, what does GOAL do here? does it matter?
        env->emit(std::make_unique<IR_RegSet>(result, case_result->to_gpr(env)));
      }

    } else {
      // CONDITION CHECK
      auto condition = compile_condition(test, env, true);

      // BRANCH FWD
      auto branch_ir = std::make_unique<IR_ConditionalBranch>(condition, Label());
      auto branch_ir_ref = branch_ir.get();
      branch_ir->mark_as_resolved();
      env->emit(std::move(branch_ir));

      // CODE
      Val* case_result = get_none();
      for_each_in_list(clauses, [&](const goos::Object& clause) {
        case_result = compile_error_guard(clause, env);
        if (!dynamic_cast<None*>(case_result)) {
          case_result = case_result->to_reg(env);
        }
      });

      case_result_types.push_back(case_result->type());
      if (!is_none(case_result)) {
        // todo, what does GOAL do here?
        env->emit(std::make_unique<IR_RegSet>(result, case_result->to_gpr(env)));
      }

      // GO TO END
      auto ir_goto_end = std::make_unique<IR_GotoLabel>(end_label);
      env->emit(std::move(ir_goto_end));

      // PATCH BRANCH FWD
      branch_ir_ref->label.idx = fenv->code().size();
    }
  });

  if (!got_else) {
    // if no else, clause, return #f.  But don't retype. todo what does goal do here?
    auto get_false = std::make_unique<IR_LoadSymbolPointer>(result, "#f");
    env->emit(std::move(get_false));
  }

  result->set_type(coerce_to_reg_type(m_ts.lowest_common_ancestor(case_result_types)));

  // PATCH END
  end_label->idx = fenv->code().size();

  return result;
}

/*!
 * Define or set a global value. Has some special magic to store data for functions which may be
 * inlined.
 */
Val* Compiler::compile_define(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& val = args.unnamed.at(1);

  // check we aren't duplicated a name as both a symbol and global constant
  auto global_constant = m_global_constants.find(sym.as_symbol());
  if (global_constant != m_global_constants.end()) {
    throw_compile_error(
        form, "it is illegal to define a GOAL symbol with the same name as a GOAL global constant");
  }

  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto sym_val = fe->alloc_val<SymbolVal>(symbol_string(sym), m_ts.make_typespec("symbol"));
  auto compiled_val = compile_error_guard(val, env);
  auto as_lambda = dynamic_cast<LambdaVal*>(compiled_val);
  if (as_lambda) {
    // there are two cases in which we save a function body that is passed to a define:
    // 1. It generated code [so went through the compiler] and the allow_inline flag is set.
    // 2. It didn't generate code [so explicitly with :inline-only lambdas]
    // The third case - immediate lambdas - don't get passed to a define,
    //   so this won't cause those to live for longer than they should
    if ((as_lambda->func && as_lambda->func->settings.allow_inline) || !as_lambda->func) {
      m_inlineable_functions[sym.as_symbol()] = as_lambda;
    }
  }

  auto in_gpr = compiled_val->to_gpr(fe);
  auto existing_type = m_symbol_types.find(sym.as_symbol()->name);
  if (existing_type == m_symbol_types.end()) {
    m_symbol_types[sym.as_symbol()->name] = in_gpr->type();
  } else {
    typecheck(form, existing_type->second, in_gpr->type(), "define on existing symbol");
  }

  if (!sym_val->settable()) {
    throw_compile_error(
        form, "Tried to use define on something that wasn't settable: " + sym_val->print());
  }
  fe->emit(std::make_unique<IR_SetSymbolValue>(sym_val, in_gpr));
  return in_gpr;
}

/*!
 * Inform the compiler of the type of a global. Will warn on changing type.
 */
Val* Compiler::compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::SYMBOL, {}}, {});
  auto& sym = args.unnamed.at(0);
  auto& typespec = args.unnamed.at(1);

  auto new_type = parse_typespec(typespec);

  auto existing_type = m_symbol_types.find(symbol_string(sym));
  if (existing_type != m_symbol_types.end() && existing_type->second != new_type) {
    // todo spdlog
    printf("[Warning] define-extern has redefined the type of symbol %s\npreviously: %s\nnow: %s\n",
           symbol_string(sym).c_str(), existing_type->second.print().c_str(),
           new_type.print().c_str());
  }
  m_symbol_types[symbol_string(sym)] = new_type;
  return get_none();
}

/*!
 * Set something to something.
 * Lots of special cases.
 */
Val* Compiler::compile_set(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});

  auto& destination = args.unnamed.at(0);
  // todo, I don't know if this is the correct order or not. Right now the value is computed
  // and to_reg'd first, then the destination is computed, if the destination requires math to
  // compute.
  auto source = compile_error_guard(args.unnamed.at(1), env)->to_reg(env);

  if (destination.is_symbol()) {
    // destination is just a symbol, so it's either a lexical variable or a global.

    // first, attempt a lexical set:
    auto lex_place = env->lexical_lookup(destination);
    if (lex_place) {
      // typecheck and set!
      typecheck(form, lex_place->type(), source->type(), "set! lexical variable");
      env->emit(std::make_unique<IR_RegSet>(lex_place, source));
      return source;
    } else {
      // try to set symbol
      auto existing = m_symbol_types.find(destination.as_symbol()->name);
      if (existing == m_symbol_types.end()) {
        throw_compile_error(
            form, "could not find something called " + symbol_string(destination) + " to set!");
      } else {
        typecheck(form, existing->second, source->type(), "set! global symbol");
        auto fe = get_parent_env_of_type<FunctionEnv>(env);
        auto sym_val =
            fe->alloc_val<SymbolVal>(symbol_string(destination), m_ts.make_typespec("symbol"));
        auto result_in_gpr = source->to_gpr(env);
        if (!sym_val->settable()) {
          throw_compile_error(
              form, "Tried to use set! on something that wasn't settable: " + sym_val->print());
        }
        env->emit(std::make_unique<IR_SetSymbolValue>(sym_val, result_in_gpr));
        return result_in_gpr;
      }
    }
  } else {
    // destination is some complex expression, so compile it and hopefully get something settable.
    auto dest = compile_error_guard(destination, env);
    if (!dest->settable()) {
      throw_compile_error(form,
                          "Tried to use set! on something that wasn't settable: " + dest->print());
    }
    auto as_mem_deref = dynamic_cast<MemoryDerefVal*>(dest);
    auto as_pair = dynamic_cast<PairEntryVal*>(dest);
    if (as_mem_deref) {
      // setting somewhere in memory
      auto base = as_mem_deref->base;
      auto base_as_mco = dynamic_cast<MemoryOffsetConstantVal*>(base);
      if (base_as_mco) {
        // if it is a constant offset, we can use a fancy x86-64 addressing mode to simplify
        auto ti = m_ts.lookup_type(as_mem_deref->type());
        env->emit(std::make_unique<IR_StoreConstOffset>(
            source, base_as_mco->offset, base_as_mco->base->to_gpr(env), ti->get_load_size()));
        return source;
      } else {
        // nope, the pointer to dereference is some compliated thing.
        auto ti = m_ts.lookup_type(as_mem_deref->type());
        env->emit(std::make_unique<IR_StoreConstOffset>(source, 0, base->to_gpr(env),
                                                        ti->get_load_size()));
        return source;
      }
    } else if (as_pair) {
      // this could probably be part of MemoryDerefVal and not a special case here.
      env->emit(std::make_unique<IR_StoreConstOffset>(source, as_pair->is_car ? -2 : 2,
                                                      as_pair->base->to_gpr(env), 4));
      return source;
    } else {
      throw_compile_error(form, "Set not implemented for this yet");
    }
  }
  throw std::runtime_error("Unexpected error in Set");
}

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
    throw_compile_error(form, "Couldn't find function to inline!");
  }

  if (kv->second->func && !kv->second->func->settings.allow_inline) {
    throw_compile_error(form, "Found function to inline, but it isn't allowed.");
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
  auto args = get_va(form, rest);
  if (args.unnamed.empty() || !args.unnamed.front().is_list() ||
      !args.only_contains_named({"name", "inline-only", "segment"})) {
    throw_compile_error(form, "Invalid lambda form");
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
      throw_compile_error(form, "invalid segment override in lambda");
    }
  }

  if (!inline_only) {
    // compile a function! First create env
    auto new_func_env = std::make_unique<FunctionEnv>(env, lambda.debug_name);
    new_func_env->set_segment(segment);

    // set up arguments
    if (lambda.params.size() >= 8) {
      throw_compile_error(form, "lambda generating code has too many parameters!");
    }

    // set up argument register constraints.
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
    if (result) {
      // got a result, so to_gpr it and return it.
      auto final_result = result->to_gpr(new_func_env.get());
      new_func_env->emit(std::make_unique<IR_Return>(return_reg, final_result));
      lambda_ts.add_arg(final_result->type());
    } else {
      // empty body, return none
      lambda_ts.add_arg(m_ts.make_typespec("none"));
    }
    // put null instruction at the end so jumps to the end have somewhere to go.
    func_block_env->end_label.idx = new_func_env->code().size();
    new_func_env->emit(std::make_unique<IR_Null>());
    new_func_env->finish();

    // save our code for possible inlining
    auto obj_env = get_parent_env_of_type<FileEnv>(new_func_env.get());
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
      throw_compile_error(form, "invalid argument count");
    }

    // construct a lexical environment
    auto lexical_env = fe->alloc_env<LexicalEnv>(env);

    Env* compile_env = lexical_env;

    // if need to, create a label env.
    // we don't want a separate label env with lets, but we do for inlined functions.
    // either inlined through the auto-inliner, or through an explicit (inline x) form.
    if (auto_inline || got_inlined_lambda) {
      compile_env = fe->alloc_env<LabelEnv>(lexical_env);
    }

    // check arg types
    if (!head->type().arg_count()) {
      if (head->type().arg_count() - 1 != eval_args.size()) {
        throw_compile_error(form,
                            "invalid number of arguments to function call (inline or immediate "
                            "lambda application)");
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
      lexical_env->vars[head_as_lambda->lambda.params.at(i).name] = copy;
    }

    // compile inline!
    bool first_thing = true;
    Val* result = get_none();
    for_each_in_list(head_as_lambda->lambda.body, [&](const goos::Object& o) {
      result = compile_error_guard(o, compile_env);
      if (!dynamic_cast<None*>(result)) {
        result = result->to_reg(compile_env);
      }
      if (first_thing) {
        first_thing = false;
        lexical_env->settings.is_set = true;
      }
    });

    // ignore the user specified return type and return the most specific type.
    // todo - does this make sense for an inline function? Should we check the return type?
    return result;
  } else {
    // not an inlined/immediate, it's a real function call.
    // todo, this order is extremely likely to be wrong, we should get the method way earlier.
    if (is_method_call) {
      // method needs at least one argument to tell what we're calling the method on.
      if (eval_args.empty()) {
        throw_compile_error(form,
                            "Unrecognized symbol " + uneval_head.print() + " as head of form");
      }
      // get the method function pointer
      head = compile_get_method_of_object(eval_args.front(), symbol_string(uneval_head), env);
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
      throw_compile_error(form, "can't figure out this function call!");
    }
  }

  throw_compile_error(form, "call_function_or_method unreachable");
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
    throw_compile_error(
        form, "This function call has unknown argument and return types and cannot be called");
  } else {
    return_ts = function->type().last_arg();
  }

  auto return_reg = env->make_ireg(return_ts, emitter::RegKind::GPR);

  // check arg count:
  if (function->type().arg_count() && !is_varargs_function(function->type())) {
    if (function->type().arg_count() - 1 != args.size()) {
      throw_compile_error(form, "invalid number of arguments to function call: got " +
                                    std::to_string(args.size()) + " and expected " +
                                    std::to_string(function->type().arg_count() - 1) + " for " +
                                    function->type().print());
    }
    for (uint32_t i = 0; i < args.size(); i++) {
      if (method_type_name.empty()) {
        typecheck(form, function->type().get_arg(i), args.at(i)->type(), "function argument");
      } else {
        typecheck(form, function->type().get_arg(i).substitute_for_method_call(method_type_name),
                  args.at(i)->type(), "function argument");
      }
    }
  }

  // set args (introducing a move here makes coloring more likely to be possible)
  std::vector<RegVal*> arg_outs;
  for (auto& arg : args) {
    arg_outs.push_back(env->make_ireg(arg->type(), emitter::RegKind::GPR));
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
    throw_compile_error(form, "function has multiple declares");
  }
  settings.is_set = true;

  for_each_in_list(rest, [&](const goos::Object& o) {
    if (!o.is_pair()) {
      throw_compile_error(o, "invalid declare specification");
    }

    auto first = o.as_pair()->car;
    auto rrest = o.as_pair()->cdr;

    if (!first.is_symbol()) {
      throw_compile_error(first, "invalid declare specification, expected a symbol");
    }

    if (first.as_symbol()->name == "inline") {
      if (!rrest.is_empty_list()) {
        throw_compile_error(first, "invalid inline declare");
      }
      settings.allow_inline = true;
      settings.inline_by_default = true;
      settings.save_code = true;
    } else if (first.as_symbol()->name == "allow-inline") {
      if (!rrest.is_empty_list()) {
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

/*!
 * Try to find a macro with the given name in the GOOS "goal_env". Return if it succeeded.
 */
bool Compiler::try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest) {
  Object macro_obj;
  bool got_macro = false;
  try {
    macro_obj = m_goos->eval_symbol(macro_name, m_goos->goal_env.as_env());
    if (macro_obj.is_macro()) {
      got_macro = true;
    }
  } catch (std::runtime_error& e) {
    got_macro = false;
  }

  if (got_macro) {
    *dest = macro_obj;
  }
  return got_macro;
}

/*!
 * Expand a macro, then compile the result.
 */
Val* Compiler::compile_goos_macro(const goos::Object& o,
                                  const goos::Object& macro_obj,
                                  const goos::Object& rest,
                                  Env* env) {
  auto macro = macro_obj.as_macro();
  Arguments args = m_goos->get_args(o, rest, macro->args);
  auto mac_env_obj = EnvironmentObject::make_new();
  auto mac_env = mac_env_obj.as_env();
  mac_env->parent_env = m_goos->global_environment.as_env();
  m_goos->set_args_in_env(o, args, macro->args, mac_env);
  m_goos->goal_to_goos.enclosing_method_type =
      get_parent_env_of_type<FunctionEnv>(env)->method_of_type_name;
  auto goos_result = m_goos->eval_list_return_last(macro->body, macro->body, mac_env);
  m_goos->goal_to_goos.reset();
  return compile_error_guard(goos_result, env);
}

/*!
 * Compile the #cond form, which is a compile-time conditional statement.
 */
Val* Compiler::compile_gscond(const goos::Object& form, const goos::Object& rest, Env* env) {
  if (!rest.is_pair()) {
    throw_compile_error(form, "#cond must have at least one clause, which must be a form");
  }
  Val* result = nullptr;

  Object lst = rest;
  for (;;) {
    if (lst.is_pair()) {
      Object current_case = lst.as_pair()->car;
      if (!current_case.is_pair()) {
        throw_compile_error(lst, "Bad case in #cond");
      }

      // check condition:
      Object condition_result = m_goos->eval_with_rewind(current_case.as_pair()->car,
                                                         m_goos->global_environment.as_env());
      if (m_goos->truthy(condition_result)) {
        if (current_case.as_pair()->cdr.is_empty_list()) {
          return get_none();
        }
        // got a match!
        result = get_none();

        for_each_in_list(current_case.as_pair()->cdr, [&](const Object& o) {
          result = compile_error_guard(o, env);
          if (!dynamic_cast<None*>(result)) {
            result = result->to_reg(env);
          }
        });
        return result;
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.is_empty_list()) {
      return get_none();
    } else {
      throw_compile_error(form, "malformed #cond");
    }
  }
}

/*!
 * Compile (quote x) or 'x forms.
 * Current only supports 'thing or '(). Static lists/pairs should be added at some point.
 */
Val* Compiler::compile_quote(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto thing = args.unnamed.at(0);
  switch (thing.type) {
    case goos::ObjectType::SYMBOL:
      return compile_get_sym_obj(thing.as_symbol()->name, env);
    case goos::ObjectType::EMPTY_LIST: {
      auto empty_pair = compile_get_sym_obj("_empty_", env);
      empty_pair->set_type(m_ts.make_typespec("pair"));
      return empty_pair;
    }
      // todo...
    default:
      throw_compile_error(form, "Can't quote this");
  }
  return get_none();
}

/*!
 * Compile defglobalconstant forms, which define a constant in both GOOS and GOAL.
 */
Val* Compiler::compile_defglobalconstant(const goos::Object& form,
                                         const goos::Object& _rest,
                                         Env* env) {
  auto rest = &_rest;
  (void)env;
  if (!rest->is_pair()) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  auto sym = pair_car(*rest).as_symbol();
  rest = &pair_cdr(*rest);
  auto value = pair_car(*rest);

  rest = &rest->as_pair()->cdr;
  if (!rest->is_empty_list()) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  // GOAL constant
  m_global_constants[sym] = value;

  // GOOS constant
  m_goos->global_environment.as_env()->vars[sym] = value;

  return get_none();
}

/*!
 * Compile an "mlet" scoped constant/symbol macro form
 */
Val* Compiler::compile_mlet(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto defs = pair_car(rest);
  auto body = pair_cdr(rest);

  auto fenv = get_parent_env_of_type<FunctionEnv>(env);
  auto menv = fenv->alloc_env<SymbolMacroEnv>(env);

  for_each_in_list(defs, [&](const goos::Object& o) {
    auto def_args = get_va(form, o);
    va_check(form, def_args, {goos::ObjectType::SYMBOL, {}}, {});
    menv->macros[def_args.unnamed.at(0).as_symbol()] = def_args.unnamed.at(1);
  });

  Val* result = get_none();
  for_each_in_list(body, [&](const goos::Object& o) {
    result = compile_error_guard(o, menv);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(menv);
    }
  });
  return result;
}

MathMode Compiler::get_math_mode(const TypeSpec& ts) {
  if (m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false)) {
    return MATH_BINT;
  }

  if (m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false)) {
    return MATH_INT;
  }

  if (m_ts.typecheck(m_ts.make_typespec("float"), ts, "", false, false)) {
    return MATH_FLOAT;
  }

  return MATH_INVALID;
}

bool Compiler::is_number(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("number"), ts, "", false, false);
}

bool Compiler::is_float(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("float"), ts, "", false, false);
}

bool Compiler::is_integer(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false) &&
         !m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false);
}

bool Compiler::is_binteger(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("binteger"), ts, "", false, false);
}

bool Compiler::is_singed_integer_or_binteger(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("integer"), ts, "", false, false) &&
         !m_ts.typecheck(m_ts.make_typespec("uinteger"), ts, "", false, false);
}

Val* Compiler::number_to_integer(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a binteger) to an integer.");
  } else if (is_float(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_FloatToInt>(result, in->to_xmm(env)));
    return result;
  } else if (is_integer(ts)) {
    return in;
  }
  throw std::runtime_error("Can't convert " + in->print() + " to an integer.");
}

Val* Compiler::number_to_binteger(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    return in;
  } else if (is_float(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a float) to a binteger.");
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    RegVal* input = in->to_reg(env);
    auto sa = fe->make_gpr(m_ts.make_typespec("int"));
    env->emit(std::make_unique<IR_LoadConstant64>(sa, 3));
    return compile_variable_shift(input, sa, env, IntegerMathKind::SHLV_64);
  }
  throw std::runtime_error("Can't convert " + in->print() + " to a binteger.");
}

Val* Compiler::number_to_float(Val* in, Env* env) {
  (void)env;
  auto ts = in->type();
  if (is_binteger(ts)) {
    throw std::runtime_error("Can't convert " + in->print() + " (a binteger) to a float.");
  } else if (is_float(ts)) {
    return in;
  } else if (is_integer(ts)) {
    auto fe = get_parent_env_of_type<FunctionEnv>(env);
    auto result = fe->make_xmm(m_ts.make_typespec("float"));
    env->emit(std::make_unique<IR_IntToFloat>(result, in->to_gpr(env)));
    return result;
  } else {
    throw std::runtime_error("Can't convert " + in->print() + " a float.");
  }
}

Val* Compiler::to_math_type(Val* in, MathMode mode, Env* env) {
  switch (mode) {
    case MATH_BINT:
      return number_to_binteger(in, env);
    case MATH_INT:
      return number_to_integer(in, env);
    case MATH_FLOAT:
      return number_to_float(in, env);
    default:
      throw std::runtime_error("Unknown math type: " + in->print());
  }
}

Val* Compiler::compile_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compile_error(form, "Invalid + form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_gpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::ADD_64, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::ADD_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_xmm(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_mul(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compile_error(form, "Invalid * form");
  }

  // look at the first value to determine the math mode
  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto result = env->make_gpr(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_gpr(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::IMUL_32, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_gpr(env)));
      }
      return result;
    }
    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));

      for (size_t i = 1; i < args.unnamed.size(); i++) {
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::MUL_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                ->to_xmm(env)));
      }
      return result;
    }
    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_sub(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_compile_error(form, "Invalid - form");
  }

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT:
      if (args.unnamed.size() == 1) {
        auto result = compile_integer(0, env)->to_gpr(env);
        env->emit(std::make_unique<IR_IntegerMath>(
            IntegerMathKind::SUB_64, result,
            to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_gpr(env)));
        return result;
      } else {
        auto result = env->make_gpr(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_gpr(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_IntegerMath>(
              IntegerMathKind::SUB_64, result,
              to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_gpr(env)));
        }
        return result;
      }

    case MATH_FLOAT:
      if (args.unnamed.size() == 1) {
        auto result =
            compile_float(0, env, get_parent_env_of_type<FunctionEnv>(env)->segment)->to_xmm(env);
        env->emit(std::make_unique<IR_FloatMath>(
            FloatMathKind::SUB_SS, result,
            to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                ->to_xmm(env)));
        return result;
      } else {
        auto result = env->make_xmm(first_type);
        env->emit(std::make_unique<IR_RegSet>(
            result, to_math_type(compile_error_guard(args.unnamed.at(0), env), math_type, env)
                        ->to_xmm(env)));

        for (size_t i = 1; i < args.unnamed.size(); i++) {
          env->emit(std::make_unique<IR_FloatMath>(
              FloatMathKind::SUB_SS, result,
              to_math_type(compile_error_guard(args.unnamed.at(i), env), math_type, env)
                  ->to_xmm(env)));
        }
        return result;
      }

    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_div(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (!args.named.empty() || args.unnamed.size() != 2) {
    throw_compile_error(form, "Invalid / form");
  }

  auto first_val = compile_error_guard(args.unnamed.at(0), env);
  auto first_type = first_val->type();
  auto math_type = get_math_mode(first_type);
  switch (math_type) {
    case MATH_INT: {
      auto fe = get_parent_env_of_type<FunctionEnv>(env);
      auto first_thing = first_val->to_gpr(env);
      auto result = env->make_ireg(first_type, emitter::RegKind::GPR);
      env->emit(std::make_unique<IR_RegSet>(result, first_thing));

      IRegConstraint result_rax_constraint;
      result_rax_constraint.instr_idx = fe->code().size();
      result_rax_constraint.ireg = result->ireg();
      result_rax_constraint.desired_register = emitter::RAX;
      fe->constrain(result_rax_constraint);

      env->emit(std::make_unique<IR_IntegerMath>(
          IntegerMathKind::IDIV_32, result,
          to_math_type(compile_error_guard(args.unnamed.at(1), env), math_type, env)->to_gpr(env)));
      return result;
    }

    case MATH_FLOAT: {
      auto result = env->make_xmm(first_type);
      env->emit(std::make_unique<IR_RegSet>(result, first_val->to_xmm(env)));
      env->emit(std::make_unique<IR_FloatMath>(
          FloatMathKind::DIV_SS, result,
          to_math_type(compile_error_guard(args.unnamed.at(1), env), math_type, env)->to_xmm(env)));
      return result;
    }

    case MATH_INVALID:
      throw_compile_error(
          form, "Cannot determine the math mode for object of type " + first_type.print());
      break;
    default:
      assert(false);
  }
  assert(false);
  return get_none();
}

Val* Compiler::compile_shlv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SHLV_64);
}

Val* Compiler::compile_sarv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SARV_64);
}

Val* Compiler::compile_shrv(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  return compile_variable_shift(first, second, env, IntegerMathKind::SHRV_64);
}

Val* Compiler::compile_variable_shift(const RegVal* in,
                                      const RegVal* sa,
                                      Env* env,
                                      IntegerMathKind kind) {
  auto result = env->make_gpr(in->type());
  auto sa_in = env->make_gpr(sa->type());

  env->emit(std::make_unique<IR_RegSet>(result, in));
  env->emit(std::make_unique<IR_RegSet>(sa_in, sa));
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  IRegConstraint sa_con;
  sa_con.ireg = sa_in->ireg();
  sa_con.instr_idx = fenv->code().size();
  sa_con.desired_register = emitter::RCX;

  if (get_math_mode(in->type()) != MathMode::MATH_INT ||
      get_math_mode(sa->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't shift a " + in->type().print() + " by a " + sa->type().print());
  }

  fenv->constrain(sa_con);
  env->emit(std::make_unique<IR_IntegerMath>(kind, result, sa_in));
  return result;
}

Val* Compiler::compile_mod(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  auto fenv = get_parent_env_of_type<FunctionEnv>(env);

  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't mod a " + first->type().print() + " by a " +
                             second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));

  IRegConstraint con;
  con.ireg = result->ireg();
  con.instr_idx = fenv->code().size();
  con.desired_register = emitter::RAX;

  fenv->constrain(con);
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::IMOD_32, result, second));
  return result;
}

Val* Compiler::compile_logand(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't logand a " + first->type().print() + " by a " +
                             second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::AND_64, result, second));
  return result;
}

Val* Compiler::compile_logior(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't logior a " + first->type().print() + " by a " +
                             second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::OR_64, result, second));
  return result;
}

Val* Compiler::compile_logxor(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}, {}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  auto second = compile_error_guard(args.unnamed.at(1), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT ||
      get_math_mode(second->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't logxor a " + first->type().print() + " by a " +
                             second->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::XOR_64, result, second));
  return result;
}

Val* Compiler::compile_lognot(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(env);
  if (get_math_mode(first->type()) != MathMode::MATH_INT) {
    throw std::runtime_error("Can't lognot a " + first->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit(std::make_unique<IR_RegSet>(result, first));
  env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::NOT_64, result, nullptr));
  return result;
}

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
          result->mark_as_settable();
        } else {
          result = fe->alloc_val<MemoryOffsetConstantVal>(field.type, result,
                                                          field.field.offset() + offset);
          result->mark_as_settable();
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
    }
  }

  auto obj = compile_error_guard(arg, env)->to_gpr(env);
  return compile_get_method_of_object(obj, method_name, env);
}
