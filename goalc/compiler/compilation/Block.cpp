/*!
 * @file Block.cpp
 * Compiler implementation for blocks / gotos / labels
 */

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

using namespace goos;

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
      result = result->to_reg(o, env);
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
    throw_compiler_error(form, "Block form has an empty or invalid body");
  }

  auto fe = env->function_env();

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
      result = result->to_reg(o, env);
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

  if (!dynamic_cast<None*>(result)) {
    // an IR to move the result of the block into the block's return register (if no return-from's
    // are taken)
    auto ir_move_rv =
        std::make_unique<IR_RegSet>(block_env->return_value, result->to_gpr(form, fe));

    // note - one drawback of doing this single pass is that a block always evaluates to a gpr.
    // so we may have an unneeded xmm -> gpr move that could have been an xmm -> xmm that could have
    // been eliminated.
    env->emit(form, std::move(ir_move_rv));
  }

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
  auto fe = env->function_env();

  // find block to return from
  auto block = dynamic_cast<BlockEnv*>(env->find_block(block_name));
  if (!block) {
    throw_compiler_error(form, "The return-from form was unable to find a block named {}.",
                         block_name);
  }

  // move result into return register
  auto ir_move_rv = std::make_unique<IR_RegSet>(block->return_value, result->to_gpr(form, fe));

  // inform block of our possible return type
  block->return_types.push_back(result->type());

  env->emit(form, std::move(ir_move_rv));

  // jump to end of block (by label object)
  auto ir_jump = std::make_unique<IR_GotoLabel>(&block->end_label);
  env->emit(form, std::move(ir_jump));

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
    throw_compiler_error(form, "There are two labels named \"{}\" in the same label environment",
                         label_name);
  }

  // make a label pointing to the end of the current function env. safe because we'll always add
  // a terminating "null" instruction at the end.
  auto func_env = env->function_env();
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
  env->function_env()->unresolved_gotos.push_back({ir_goto.get(), label_name});
  env->emit(form, std::move(ir_goto));
  return get_none();
}

Val* Compiler::compile_nop(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  env->emit_ir<IR_Nop>(form);
  return get_none();
}