/*!
 * @file GoalBlockForms.cpp
 * GOAL Compiler Forms related to blocks.
 */

#include "Goal.h"
#include "GoalEnv.h"
#include "util.h"

/*!
 * Compile a list of forms.  The top-level form is the same as a begin, but top-levels are generated
 * by the reader when reading a file.
 */
std::shared_ptr<Place> Goal::compile_top_level(const Object& form,
                                               Object rest,
                                               std::shared_ptr<GoalEnv> env) {
  return compile_begin(form, rest, env);
}

/*!
 * Compile begin statement.  Just compile everything in order and return the last thing.
 * If there's nothing in it, return none.
 */
std::shared_ptr<Place> Goal::compile_begin(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  (void)form;
  std::shared_ptr<Place> result = get_none();
  for_each_in_list(rest, [&](Object o) { result = compile_error_guard(o, env); });
  return result;
}

/*!
 * Compile a block statement.
 * This pushes a block environment and is like a begin.
 * It returns the last thing in the list, unless you jump to the end with a return-from.
 * The type of the return-from's are checked!
 */
std::shared_ptr<Place> Goal::compile_block(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  auto name = pair_car(rest);
  rest = pair_cdr(rest);

  if (rest.type != PAIR) {
    throw_compile_error(form, "Block form has an empty body");
  }

  // create environment
  auto block_env = std::make_shared<BlockEnv>(env, symbol_string(name));

  // we need to create a return value register, as a "return-from" statement inside the block may
  // set it. for now it has a type of none, but we will set it more accurate after compiling the
  // block.
  block_env->return_value = env->alloc_reg(get_base_typespec("none"));

  // create label to the end of the block (we don't yet know where it is...)
  block_env->end_label = std::make_shared<Label>(get_parent_env_of_type<FunctionEnv>(env).get());

  // compile everything in the body
  std::shared_ptr<Place> result = get_none();
  for_each_in_list(rest, [&](Object o) { result = compile_error_guard(o, block_env); });

  // if no return-from's were used, we can ignore the return_value register, and basically turn this
  // into a begin. this allows a block which returns a floating point value to return the value in
  // an xmm register, which is likely to eliminate a gpr->xmm move.
  if (block_env->return_types.empty()) {
    return result;
  }

  // determine return type as the lowest common ancestor of the block's last form and any
  // return-from's
  auto& return_types = block_env->return_types;
  return_types.push_back(result->type);
  auto return_type = lowest_common_ancestor(return_types);

  // an IR to move the result of the block into the block's return register (if no return-from's are
  // taken)
  auto ir_move_rv = make_unique<IR_Set>();
  ir_move_rv->dest = block_env->return_value;
  ir_move_rv->dest->type = return_type;

  // note - one drawback of doing this single pass is that a block always evaluates to a gpr.
  // so we may have an unneeded xmm -> gpr move that could have been an xmm -> xmm that could have
  // been eliminated.
  ir_move_rv->src = resolve_to_gpr_or_xmm(result, env);
  env->emit(std::move(ir_move_rv));

  // now we know the end of the block, so we set the label index to be on whatever comes after the
  // return move. functions always end with a "null" IR and "null" instruction, so this is safe.
  block_env->end_label->idx = block_env->end_label->func->code.size();

  return block_env->return_value;
}

/*!
 * Compile a return-from statement.
 * Note that doing a "return-from" will affect the return type of the block
 */
std::shared_ptr<Place> Goal::compile_return_from(const Object& form,
                                                 Object rest,
                                                 std::shared_ptr<GoalEnv> env) {
  auto block_name = symbol_string(pair_car(rest));
  rest = pair_cdr(rest);
  auto value_expression = pair_car(rest);
  expect_empty_list(pair_cdr(rest));

  // evaluate expression to return
  auto result = compile_error_guard(value_expression, env);

  // find block to return from
  auto block = dynamic_cast<BlockEnv*>(env->find_block(block_name));
  if (!block) {
    throw_compile_error(form,
                        "The return-from form was unable to find a block named " + block_name);
  }

  // move result into return register
  auto ir_move_rv = make_unique<IR_Set>();
  ir_move_rv->dest = block->return_value;
  ir_move_rv->src = resolve_to_gpr_or_xmm(result, env);

  // inform block of our possible return type
  block->return_types.push_back(result->type);

  env->emit(std::move(ir_move_rv));

  // jump to end of block
  auto ir_jump = make_unique<IR_Goto_Label>();
  ir_jump->label = block->end_label;
  // we know this label is a real label. even though end_label doesn't know where it is, there is an
  // actual label object. this means we won't try to resolve this label _by name_ later on when the
  // block is done.
  ir_jump->resolved = true;
  env->emit(std::move(ir_jump));

  // In the real GOAL, there is likely a bug here where a non-none value is returned.
  return get_none();
}

/*!
 * Compile a label statement
 */
std::shared_ptr<Place> Goal::compile_label(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  auto label_name = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  // make sure we don't have a label with this name already
  auto& labels = env->get_label_map();
  auto kv = labels.find(label_name);
  if (kv != labels.end()) {
    throw_compile_error(
        form, "There are two labels named " + label_name + " in the same label environment");
  }

  // make a label pointing to the end of the current function env.
  auto func_env = get_parent_env_of_type<FunctionEnv>(env);
  auto new_label = std::make_shared<Label>(func_env.get(), func_env->code.size());

  labels[label_name] = new_label;
  return get_none();
}

/*!
 * Compile a goto
 */
std::shared_ptr<Place> Goal::compile_goto(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  (void)form;
  auto label_name = symbol_string(pair_car(rest));
  expect_empty_list(pair_cdr(rest));

  auto ir_goto = make_unique<IR_Goto_Label>();
  // this requires looking up the label by name after, as it may be a goto to a label which has not
  // yet been defined.
  ir_goto->resolved = false;

  // add this goto to the list of gotos to resolve after the function is done.
  // it's safe to have this reference, as the FunctionEnv also owns the goto.
  get_parent_env_of_type<FunctionEnv>(env)->unresolved_gotos.push_back({ir_goto.get(), label_name});
  env->emit(std::move(ir_goto));
  return get_none();
}