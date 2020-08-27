#include "Goal.h"

TypeSpec Goal::compile_typespec(const Object& form) {
  if (form.type == SYMBOL) {
    return get_base_typespec(symbol_string(form));
  }

  if (form.type == PAIR) {
    std::vector<TypeSpec> args;
    auto head = compile_typespec(pair_car(form));
    auto rest = pair_cdr(form);

    for_each_in_list(rest, [&](Object o) { args.push_back(compile_typespec(o)); });

    return TypeSpec(head.type, args);
  }

  throw_compile_error(form, "invalid typespec");
  return {};
}

/*!
 * Goal type cast.
 * TODO - cast integer/binteger/float correctly
 */
std::shared_ptr<Place> Goal::compile_the(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  if (rest.type == EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto type = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  if (rest.type == EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto obj = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto desired_ts = compile_typespec(type);
  auto base = compile_error_guard(obj, env);
  std::shared_ptr<Place> result = base;

  if (is_binteger(desired_ts)) {
    if (is_number(base->type)) {
      result = to_binteger(base, env);
    }
  }

  if (is_float(desired_ts)) {
    if (is_number(base->type)) {
      result = to_float(base, env);
    }
  }

  // todo - do we really want all descendants of integer types to do this?
  if (is_integer(desired_ts) && !is_binteger(desired_ts)) {
    if (is_number(base->type)) {
      result = to_integer(base, env);
    }
  }

  /*
    auto original = compile_error_guard(obj, env);
    auto result = env->alloc_reg(get_base_typespec(type.as_symbol()->name));
    env->emit(std::make_shared<IR_Set>(result, original));
   */

  result = std::make_shared<AliasPlace>(desired_ts, result);
  return result;
}

/*!
 * Goal type cast.
 * TODO - cast integer/binteger/float correctly
 */
std::shared_ptr<Place> Goal::compile_the_as(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  if (rest.type == EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto type = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  if (rest.type == EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto obj = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "the must get two args");
  }

  auto desired_ts = compile_typespec(type);
  auto base = compile_error_guard(obj, env);
  std::shared_ptr<Place> result = base;

  result = std::make_shared<AliasPlace>(desired_ts, result);
  return result;
}