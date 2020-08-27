/*!
 * @file GoalConditionalCompilation.cpp
 *
 * Compiler forms to omit certain forms from compilation in certain cases.
 */

#include "Goal.h"

/*!
 * A "cond" form evaluated at compile time using GOOS conditions.
 */
std::shared_ptr<Place> Goal::compile_gscond(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env) {
  if (rest.type != PAIR)
    throw_compile_error(form, "#cond must have at least one clause, which must be a form");

  Object lst = rest;
  for (;;) {
    if (lst.type == PAIR) {
      Object current_case = lst.as_pair()->car;
      if (current_case.type != PAIR)
        throw_compile_error(lst, "Bad case in #cond");

      // check condition:
      Object condition_result =
          goos.eval_with_rewind(current_case.as_pair()->car, goos.global_environment.as_env());
      if (truthy(condition_result)) {
        if (current_case.as_pair()->cdr.type == EMPTY_LIST) {
          return get_none();
        }
        // got a match!
        auto result = get_none();

        for_each_in_list(current_case.as_pair()->cdr,
                         [&](Object o) { result = compile_error_guard(o, env); });
        return result;
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.type == EMPTY_LIST) {
      return get_none();
    } else {
      throw_compile_error(form, "malformed #cond");
    }
  }
}

/*!
 * Evaluate GOOS at compile time.
 */
std::shared_ptr<Place> Goal::compile_seval(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  (void)env;
  try {
    for_each_in_list(rest,
                     [&](Object o) { goos.eval_with_rewind(o, goos.global_environment.as_env()); });
  } catch (std::runtime_error& e) {
    throw_compile_error(form, std::string("seval error: ") + e.what());
  }
  return get_none();
}

/*!
 * Define a GOAL and GOOS constant.
 */
std::shared_ptr<Place> Goal::compile_defglobalconstant(const Object& form,
                                                       Object rest,
                                                       std::shared_ptr<GoalEnv> env) {
  (void)env;
  if (rest.type != PAIR) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  auto sym = as_symbol_obj(pair_car(rest));
  rest = pair_cdr(rest);
  auto value = pair_car(rest);

  rest = rest.as_pair()->cdr;
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  // GOAL constant
  global_constants[sym] = value;

  // GOOS constant
  goos.global_environment.as_env()->vars[sym] = value;

  return get_none();
}
