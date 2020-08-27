#include "Goal.h"

std::shared_ptr<Place> Goal::compile_car(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  auto arg = pair_car(rest);
  rest = pair_cdr(rest);
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "can't do this car");
  }

  return std::make_shared<PairPlace>(get_base_typespec("object"), true,
                                     compile_error_guard(arg, env));
}

std::shared_ptr<Place> Goal::compile_cdr(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  auto arg = pair_car(rest);
  rest = pair_cdr(rest);
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "can't do this cdr");
  }

  return std::make_shared<PairPlace>(get_base_typespec("object"), false,
                                     compile_error_guard(arg, env));
}