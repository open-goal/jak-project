#include "state.h"

/*!
 * Convert a (state <blah> ...) to the function required to go. Must be state.
 */
TypeSpec state_to_go_function(const TypeSpec& state_type) {
  assert(state_type.base_type() == "state");
  std::vector<TypeSpec> arg_types;
  for (int i = 0; i < (int)state_type.arg_count() - 1; i++) {
    arg_types.push_back(state_type.get_arg(i));
  }

  arg_types.push_back(TypeSpec("none"));  // none for the return.
  return TypeSpec("function", arg_types);
}