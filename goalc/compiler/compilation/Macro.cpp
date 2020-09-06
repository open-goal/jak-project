#include "goalc/compiler/Compiler.h"

using namespace goos;

bool Compiler::try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest) {
  Object macro_obj;
  bool got_macro = false;
  try {
    macro_obj = m_goos.eval_symbol(macro_name, m_goos.goal_env.as_env());
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

Val* Compiler::compile_goos_macro(const goos::Object& o,
                                  const goos::Object& macro_obj,
                                  const goos::Object& rest,
                                  Env* env) {
  auto macro = macro_obj.as_macro();
  Arguments args = m_goos.get_args(o, rest, macro->args);
  auto mac_env_obj = EnvironmentObject::make_new();
  auto mac_env = mac_env_obj.as_env();
  m_goos.set_args_in_env(o, args, macro->args, mac_env);
  m_goos.goal_to_goos.enclosing_method_type =
      get_parent_env_of_type<FunctionEnv>(env)->method_of_type_name;
  auto goos_result = m_goos.eval_list_return_last(macro->body, macro->body, mac_env);
  m_goos.goal_to_goos.reset();
  return compile_error_guard(goos_result, env);
}