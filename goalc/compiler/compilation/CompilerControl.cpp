#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

Val* Compiler::compile_exit(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {}, {});
  if (m_listener.is_connected()) {
    m_listener.send_reset(false);
  }
  m_want_exit = true;
  return get_none();
}

Val* Compiler::compile_seval(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  try {
    for_each_in_list(rest, [&](const goos::Object& o) {
      m_goos.eval_with_rewind(o, m_goos.global_environment.as_env());
    });
  } catch (std::runtime_error& e) {
    throw_compile_error(form, std::string("seval error: ") + e.what());
  }
  return get_none();
}