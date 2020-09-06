#include "goalc/compiler/Compiler.h"

using namespace goos;

Val * Compiler::compile_top_level(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_begin(form, rest, env);
}

Val * Compiler::compile_begin(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  Val* result = get_none();
  for_each_in_list(rest, [&](const Object& o){
    result = compile_error_guard(o, env);
  });
  return result;
}
