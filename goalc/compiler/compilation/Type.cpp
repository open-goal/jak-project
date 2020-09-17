#include "goalc/compiler/Compiler.h"
#include "common/type_system/deftype.h"

Val* Compiler::compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)env;

  parse_deftype(rest, &m_ts);

  return get_none();
}
