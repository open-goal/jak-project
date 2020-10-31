#include "goalc/compiler/Compiler.h"
#include "third-party/fmt/core.h"

Val* Compiler::compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;
  if (!m_debugger.is_valid()) {
    fmt::print("[Debugger] Could not start debugger because there is no valid debugging context\n");
    return get_none();
  }

  if (m_debugger.is_attached()) {
    fmt::print("[Debugger] Could not start debugger because the debugger is already attached.\n");
    return get_none();
  }

  if (m_debugger.attach_and_break()) {
    fmt::print("Debugger connected.\n");
  } else {
    fmt::print("ERROR\n");
  }
  return get_none();
}