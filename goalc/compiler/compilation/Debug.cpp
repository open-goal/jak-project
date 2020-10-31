#include "goalc/compiler/Compiler.h"
#include "third-party/fmt/core.h"

Val* Compiler::compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;
  auto& ctx = m_listener.get_debug_context();
  if (!ctx.valid) {
    fmt::print("[Debugger] Could not start debugger because there is no valid debugging context\n");
    return get_none();
  }

  if (xdbg::attach_and_break(ctx.tid)) {
    ctx.running = false;
    fmt::print("Debugger connected.\n");
  } else {
    fmt::print("ERROR\n");
  }
  return get_none();
}