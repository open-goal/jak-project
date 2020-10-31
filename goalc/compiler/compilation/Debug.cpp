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

Val* Compiler::compile_dbs(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  fmt::print(" Listener connected? {}\n", m_listener.is_connected());
  fmt::print(" Debugger context? {}\n", m_debugger.is_valid());
  if (m_debugger.is_valid()) {
    fmt::print(" Attached? {}\n", m_debugger.is_attached());

    if (m_debugger.is_attached()) {
      fmt::print(" Halted? {}\n", m_debugger.is_halted());
    }

    fmt::print(" Context: {}\n", m_debugger.get_context_string());
  }

  if (m_debugger.is_valid()) {
  } else {
    fmt::print("There is no valid debug context from the target.");
  }

  return get_none();
}

Val* Compiler::compile_cont(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  if (m_debugger.is_valid() && m_debugger.is_attached() && m_debugger.is_halted()) {
    m_debugger.do_continue();
  } else {
    fmt::print("Couldn't do :cont. Valid {}, attached {}, halted {}\n", m_debugger.is_valid(),
               m_debugger.is_attached(), m_debugger.is_halted());
  }

  return get_none();
}

Val* Compiler::compile_break(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  if (m_debugger.is_valid() && m_debugger.is_attached() && m_debugger.is_running()) {
    m_debugger.do_break();
  } else {
    fmt::print("Couldn't do :break. Valid {}, attached {}, running {}\n", m_debugger.is_valid(),
               m_debugger.is_attached(), m_debugger.is_running());
  }

  return get_none();
}