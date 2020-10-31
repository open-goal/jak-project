#include "Debugger.h"
#include "third-party/fmt/core.h"

bool Debugger::is_halted() const {
  return m_context_valid && !m_running;
}

bool Debugger::is_valid() const {
  return m_context_valid;
}

void Debugger::invalidate() {
  m_context_valid = false;
}

bool Debugger::is_attached() const {
  return m_context_valid && m_attached;
}

void Debugger::detach() {
  if (is_valid() && m_attached) {
    xdbg::detach_and_resume(m_debug_context.tid);
    m_context_valid = false;
    m_attached = false;
  }
  // todo, should we print something if we can't detach?
}

void Debugger::set_context(u32 s7, uintptr_t base, const std::string& thread_id) {
  m_debug_context.s7 = s7;
  m_debug_context.base = base;
  m_debug_context.tid = xdbg::ThreadID(thread_id);
  m_context_valid = true;
}

std::string Debugger::get_context_string() const {
  return fmt::format("valid = {}, s7 = 0x{:x}, base = 0x{:x}, tid = {}\n", is_valid(),
                     m_debug_context.s7, m_debug_context.base, m_debug_context.tid.to_string());
}

bool Debugger::attach_and_break() {
  if (is_valid() && !m_attached) {
    if (xdbg::attach_and_break(m_debug_context.tid)) {
      m_attached = true;
      m_running = false;
      return true;
    }
  } else {
    fmt::print("[Debugger] attach_and_break can't be done when valid = {} and attached = {}\n",
               is_valid(), m_attached);
  }
  return false;
}