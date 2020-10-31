/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 */

#include <cassert>
#include "Debugger.h"
#include "third-party/fmt/core.h"

/*!
 * Is the target halted? If we don't know or aren't connected, returns false.
 */
bool Debugger::is_halted() const {
  return m_context_valid && m_attached && !m_running;
}

/*!
 * Is the target running and attached?  Note that this returns false if it's running, but not
 * attached to the debugger.
 */
bool Debugger::is_running() const {
  return m_context_valid && m_attached && m_running;
}

/*!
 * Do we have a valid debugging context? Without this we cannot attach or do any debugging.
 */
bool Debugger::is_valid() const {
  return m_context_valid;
}

/*!
 * Invalidate the current debugging context. For example if the target restarts.
 */
void Debugger::invalidate() {
  m_context_valid = false;
}

/*!
 * Are we attached to a valid target?
 */
bool Debugger::is_attached() const {
  return m_context_valid && m_attached;
}

/*!
 * If attached, detach. If halted and attached, will unhalt.
 * Will silently do nothing if we aren't attached.
 */
void Debugger::detach() {
  if (is_valid() && m_attached) {
    xdbg::close_memory(m_debug_context.tid, &m_memory_handle);
    xdbg::detach_and_resume(m_debug_context.tid);
    m_context_valid = false;
    m_attached = false;
  }
  // todo, should we print something if we can't detach?
}

/*!
 * Set the debug context to allow Debugger to attach.
 */
void Debugger::set_context(u32 s7, uintptr_t base, const std::string& thread_id) {
  m_debug_context.s7 = s7;
  m_debug_context.base = base;
  m_debug_context.tid = xdbg::ThreadID(thread_id);
  m_context_valid = true;
}

/*!
 * Get information about the context for debugging the debugger.
 */
std::string Debugger::get_context_string() const {
  return fmt::format("valid = {}, s7 = 0x{:x}, base = 0x{:x}, tid = {}\n", is_valid(),
                     m_debug_context.s7, m_debug_context.base, m_debug_context.tid.to_string());
}

/*!
 * Attach the debugger to the current context (must be valid) and break.
 * Returns once the target actually stops.
 */
bool Debugger::attach_and_break() {
  if (is_valid() && !m_attached) {
    if (xdbg::attach_and_break(m_debug_context.tid)) {
      if (!xdbg::open_memory(m_debug_context.tid, &m_memory_handle)) {
        return false;
      }

      m_attached = true;
      m_running = false;

      xdbg::Regs regs;
      if (!xdbg::get_regs_now(m_debug_context.tid, &regs)) {
        fmt::print("[Debugger] get_regs_now failed after break, something is wrong\n");
      } else {
        fmt::print("{}", regs.print_gprs());
      }
      return true;
    }
  } else {
    fmt::print("[Debugger] attach_and_break can't be done when valid = {} and attached = {}\n",
               is_valid(), m_attached);
  }
  return false;
}

/*!
 * Stop the target. Must be attached and not stopped.
 */
bool Debugger::do_break() {
  assert(is_valid() && is_attached() && is_running());
  if (!xdbg::break_now(m_debug_context.tid)) {
    return false;
  } else {
    m_running = false;
    xdbg::Regs regs;
    if (!xdbg::get_regs_now(m_debug_context.tid, &regs)) {
      fmt::print("[Debugger] get_regs_now failed after break, something is wrong\n");
    } else {
      fmt::print("{}", regs.print_gprs());
    }
    return true;
  }
}

/*!
 * Continue the target, must be attached and stopped.
 */
bool Debugger::do_continue() {
  assert(is_valid() && is_attached() && is_halted());
  if (!xdbg::cont_now(m_debug_context.tid)) {
    return false;
  } else {
    m_running = true;
    return true;
  }
}

bool Debugger::read_memory(u8* dest_buffer, int size, u32 goal_addr) {
  assert(is_valid() && is_attached() && is_halted());
  return xdbg::read_goal_memory(dest_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}

bool Debugger::write_memory(const u8* src_buffer, int size, u32 goal_addr) {
  assert(is_valid() && is_attached() && is_halted());
  return xdbg::write_goal_memory(src_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}