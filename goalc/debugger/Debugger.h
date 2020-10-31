#pragma once

#include "common/common_types.h"
#include "common/cross_os_debug/xdbg.h"

class Debugger {
 public:
  Debugger() = default;
  bool is_halted() const; // are we halted?
  bool is_valid() const;
  bool is_attached() const;
  bool is_running() const;
  void detach();
  void invalidate();
  void set_context(u32 s7, uintptr_t base, const std::string& thread_id);
  std::string get_context_string() const;

  bool attach_and_break();

 private:
  xdbg::DebugContext m_debug_context;
  bool m_context_valid = false;
  bool m_running = true;
  bool m_attached = false;
};
