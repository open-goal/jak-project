/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 */

#pragma once

#include "common/common_types.h"
#include "common/cross_os_debug/xdbg.h"

class Debugger {
 public:
  Debugger() = default;
  bool is_halted() const;  // are we halted?
  bool is_valid() const;
  bool is_attached() const;
  bool is_running() const;
  void detach();
  void invalidate();
  void set_context(u32 s7, uintptr_t base, const std::string& thread_id);
  std::string get_context_string() const;

  bool attach_and_break();

  bool do_break();
  bool do_continue();

  bool read_memory(u8* dest_buffer, int size, u32 goal_addr);
  bool write_memory(const u8* src_buffer, int size, u32 goal_addr);

  template <typename T>
  bool write_value(const T& value, u32 goal_addr) {
    return write_memory((const u8*)&value, sizeof(T), goal_addr);
  }

  template <typename T>
  bool read_value(T* value, u32 goal_addr) {
    return read_memory((u8*)value, sizeof(T), goal_addr);
  }

 private:
  xdbg::DebugContext m_debug_context;
  xdbg::MemoryHandle m_memory_handle;
  bool m_context_valid = false;
  bool m_running = true;
  bool m_attached = false;
};
