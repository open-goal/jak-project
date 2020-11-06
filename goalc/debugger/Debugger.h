/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 */

#pragma once

#include <unordered_map>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include "common/common_types.h"
#include "common/cross_os_debug/xdbg.h"

class Debugger {
 public:
  Debugger() = default;
  ~Debugger();
  bool is_halted() const;  // are we halted?
  bool is_valid() const;
  bool is_attached() const;
  bool is_running() const;
  void detach();
  void invalidate();
  void set_context(u32 s7, uintptr_t base, const std::string& thread_id);
  std::string get_context_string() const;
  u64 get_x86_base_addr() const {
    assert(m_context_valid);
    return m_debug_context.base;
  }

  const xdbg::ThreadID& get_thread_id() {
    assert(m_context_valid);
    return m_debug_context.tid;
  }

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

  void read_symbol_table();

  u32 get_symbol_address(const std::string& sym_name);
  bool get_symbol_value(const std::string& sym_name, u32* output);

  bool regs_valid() const { return m_regs_valid; }

  void add_addr_breakpoint(u32 addr);
  void remove_addr_breakpoint(u32 addr);

  void read_symbols_and_regs();

 private:
  static constexpr int INSTR_DUMP_SIZE_REV = 32;
  static constexpr int INSTR_DUMP_SIZE_FWD = 64;
  std::unordered_map<std::string, s32> m_symbol_name_to_offset_map;
  std::unordered_map<std::string, u32> m_symbol_name_to_value_map;
  std::unordered_map<s32, std::string> m_symbol_offset_to_name_map;
  xdbg::DebugContext m_debug_context;
  xdbg::MemoryHandle m_memory_handle;
  xdbg::Regs m_regs_at_break;

  bool m_watcher_should_stop = false;
  bool m_watcher_running = false;
  bool m_regs_valid = false;

  void start_watcher();
  void stop_watcher();

  void watcher();

  struct SignalInfo {
    xdbg::SignalInfo::Kind kind;
  };

  struct Breakpoint {
    u32 goal_addr = 0;
    int id = -1;
    u8 old_data = 0;
  };

  std::unordered_map<u32, Breakpoint> m_addr_breakpoints;

  std::queue<SignalInfo> m_watcher_queue;
  std::mutex m_watcher_mutex;
  std::condition_variable m_watcher_cv;
  std::thread m_watcher_thread;

  bool try_pop_signal(SignalInfo* out);
  SignalInfo pop_signal();
  int get_signal_count();
  void clear_signal_queue();

  bool m_context_valid = false;
  bool m_running = true;
  bool m_attached = false;
};
