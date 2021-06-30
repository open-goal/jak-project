/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 * Uses xdbg functions to debug an OpenGOAL target.
 */

#pragma once

#include <unordered_map>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include "common/common_types.h"
#include "common/cross_os_debug/xdbg.h"
#include "goalc/listener/MemoryMap.h"
#include "DebugInfo.h"

namespace listener {
class Listener;
}

struct BreakInfo {
  u64 real_rip = 0;
  u32 goal_rip = 0;

  bool knows_object = false;
  std::string object_name;
  u8 object_seg = -1;
  u32 object_offset = -1;

  bool knows_function = false;
  std::string function_name;
  u32 function_offset = -1;

  bool disassembly_failed = false;
};

class Debugger {
 public:
  explicit Debugger(listener::Listener* listener) : m_listener(listener) {}
  ~Debugger();
  bool is_halted() const;
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
  void read_symbol_table();
  u32 get_symbol_address(const std::string& sym_name);
  bool get_symbol_value(const std::string& sym_name, u32* output);
  void add_addr_breakpoint(u32 addr);
  void remove_addr_breakpoint(u32 addr);
  void update_break_info();
  DebugInfo& get_debug_info_for_object(const std::string& object_name);
  const BreakInfo& get_cached_break_info() { return m_break_info; }
  std::string get_info_about_addr(u32 addr);

  /*!
   * Get the x86 address of GOAL memory
   */
  u64 get_x86_base_addr() const {
    assert(m_context_valid);
    return m_debug_context.base;
  }

  /*!
   * Get the thread being debugged.
   */
  const xdbg::ThreadID& get_thread_id() const {
    assert(m_context_valid);
    return m_debug_context.tid;
  }

  /*!
   * Are the register values currently stored by the debugger currently accurate?
   */
  bool regs_valid() const { return m_regs_valid; }

  /*!
   * Write a value to GOAL memory
   */
  template <typename T>
  bool write_value(const T& value, u32 goal_addr) {
    return write_memory((const u8*)&value, sizeof(T), goal_addr);
  }

  /*!
   * Read a value from GOAL memory
   */
  template <typename T>
  bool read_value(T* value, u32 goal_addr) {
    return read_memory((u8*)value, sizeof(T), goal_addr);
  }

  const xdbg::Regs& get_regs() {
    assert(m_regs_valid);
    return m_regs_at_break;
  }

 private:
  // how many bytes of instructions to look at ahead of / behind rip when stopping
  static constexpr int INSTR_DUMP_SIZE_REV = 32;
  static constexpr int INSTR_DUMP_SIZE_FWD = 64;

  // symbol table info (all s7-relative offsets)
  std::unordered_map<std::string, s32> m_symbol_name_to_offset_map;
  std::unordered_map<std::string, u32> m_symbol_name_to_value_map;
  std::unordered_map<s32, std::string> m_symbol_offset_to_name_map;

  // debug state
  xdbg::DebugContext m_debug_context;
  xdbg::MemoryHandle m_memory_handle;
  xdbg::Regs m_regs_at_break;

  bool m_watcher_should_stop = false;
  bool m_watcher_running = false;
  bool m_regs_valid = false;

  void start_watcher();
  void stop_watcher();
  void watcher();
  void update_continue_info();

  struct Breakpoint {
    u32 goal_addr = 0;  // address to break at
    int id = -1;        // breakpoint ID
    u8 old_data = 0;    // byte originally stored at goal_addr
  };

  bool m_expecting_immeidate_break = false;

  std::unordered_map<u32, Breakpoint> m_addr_breakpoints;

  std::mutex m_watcher_mutex;
  std::condition_variable m_watcher_cv;
  std::thread m_watcher_thread;

  struct ContinueInfo {
    bool subtract_1 = false;
    bool valid = false;
    bool is_addr_breakpiont = false;
    Breakpoint addr_breakpoint;
  } m_continue_info;

  // for more complicated breakpoint stuff, we have a queue of stops.
  // right now it's barely used for anything other than waiting for a "break" to be acknowledged.
  struct SignalInfo {
    xdbg::SignalInfo::Kind kind;
  };
  std::queue<SignalInfo> m_watcher_queue;
  bool try_pop_signal(SignalInfo* out);
  SignalInfo pop_signal();
  int get_signal_count();
  void clear_signal_queue();

  bool m_context_valid = false;
  bool m_running = true;
  bool m_attached = false;

  BreakInfo m_break_info;

  listener::Listener* m_listener = nullptr;
  listener::MemoryMap m_memory_map;
  std::unordered_map<std::string, DebugInfo> m_debug_info;
};
