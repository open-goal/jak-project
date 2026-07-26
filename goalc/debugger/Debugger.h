/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 * Uses xdbg functions to debug an OpenGOAL target.
 */

#pragma once

#include <atomic>
#include <condition_variable>
#include <cstring>
#include <functional>
#include <mutex>
#include <optional>
#include <queue>
#include <thread>
#include <unordered_map>

#include "DebugInfo.h"

#include "common/common_types.h"
#include "common/cross_os_debug/xdbg.h"
#include "common/versions/versions.h"

#include "goalc/listener/MemoryMap.h"

namespace listener {
class Listener;
}

/*!
 * Information about an instruction pointer, used for constructing a useful disassembly around it.
 */
struct InstructionPointerInfo {
  u64 real_rip = 0;  // x86-64 rip register value (64-bits)
  u32 goal_rip = 0;  // GOAL pointer of rip.

  u64 real_rsp = 0;

  bool in_goal_mem = false;

  bool knows_object = false;
  std::string object_name;
  u8 object_seg = -1;
  u32 object_offset = -1;

  bool knows_function = false;
  std::string function_name;
  u32 function_offset = -1;

  std::optional<listener::MemoryMapEntry> map_entry;

  FunctionDebugInfo* func_debug = nullptr;
};

struct Disassembly {
  std::string text;
  bool failed = false;
};

struct BacktraceFrame {
  InstructionPointerInfo rip_info;
  u64 rsp_at_rip = 0;
};

struct SourceLocation {
  std::string filename;
  int line = -1;
  int column = -1;
  std::string line_text;
};

struct ResolvedBreakpoint {
  u32 goal_addr = 0;
  int line = -1;
  std::string function_name;
  std::string object_name;
  bool loaded = false;
};

struct SourceStackFrame {
  std::string function_name;
  std::string object_name;
  u64 rip = 0;
  u32 goal_rip = 0;
  u64 rsp = 0;
  std::optional<SourceLocation> source;
};

struct LiveVariable {
  std::string name;
  TypeSpec type;
  bool is_parameter = false;

  bool in_register = false;
  int reg = -1;        //! emitter::Register id, when in_register
  u32 stack_addr = 0;  //! GOAL address of the value, when spilled to the stack
};

enum class StepKind { OVER, INTO, OUT };

class Debugger {
 public:
  explicit Debugger(listener::Listener* listener, const goos::Reader* reader, GameVersion version)
      : m_listener(listener), m_reader(reader), m_version(version) {}
  ~Debugger();
  bool is_halted() const;
  bool is_valid() const;
  bool is_attached() const;
  bool is_running() const;
  bool detach();
  void invalidate();
  void set_context(u32 s7, uintptr_t base, const std::string& thread_id);
  std::string get_context_string() const;
  bool attach_and_break();
  bool do_break();
  bool do_continue();
  bool read_memory(u8* dest_buffer, int size, u32 goal_addr) const;
  bool read_memory_if_safe(u8* dest_buffer, int size, u32 goal_addr) const;
  template <typename T>
  bool read_memory_if_safe(T* dst, u32 goal_addr) const {
    u8 temp[sizeof(T)];
    if (read_memory_if_safe(temp, sizeof(T), goal_addr)) {
      memcpy(dst, temp, sizeof(T));
      return true;
    }
    return false;
  }
  bool write_memory(const u8* src_buffer, int size, u32 goal_addr);
  void read_symbol_table();
  void read_symbol_table_jak1();
  void read_symbol_table_jak2();
  void read_symbol_table_jak3();
  u32 get_symbol_address(const std::string& sym_name);
  bool get_symbol_value(const std::string& sym_name, u32* output);
  const char* get_symbol_name_from_offset(s32 ofs) const;
  void add_addr_breakpoint(u32 addr);
  void remove_addr_breakpoint(u32 addr);
  void update_break_info(std::optional<std::string> dump_path);
  bool refresh_break_state();

  InstructionPointerInfo get_rip_info(u64 x86_rip);
  DebugInfo& get_debug_info_for_object(const std::string& object_name);
  bool knows_object(const std::string& object_name) const;
  const InstructionPointerInfo& get_cached_break_info() { return m_break_info; }
  std::string get_info_about_addr(u32 addr);
  Disassembly disassemble_at_rip(const InstructionPointerInfo& info);

  std::vector<BacktraceFrame> get_backtrace(u64 rip,
                                            u64 rsp,
                                            std::optional<std::string> dump_path,
                                            bool quiet = false);
  std::optional<SourceLocation> get_source_location(u32 goal_addr);
  std::optional<std::string> get_type_name_of_basic(u32 goal_addr);
  std::optional<std::string> get_symbol_name_for_value(u32 value) const;
  std::optional<std::string> get_symbol_name_at_address(u32 goal_addr) const;

  std::vector<ResolvedBreakpoint> resolve_source_breakpoint(const std::string& filename,
                                                            int line,
                                                            int max_line_slide = 50);

  std::vector<SourceStackFrame> get_source_stack_frames(int max_frames = 128);
  std::vector<LiveVariable> get_live_variables();

  bool resume_from_break();

  bool do_step(StepKind kind);

  void set_stop_callback(std::function<void(xdbg::SignalInfo::Kind)> cb) {
    std::lock_guard<std::mutex> lock(m_stop_callback_mutex);
    m_stop_callback = std::move(cb);
  }

  u64 get_normalized_rip() const;
  std::optional<u32> get_breakpoint_addr_at_stop() const;

  void set_suppress_stop_reporting(bool suppress) { m_suppress_stop_reporting = suppress; }

  std::string disassemble_x86_with_symbols(int len, u64 base_addr) const;

  /*!
   * Get the x86 address of GOAL memory
   */
  u64 get_x86_base_addr() const {
    ASSERT(m_context_valid);
    return m_debug_context.base;
  }

  /*!
   * Get the thread being debugged.
   */
  const xdbg::ThreadID& get_thread_id() const {
    ASSERT(m_context_valid);
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
    ASSERT(m_regs_valid);
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
  bool m_attach_response = false;
  bool m_attach_return = false;
  std::condition_variable m_attach_cv;

  bool try_start_watcher();
  void start_watcher();
  void stop_watcher();
  void watcher();
  void reload_break_state();
  void update_continue_info();
  void handle_disappearance();

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

  ContinueInfo get_continue_info(u64 rip) const;

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

  std::atomic_bool m_suppress_stop_reporting{false};

  std::mutex m_stop_callback_mutex;
  std::function<void(xdbg::SignalInfo::Kind)> m_stop_callback;
  void fire_stop_callback(xdbg::SignalInfo::Kind kind);
  bool normalize_rip_after_break();
  bool single_step_once();
  bool run_to_addr(u32 goal_addr);
  std::optional<u64> get_return_address_of_current_frame();
  void place_breakpoints();
  void remove_breakpoints();
  std::optional<SourceLocation> source_location_for_function_offset(const FunctionDebugInfo& func,
                                                                    u32 function_offset) const;

  InstructionPointerInfo m_break_info;

  listener::Listener* m_listener = nullptr;
  const goos::Reader* m_reader = nullptr;
  listener::MemoryMap m_memory_map;
  std::unordered_map<std::string, DebugInfo> m_debug_info;
  GameVersion m_version;
};
