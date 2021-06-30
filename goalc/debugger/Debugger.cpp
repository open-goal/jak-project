/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 * Uses xdbg functions to debug an OpenGOAL target.
 */

#include "common/util/assert.h"
#include "Debugger.h"
#include "common/util/Timer.h"
#include "common/goal_constants.h"
#include "common/symbols.h"
#include "third-party/fmt/core.h"
#include "goalc/debugger/disassemble.h"
#include "goalc/listener/Listener.h"

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
 * Will silently do nothing if we aren't attached, so it is safe to just call detach() to try to
 * clean up when exiting.
 */
void Debugger::detach() {
  if (is_valid() && m_attached) {
    stop_watcher();
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
    // reset and start the stop watcher
    clear_signal_queue();
    start_watcher();

    // attach and send a break command
    if (xdbg::attach_and_break(m_debug_context.tid)) {
      // wait for the signal queue to get a stop and pop it.
      auto info = pop_signal();

      // manually set up continue for this.
      m_continue_info.valid = true;
      m_continue_info.subtract_1 = false;

      // this may fail if you crash at exactly the wrong time. todo - remove?
      assert(info.kind == xdbg::SignalInfo::BREAK);

      // open the memory of the process
      if (!xdbg::open_memory(m_debug_context.tid, &m_memory_handle)) {
        return false;
      }

      m_attached = true;
      m_running = false;

      // get info from target
      update_break_info();

      auto signal_count = get_signal_count();
      assert(signal_count == 0);
      return true;
    }
  } else {
    fmt::print("[Debugger] attach_and_break can't be done when valid = {} and attached = {}\n",
               is_valid(), m_attached);
  }
  return false;
}

std::string Debugger::get_info_about_addr(u32 addr) {
  if (addr >= EE_MAIN_MEM_LOW_PROTECT && addr < EE_MAIN_MEM_SIZE) {
    auto map_loc = m_memory_map.lookup(addr);
    if (map_loc.empty) {
      return "Unknown Address";
    }
    std::string result = fmt::format("Object: {}\n", map_loc.obj_name);
    u64 obj_offset = addr - map_loc.start_addr;
    FunctionDebugInfo* info = nullptr;
    std::string name;
    if (get_debug_info_for_object(map_loc.obj_name)
            .lookup_function(&info, &name, obj_offset, map_loc.seg_id)) {
      result += fmt::format("Name: {}\n", name);
    }
    return result;
  } else {
    return "Outside of GOAL memory";
  }
}

/*!
 * Read the registers, symbol table, and instructions near rip.
 * Print out some info about where we are.
 */
void Debugger::update_break_info() {
  // todo adjust rip if break instruction????

  m_memory_map = m_listener->build_memory_map();
  // fmt::print("{}", m_memory_map.print());
  read_symbol_table();
  m_regs_valid = false;
  if (!xdbg::get_regs_now(m_debug_context.tid, &m_regs_at_break)) {
    fmt::print("[Debugger] get_regs_now failed after break, something is wrong\n");
  } else {
    m_regs_valid = true;
    fmt::print("{}", m_regs_at_break.print_gprs());
  }

  if (regs_valid()) {
    std::vector<u8> mem;
    mem.resize(INSTR_DUMP_SIZE_REV + INSTR_DUMP_SIZE_FWD);
    // very basic asm dump.
    auto rip = m_regs_at_break.rip;
    m_break_info.real_rip = rip;
    m_break_info.goal_rip = rip - m_debug_context.base;

    m_break_info.disassembly_failed = false;

    if (rip >= m_debug_context.base + EE_MAIN_MEM_LOW_PROTECT &&
        rip < m_debug_context.base + EE_MAIN_MEM_SIZE) {
      read_memory(mem.data(), INSTR_DUMP_SIZE_REV + INSTR_DUMP_SIZE_FWD,
                  rip - m_debug_context.base - INSTR_DUMP_SIZE_REV);
      auto map_loc = m_memory_map.lookup(rip - m_debug_context.base);
      if (map_loc.empty) {
        fmt::print("In unknown code\n");
        fmt::print("{}", disassemble_x86(mem.data(), mem.size(), rip - INSTR_DUMP_SIZE_REV, rip));
        m_break_info.disassembly_failed = true;
        m_break_info.knows_object = false;
        m_break_info.knows_function = false;
      } else {
        u64 obj_offset = rip - m_debug_context.base - map_loc.start_addr;
        m_break_info.knows_object = true;
        m_break_info.object_name = map_loc.obj_name;
        m_break_info.object_seg = map_loc.seg_id;
        m_break_info.object_offset = obj_offset;

        FunctionDebugInfo* info = nullptr;
        std::string name;

        if (get_debug_info_for_object(map_loc.obj_name)
                .lookup_function(&info, &name, obj_offset, map_loc.seg_id)) {
          update_continue_info();
          m_break_info.knows_function = true;
          m_break_info.function_name = name;
          m_break_info.function_offset = obj_offset - info->offset_in_seg;

          assert(!info->instructions.empty());

          std::vector<u8> function_mem;
          function_mem.resize(info->instructions.back().offset +
                              info->instructions.back().instruction.length());
          read_memory(function_mem.data(), function_mem.size(),
                      map_loc.start_addr + info->offset_in_seg);

          int rip_offset = 0;
          if (m_continue_info.valid && m_continue_info.is_addr_breakpiont) {
            int offset_in_fmem = uint64_t(m_continue_info.addr_breakpoint.goal_addr) -
                                 uint64_t(map_loc.start_addr + info->offset_in_seg);
            if (offset_in_fmem < 0 || offset_in_fmem >= int(function_mem.size())) {
              m_break_info.disassembly_failed = true;
            } else {
              function_mem.at(offset_in_fmem) = m_continue_info.addr_breakpoint.old_data;
              rip_offset = -1;
            }
          }

          fmt::print(
              "In function {} in segment {} of obj {}, offset_obj 0x{:x}, offset_func 0x{:x}\n",
              name, map_loc.seg_id, map_loc.obj_name, obj_offset, m_break_info.function_offset);

          fmt::print("{}", disassemble_x86_function(
                               function_mem.data(), function_mem.size(),
                               m_debug_context.base + map_loc.start_addr + info->offset_in_seg,
                               rip + rip_offset, info->instructions, info->irs,
                               &m_break_info.disassembly_failed));

        } else {
          m_break_info.disassembly_failed = true;
          m_break_info.knows_function = false;
          fmt::print("In segment {} of obj {}, offset 0x{:x}\n", map_loc.seg_id, map_loc.obj_name,
                     obj_offset);
          fmt::print("{}", disassemble_x86(mem.data(), mem.size(), rip - INSTR_DUMP_SIZE_REV, rip));
        }
      }
    } else {
      m_break_info.disassembly_failed = true;
      fmt::print("Not in GOAL code!\n");
    }
  }
}

/*!
 * Stop the target. Must be attached and not stopped.
 * Waits for break to be acknowledged and reads break info.
 */
bool Debugger::do_break() {
  assert(is_valid() && is_attached() && is_running());
  m_expecting_immeidate_break = true;
  m_continue_info.valid = false;
  clear_signal_queue();
  if (!xdbg::break_now(m_debug_context.tid)) {
    return false;
  } else {
    auto info = pop_signal();
    assert(info.kind == xdbg::SignalInfo::BREAK);
    update_break_info();
    m_running = false;
    return true;
  }
}

/*!
 * Continue the target, must be attached and stopped.
 */
bool Debugger::do_continue() {
  assert(is_valid() && is_attached() && is_halted());
  if (!m_regs_valid) {
    update_break_info();
  }
  assert(regs_valid());

  if (!m_continue_info.valid) {
    update_continue_info();
  }
  assert(m_continue_info.valid);
  m_regs_valid = false;

  if (m_continue_info.subtract_1) {
    m_regs_at_break.rip--;
    auto result = xdbg::set_regs_now(m_debug_context.tid, m_regs_at_break);
    assert(result);
  }

  m_expecting_immeidate_break = false;
  if (!xdbg::cont_now(m_debug_context.tid)) {
    return false;
  } else {
    m_running = true;
    return true;
  }
}

/*!
 * Read memory from an attached and halted target.
 */
bool Debugger::read_memory(u8* dest_buffer, int size, u32 goal_addr) {
  assert(is_valid() && is_attached() && is_halted());
  return xdbg::read_goal_memory(dest_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}

/*!
 * Write the memory of an attached and halted target.
 */
bool Debugger::write_memory(const u8* src_buffer, int size, u32 goal_addr) {
  assert(is_valid() && is_attached() && is_halted());
  return xdbg::write_goal_memory(src_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}

/*!
 * Read the GOAL Symbol table from an attached and halted target.
 */
void Debugger::read_symbol_table() {
  assert(is_valid() && is_attached() && is_halted());
  u32 bytes_read = 0;
  u32 reads = 0;
  Timer timer;

  u32 st_base = m_debug_context.s7 - ((GOAL_MAX_SYMBOLS / 2) * 8 + BASIC_OFFSET);
  u32 empty_pair_offset = (m_debug_context.s7 + FIX_SYM_EMPTY_PAIR - PAIR_OFFSET) - st_base;

  std::vector<u8> mem;
  mem.resize(0x20000);

  if (!xdbg::read_goal_memory(mem.data(), 0x20000, st_base, m_debug_context, m_memory_handle)) {
    fmt::print("Read failed during read_symbol_table\n");
    return;
  }
  reads++;
  bytes_read += 0x20000;

  struct SymLower {
    u32 type;
    u32 value;
  };

  struct SymUpper {
    u32 hash;
    u32 str;
  };

  m_symbol_name_to_offset_map.clear();
  m_symbol_offset_to_name_map.clear();
  m_symbol_name_to_value_map.clear();

  u32 sym_type = 0;
  // now loop through all the symbols
  for (int i = 0; i < (SYM_INFO_OFFSET + 4) / int(sizeof(SymLower)); i++) {
    auto offset = i * sizeof(SymLower);
    if (offset == empty_pair_offset) {
      continue;
    }
    auto sym = (SymLower*)(mem.data() + offset);
    if (sym->type) {
      // got a symbol!
      if (!sym_type) {
        sym_type = sym->type;
      } else {
        if (sym_type != sym->type) {
          fmt::print("Got bad symbol type. Expected 0x{:x} got 0x{:x}\n", sym_type, sym->type);
          return;
        }
      }

      // now get the info
      auto info = (SymUpper*)(mem.data() + i * sizeof(SymLower) + SYM_INFO_OFFSET + BASIC_OFFSET);

      // now get the string.
      char str_buff[128];
      if (!xdbg::read_goal_memory((u8*)str_buff, 128, info->str + 4, m_debug_context,
                                  m_memory_handle)) {
        fmt::print("Read symbol string failed during read_symbol_table\n");
        return;
      }
      reads++;
      bytes_read += 128;
      // just in case
      str_buff[127] = '\0';
      assert(strlen(str_buff) < 50);
      std::string str(str_buff);

      // GOAL sym - s7
      auto sym_offset = s32(offset + st_base + BASIC_OFFSET) - s32(m_debug_context.s7);
      assert(sym_offset >= INT16_MIN);
      assert(sym_offset <= INT16_MAX);

      // update maps
      if (m_symbol_name_to_offset_map.find(str) != m_symbol_name_to_offset_map.end()) {
        if (str == "asize-of-basic-func") {
          // this is an actual bug in kscheme. The bug has no effect, but we replicate it so that
          // the symbol table layout is closer.

          // to hide this duplicate symbol, we append "-hack-copy" to the end of it.
          str += "-hack-copy";
        } else {
          fmt::print("Symbol {} appears multiple times!\n", str);
          assert(false);
        }
      }

      m_symbol_name_to_offset_map[str] = sym_offset;
      m_symbol_offset_to_name_map[sym_offset] = str;
      m_symbol_name_to_value_map[str] = sym->value;
    }
  }

  assert(m_symbol_offset_to_name_map.size() == m_symbol_name_to_offset_map.size());
  fmt::print("Read symbol table ({} bytes, {} reads, {} symbols, {:.2f} ms)\n", bytes_read, reads,
             m_symbol_name_to_offset_map.size(), timer.getMs());
}

/*!
 * Get the address of a symbol by name. Returns a GOAL address.
 * Returns 0 if the symbol doesn't exist.
 */
u32 Debugger::get_symbol_address(const std::string& sym_name) {
  assert(is_valid());
  auto kv = m_symbol_name_to_offset_map.find(sym_name);
  if (kv != m_symbol_name_to_offset_map.end()) {
    return m_debug_context.s7 + kv->second;
  }
  return 0;
}

/*!
 * Get the value of a symbol by name. Returns if the symbol exists and populates output if it does.
 */
bool Debugger::get_symbol_value(const std::string& sym_name, u32* output) {
  assert(is_valid());
  auto kv = m_symbol_name_to_value_map.find(sym_name);
  if (kv != m_symbol_name_to_value_map.end()) {
    *output = kv->second;
    return true;
  }
  return false;
}

/*!
 * Starts the debugger watch thread which watches the target process to see if it stops.
 */
void Debugger::start_watcher() {
  assert(!m_watcher_running);
  m_watcher_running = true;
  m_watcher_should_stop = false;
  m_watcher_thread = std::thread(&Debugger::watcher, this);
}

/*!
 * Stops the debugger watch thread (waits for it to end)
 */
void Debugger::stop_watcher() {
  assert(m_watcher_running);
  m_watcher_running = false;
  m_watcher_should_stop = true;
  m_watcher_thread.join();
}

Debugger::~Debugger() {
  if (m_watcher_running) {
    stop_watcher();
  }
}

/*!
 * The watcher thread.
 */
void Debugger::watcher() {
  xdbg::SignalInfo signal_info;
  while (!m_watcher_should_stop) {
    // we just sit in a loop, waiting for stops.
    if (xdbg::check_stopped(m_debug_context.tid, &signal_info)) {
      // the target stopped!
      m_continue_info.valid = false;

      switch (signal_info.kind) {
        case xdbg::SignalInfo::SEGFAULT:
          printf("Target has crashed with a SEGFAULT! Run (:di) to get more information.\n");
          break;
        case xdbg::SignalInfo::BREAK:
          printf("Target has stopped. Run (:di) to get more information.\n");
          break;
        default:
          printf("[Debugger] unhandled signal in watcher: %d\n", int(signal_info.kind));
          assert(false);
      }

      {
        std::lock_guard<std::mutex> lock(m_watcher_mutex);
        m_running = false;
        m_watcher_queue.push({signal_info.kind});  // todo, more info?
      }
      m_watcher_cv.notify_one();

    } else {
      // the target didn't stop.
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
  }
}

Debugger::SignalInfo Debugger::pop_signal() {
  {
    std::unique_lock<std::mutex> lock(m_watcher_mutex);
    m_watcher_cv.wait(lock, [&] { return !m_watcher_queue.empty(); });
  }

  Debugger::SignalInfo result;
  if (!try_pop_signal(&result)) {
    assert(false);
  }
  return result;
}

bool Debugger::try_pop_signal(SignalInfo* out) {
  {
    std::unique_lock<std::mutex> lock(m_watcher_mutex);
    if (!m_watcher_queue.empty()) {
      *out = m_watcher_queue.front();
      m_watcher_queue.pop();
      return true;
    }
  }

  return false;
}

int Debugger::get_signal_count() {
  std::unique_lock<std::mutex> lock(m_watcher_mutex);
  return int(m_watcher_queue.size());
}

void Debugger::clear_signal_queue() {
  std::unique_lock<std::mutex> lock(m_watcher_mutex);
  while (!m_watcher_queue.empty()) {
    m_watcher_queue.pop();
  }
}

void Debugger::add_addr_breakpoint(u32 addr) {
  {
    std::unique_lock<std::mutex> lock(m_watcher_mutex);
    auto kv = m_addr_breakpoints.find(addr);
    if (kv != m_addr_breakpoints.end()) {
      fmt::print("Breakpoint at address 0x{:08x} already exists as breakpoint {}\n", addr,
                 kv->second.id);
      return;
    }

    Breakpoint bp;
    bp.goal_addr = addr;
    bp.id = m_addr_breakpoints.size();
    if (!read_memory(&bp.old_data, 1, addr)) {
      fmt::print("Failed to read memory for breakpoint, not adding breakpoint\n");
      return;
    }

    u8 int3 = 0xcc;
    if (!write_memory(&int3, 1, addr)) {
      fmt::print("Failed to write memory for breakpoint, not adding breakpoint\n");
      return;
    }

    m_addr_breakpoints[addr] = bp;
  }
}

void Debugger::remove_addr_breakpoint(u32 addr) {
  {
    std::unique_lock<std::mutex> lock(m_watcher_mutex);
    update_continue_info();
    auto kv = m_addr_breakpoints.find(addr);
    if (kv == m_addr_breakpoints.end()) {
      fmt::print("Breakpoint at address 0x{:08x} does not exist\n", addr);
      return;
    }

    if (!write_memory(&kv->second.old_data, 1, addr)) {
      fmt::print("Failed to remove breakpoint\n");
      return;
    }

    m_addr_breakpoints.erase(kv);
  }
}

void Debugger::update_continue_info() {
  if (m_continue_info.valid || !is_halted()) {
    return;
  }

  if (!m_regs_valid) {
    update_break_info();
  }

  auto kv = m_addr_breakpoints.find(get_regs().rip - m_debug_context.base - 1);
  if (kv == m_addr_breakpoints.end()) {
    m_continue_info.subtract_1 = false;
    m_continue_info.is_addr_breakpiont = false;
  } else {
    if (m_expecting_immeidate_break) {
      printf("Warning, conflicting break and breakpoints. Not sure why we stopped!\n");
    }

    m_continue_info.subtract_1 = true;
    m_continue_info.is_addr_breakpiont = true;
    m_continue_info.addr_breakpoint = kv->second;
  }

  m_expecting_immeidate_break = false;
  m_continue_info.valid = true;
}

DebugInfo& Debugger::get_debug_info_for_object(const std::string& object_name) {
  auto kv = m_debug_info.find(object_name);
  if (kv != m_debug_info.end()) {
    return kv->second;
  }

  return m_debug_info.insert(std::make_pair(object_name, DebugInfo(object_name))).first->second;
}