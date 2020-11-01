/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 */

#include <cassert>
#include "Debugger.h"
#include "common/util/Timer.h"
#include "common/goal_constants.h"
#include "common/symbols.h"
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

      read_symbol_table();
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
    read_symbol_table();
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

u32 Debugger::get_symbol_address(const std::string& sym_name) {
  assert(is_valid());
  auto kv = m_symbol_name_to_offset_map.find(sym_name);
  if (kv != m_symbol_name_to_offset_map.end()) {
    return m_debug_context.s7 + kv->second;
  }
  return 0;
}

bool Debugger::get_symbol_value(const std::string& sym_name, u32* output) {
  assert(is_valid());
  auto kv = m_symbol_name_to_value_map.find(sym_name);
  if (kv != m_symbol_name_to_value_map.end()) {
    *output = kv->second;
    return true;
  }
  return false;
}