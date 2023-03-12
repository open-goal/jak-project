/*!
 * @file Debugger.h
 * The OpenGOAL debugger.
 * Uses xdbg functions to debug an OpenGOAL target.
 */

#include "Debugger.h"

#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"

#include "goalc/debugger/disassemble.h"
#include "goalc/emitter/Register.h"
#include "goalc/listener/Listener.h"

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
 * Will silently do nothing if we aren't attached, so it is safe to just call detach() to try to
 * clean up when exiting.
 */
bool Debugger::detach() {
  bool succ = true;
  if (is_valid() && m_attached) {
#ifdef __linux__
    if (!is_halted()) {
      succ = do_break();
    }
    stop_watcher();
    xdbg::close_memory(m_debug_context.tid, &m_memory_handle);
    xdbg::detach_and_resume(m_debug_context.tid);
#elif _WIN32
    if (is_halted()) {
      succ = do_continue();
    }
    {
      std::unique_lock<std::mutex> lk(m_watcher_mutex);
      m_attach_return = false;
    }
    stop_watcher();
    {
      std::unique_lock<std::mutex> lk(m_watcher_mutex);
      m_attach_cv.wait(lk, [&]() { return m_attach_return; });
    }
    xdbg::close_memory(m_debug_context.tid, &m_memory_handle);
#endif
    // m_context_valid = false;
    m_attached = false;
  } else {
    succ = false;
  }
  // todo, should we print something if we can't detach?
  return succ;
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

    // attach and send a break command
    if (try_start_watcher()) {
      // wait for the signal queue to get a stop and pop it.
      auto info = pop_signal();

      // manually set up continue for this.
      m_continue_info.valid = true;
      m_continue_info.subtract_1 = false;

      // this may fail if you crash at exactly the wrong time. todo - remove?
      if (info.kind != xdbg::SignalInfo::BREAK) {
        lg::print("[Debugger] got signal {} when expecting break.\n", (int)info.kind);
      }

      // open the memory of the process
      if (!xdbg::open_memory(m_debug_context.tid, &m_memory_handle)) {
        return false;
      }

      m_attached = true;
      m_running = false;

      // get info from target
      update_break_info({});

      auto signal_count = get_signal_count();
      if (signal_count != 0) {
        lg::print("[Debugger] got signal count of {} in attach_and_break\n", signal_count);
      }
      return true;
    }
  } else {
    lg::print("[Debugger] attach_and_break can't be done when valid = {} and attached = {}\n",
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
 * This assumes we have an up-to-date memory map and symbol info.
 */
InstructionPointerInfo Debugger::get_rip_info(u64 rip) {
  InstructionPointerInfo result;
  result.real_rip = rip;

  if (m_context_valid) {
    result.goal_rip = rip - m_debug_context.base;
    if (rip >= m_debug_context.base + EE_MAIN_MEM_LOW_PROTECT &&
        rip < m_debug_context.base + EE_MAIN_MEM_SIZE) {
      result.in_goal_mem = true;
      auto map_loc = m_memory_map.lookup(rip - m_debug_context.base);
      if (map_loc.empty) {
        result.knows_object = false;
        result.knows_function = false;
      } else {
        u64 obj_offset = rip - m_debug_context.base - map_loc.start_addr;
        result.map_entry = map_loc;
        result.knows_object = true;
        result.object_name = map_loc.obj_name;
        result.object_seg = map_loc.seg_id;
        result.object_offset = obj_offset;

        FunctionDebugInfo* info = nullptr;
        std::string name;

        if (get_debug_info_for_object(map_loc.obj_name)
                .lookup_function(&info, &name, obj_offset, map_loc.seg_id)) {
          result.knows_function = true;
          result.function_name = name;
          result.function_offset = obj_offset - info->offset_in_seg;
          result.func_debug = info;

          ASSERT(!info->instructions.empty());
        }
      }
    }
  }

  return result;
}

void print_and_append_to_string(std::string& str, const std::string& log) {
  str += log;
  lg::print(log);
}

std::vector<BacktraceFrame> Debugger::get_backtrace(u64 rip,
                                                    u64 rsp,
                                                    std::optional<std::string> dump_path) {
  // TODO - it would probably be nice to decouple printing the backtrace from getting the backtrace
  // for now, build up a string and dump it at the end (if a path is provided)
  std::string backtrace_contents = "";
  lg::print("Backtrace:\n");
  std::vector<BacktraceFrame> bt;

  if (rip == m_debug_context.base) {
    // we jumped to NULL.
    print_and_append_to_string(backtrace_contents,
                               "Jumped to GOAL 0x0. Attempting to find previous function.\n");
    u64 next_rip = 0;
    if (!read_memory_if_safe<u64>(&next_rip, rsp - m_debug_context.base)) {
      print_and_append_to_string(backtrace_contents,
                                 "  failed to read return address off of the stack\n");
      return {};
    }

    rip = next_rip;
    rsp += 8;
  }

  int fails = 0;
  while (true) {
    print_and_append_to_string(
        backtrace_contents,
        fmt::format("   rsp: 0x{:x} (#x{:x}) rip: 0x{:x} (#x{:x})\n", rsp,
                    rsp - m_debug_context.base, rip, rip - m_debug_context.base));
    BacktraceFrame frame;
    frame.rip_info = get_rip_info(rip);
    frame.rsp_at_rip = rsp;

    if (frame.rip_info.knows_function && frame.rip_info.func_debug &&
        frame.rip_info.func_debug->stack_usage) {
      fails = 0;
      print_and_append_to_string(backtrace_contents,
                                 "<====================== CALL STACK ======================>\n");
      print_and_append_to_string(backtrace_contents,
                                 fmt::format("{} from {}\n", frame.rip_info.function_name,
                                             frame.rip_info.func_debug->obj_name));
      // we're good!
      auto disasm = disassemble_at_rip(frame.rip_info);
      print_and_append_to_string(backtrace_contents, fmt::format("{}\n", disasm.text));
      u64 rsp_at_call = rsp + *frame.rip_info.func_debug->stack_usage;

      u64 next_rip = 0;
      if (!read_memory_if_safe<u64>(&next_rip, rsp_at_call - m_debug_context.base)) {
        print_and_append_to_string(backtrace_contents, "Invalid return address encountered!\n");
        break;
      }

      rip = next_rip;
      rsp = rsp_at_call + 8;  // 8 for the call itself.

    } else {
      if (!frame.rip_info.knows_function) {
        if (fails == 0) {
          print_and_append_to_string(backtrace_contents, "Unknown Function at rip\n");
        }

        /*
        bool found = false;
        if (s32(rip - m_debug_context.base) > 0 &&
            m_symbol_name_to_value_map.find("function") != m_symbol_name_to_value_map.cend()) {
          lg::print("Attempting to find function at this address.\n");
          u32 function_sym_val = m_symbol_name_to_value_map.at("function");
          u32 goal_pc = u32(rip - m_debug_context.base) & -8;

          // go back through memory, but stop before reading the symbol table
          u32 symtable_end = m_symbol_name_to_value_map.at("#f") + 0xff38;
          while (goal_pc > symtable_end) {
            goal_pc -= 8;
            u32 wordval;
            if (!read_memory_if_safe<u32>(&wordval, goal_pc)) {
              goal_pc = symtable_end;
              break;
            }

            if (wordval == function_sym_val) {
              // found a function!
              lg::print("Found function after {} bytes!\n",
                         (rip - m_debug_context.base) - goal_pc);
              break;
            }
          }

          if (goal_pc <= symtable_end) {
            lg::print("Could not find function within this address.\n");
          } else {
            rip = goal_pc + m_debug_context.base + BASIC_OFFSET;
            found = true;
          }
        } else*/
        if (fails > 70) {
          print_and_append_to_string(
              backtrace_contents,
              "Backtrace was too long. Exception might have happened outside GOAL code, or the "
              "stack frame is too long.\n");
          break;
        }
        // attempt to backtrace anyway! if this fails then rip
        u64 next_rip = 0;
        if (!read_memory_if_safe<u64>(&next_rip, rsp - m_debug_context.base - 8)) {
          print_and_append_to_string(backtrace_contents, "Invalid return address encountered!\n");
          break;
        }

        rip = next_rip;
        rsp = rsp + 8;  // 8 for the call itself.
        ++fails;
        // break;
      } else if (!frame.rip_info.func_debug) {
        print_and_append_to_string(
            backtrace_contents,
            fmt::format("Function {} has no debug info.\n", frame.rip_info.function_name));
        break;
      } else {
        print_and_append_to_string(
            backtrace_contents,
            fmt::format("Function {} with no stack frame data.\n", frame.rip_info.function_name));
        break;
      }
    }

    bt.push_back(frame);
  }

  if (dump_path) {
    file_util::write_text_file(dump_path.value(), backtrace_contents);
  }

  return bt;
}

/*!
 * This assumes we have an up-to-date memory map and symbol info.
 */
Disassembly Debugger::disassemble_at_rip(const InstructionPointerInfo& info) {
  // todo adjust rip if break instruction????
  Disassembly result;

  result.failed = false;
  u64 rip = info.real_rip;

  if (info.in_goal_mem) {
    // we only want to disassemble GOAL code.
    // if the crash happens outside of GOAL code, use a normal debugger.

    if (!info.knows_function || !info.knows_object || !info.map_entry) {
      // something went wrong and we can't find this code.
      // however, we can still do better than nothing by dumping the memory and disassembling.
      std::vector<u8> mem;
      mem.resize(INSTR_DUMP_SIZE_REV + INSTR_DUMP_SIZE_FWD);
      read_memory(mem.data(), INSTR_DUMP_SIZE_REV + INSTR_DUMP_SIZE_FWD,
                  info.real_rip - m_debug_context.base - INSTR_DUMP_SIZE_REV);
      result.failed = true;
      if (info.knows_object) {
        result.text += fmt::format("In segment {} of obj {}, offset 0x{:x}\n", info.object_seg,
                                   info.object_name, info.object_offset);
        result.text += disassemble_x86(mem.data(), mem.size(), rip - INSTR_DUMP_SIZE_REV, rip);
      } else {
        result.text += "In unknown code\n";
        result.text += disassemble_x86(mem.data(), mem.size(), rip - INSTR_DUMP_SIZE_REV, rip);
      }
    } else {
      // we have enough info to do a fancy disassembly!
      u64 obj_offset = rip - m_debug_context.base - info.map_entry->start_addr;

      FunctionDebugInfo* func_info = info.func_debug;
      std::string name = func_info->name;
      auto continue_info = get_continue_info(rip);
      ASSERT(!func_info->instructions.empty());

      std::vector<u8> function_mem;
      function_mem.resize(func_info->instructions.back().offset +
                          func_info->instructions.back().instruction.length());
      read_memory(function_mem.data(), function_mem.size(),
                  info.map_entry->start_addr + func_info->offset_in_seg);

      int rip_offset = 0;
      if (continue_info.valid && continue_info.is_addr_breakpiont) {
        int offset_in_fmem = uint64_t(continue_info.addr_breakpoint.goal_addr) -
                             uint64_t(info.map_entry->start_addr + func_info->offset_in_seg);
        if (offset_in_fmem < 0 || offset_in_fmem >= int(function_mem.size())) {
          result.failed = true;
        } else {
          function_mem.at(offset_in_fmem) = continue_info.addr_breakpoint.old_data;
          rip_offset = -1;
        }
      }

      result.text += fmt::format(
          "In function {} in segment {} of obj {}, offset_obj 0x{:x}, offset_func 0x{:x}\n", name,
          info.map_entry->seg_id, info.map_entry->obj_name, obj_offset, info.function_offset);

      result.text += disassemble_x86_function(
          function_mem.data(), function_mem.size(), m_reader,
          m_debug_context.base + info.map_entry->start_addr + func_info->offset_in_seg,
          rip + rip_offset, func_info->instructions, func_info->code_sources, func_info->ir_strings,
          &result.failed, false);
    }
  } else {
    result.failed = true;
    result.text = "Not in GOAL code!\n";
  }
  return result;
}

/*!
 * Read the registers, symbol table, and instructions near rip.
 * Print out some info about where we are.
 */
void Debugger::update_break_info(std::optional<std::string> dump_path) {
  // todo adjust rip if break instruction????

  m_memory_map = m_listener->build_memory_map();
  // lg::print("{}", m_memory_map.print());
  read_symbol_table();
  m_regs_valid = false;
  if (!xdbg::get_regs_now(m_debug_context.tid, &m_regs_at_break)) {
    lg::print("[Debugger] get_regs_now failed after break, something is wrong\n");
  } else {
    m_regs_valid = true;
    lg::print("{}", m_regs_at_break.print_gprs());
  }

  if (regs_valid()) {
    m_break_info = get_rip_info(m_regs_at_break.rip);
    update_continue_info();
    auto dis = disassemble_at_rip(m_break_info);
    lg::print("{}\n", dis.text);

    get_backtrace(m_regs_at_break.rip, m_regs_at_break.gprs[emitter::RSP], dump_path);
  }
}

/*!
 * Stop the target. Must be attached and not stopped.
 * Waits for break to be acknowledged and reads break info.
 */
bool Debugger::do_break() {
  ASSERT(is_valid() && is_attached() && is_running());
  m_expecting_immeidate_break = true;
  m_continue_info.valid = false;
  clear_signal_queue();
  if (!xdbg::break_now(m_debug_context.tid)) {
    return false;
  } else {
    auto info = pop_signal();
    ASSERT(info.kind == xdbg::SignalInfo::BREAK);
    update_break_info({});
    m_running = false;
    return true;
  }
}

/*!
 * Continue the target, must be attached and stopped.
 */
bool Debugger::do_continue() {
  ASSERT(is_valid() && is_attached() && is_halted());
  if (!m_regs_valid) {
    update_break_info({});
  }
  ASSERT(regs_valid());

  if (!m_continue_info.valid) {
    update_continue_info();
  }
  ASSERT(m_continue_info.valid);
  m_regs_valid = false;

  if (m_continue_info.subtract_1) {
    m_regs_at_break.rip--;
    auto result = xdbg::set_regs_now(m_debug_context.tid, m_regs_at_break);
    ASSERT(result);
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
bool Debugger::read_memory(u8* dest_buffer, int size, u32 goal_addr) const {
  ASSERT(is_valid() && is_attached() && is_halted());
  return xdbg::read_goal_memory(dest_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}

bool Debugger::read_memory_if_safe(u8* dest_buffer, int size, u32 goal_addr) const {
  ASSERT(is_valid() && is_attached() && is_halted());
  if (goal_addr >= EE_MAIN_MEM_LOW_PROTECT && goal_addr + size < EE_MAIN_MEM_SIZE) {
    return read_memory(dest_buffer, size, goal_addr);
  }
  return false;
}

/*!
 * Write the memory of an attached and halted target.
 */
bool Debugger::write_memory(const u8* src_buffer, int size, u32 goal_addr) {
  ASSERT(is_valid() && is_attached() && is_halted());
  return xdbg::write_goal_memory(src_buffer, size, goal_addr, m_debug_context, m_memory_handle);
}

void Debugger::read_symbol_table_jak1() {
  using namespace jak1_symbols;
  using namespace jak1;
  ASSERT(is_valid() && is_attached() && is_halted());
  u32 bytes_read = 0;
  u32 reads = 0;
  Timer timer;

  u32 st_base = m_debug_context.s7 - ((GOAL_MAX_SYMBOLS / 2) * 8 + BASIC_OFFSET);
  u32 empty_pair_offset = (m_debug_context.s7 + FIX_SYM_EMPTY_PAIR - PAIR_OFFSET) - st_base;

  std::vector<u8> mem;
  mem.resize(SYM_TABLE_MEM_SIZE);

  if (!xdbg::read_goal_memory(mem.data(), SYM_TABLE_MEM_SIZE, st_base, m_debug_context,
                              m_memory_handle)) {
    lg::print("Read failed during read_symbol_table\n");
    return;
  }
  reads++;
  bytes_read += SYM_TABLE_MEM_SIZE;

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
          lg::print("Got bad symbol type. Expected 0x{:x} got 0x{:x}: addr 0x{:x}\n", sym_type,
                    sym->type, offset + st_base + (uint64_t)m_debug_context.base);
          return;
        }
      }

      // now get the info
      auto info = (SymUpper*)(mem.data() + i * sizeof(SymLower) + SYM_INFO_OFFSET + BASIC_OFFSET);

      // now get the string.
      char str_buff[128];
      if (!xdbg::read_goal_memory((u8*)str_buff, 128, info->str + 4, m_debug_context,
                                  m_memory_handle)) {
        lg::print("Read symbol string failed during read_symbol_table\n");
        return;
      }
      reads++;
      bytes_read += 128;
      // just in case
      str_buff[127] = '\0';

      // GOAL sym - s7
      auto sym_offset = s32(offset + st_base + BASIC_OFFSET) - s32(m_debug_context.s7);
      ASSERT(sym_offset >= -SYM_TABLE_MEM_SIZE / 4);
      ASSERT(sym_offset < SYM_TABLE_MEM_SIZE / 4);

      std::string str(str_buff);
      if (str.length() >= 50) {
        lg::print("Invalid symbol #x{:x}!\n", sym_offset);
        continue;
      }

      // update maps
      if (m_symbol_name_to_offset_map.find(str) != m_symbol_name_to_offset_map.end()) {
        if (str == "asize-of-basic-func") {
          // this is an actual bug in kscheme. The bug has no effect, but we replicate it so that
          // the symbol table layout is closer.

          // to hide this duplicate symbol, we append "-hack-copy" to the end of it.
          str += "-hack-copy";
        } else {
          lg::print("Symbol {} (#x{:x}) appears multiple times!\n", str, sym_offset);
          continue;
          // ASSERT(false);
        }
      }

      m_symbol_name_to_offset_map[str] = sym_offset;
      m_symbol_offset_to_name_map[sym_offset] = str;
      m_symbol_name_to_value_map[str] = sym->value;
    }
  }

  ASSERT(m_symbol_offset_to_name_map.size() == m_symbol_name_to_offset_map.size());
  lg::print("Read symbol table ({} bytes, {} reads, {} symbols, {:.2f} ms)\n", bytes_read, reads,
            m_symbol_name_to_offset_map.size(), timer.getMs());
}

void Debugger::read_symbol_table_jak2() {
  using namespace jak2_symbols;
  using namespace jak2;
  ASSERT(is_valid() && is_attached() && is_halted());
  u32 bytes_read = 0;
  u32 reads = 0;
  Timer timer;

  u32 st_base = m_debug_context.s7 - ((GOAL_MAX_SYMBOLS / 2) * 4 + 1);
  u32 empty_pair_offset =
      (m_debug_context.s7 + S7_OFF_FIX_SYM_EMPTY_PAIR /*- PAIR_OFFSET*/) - st_base;

  std::vector<u8> mem;
  mem.resize(SYM_TABLE_MEM_SIZE);

  if (!xdbg::read_goal_memory(mem.data(), SYM_TABLE_MEM_SIZE, st_base, m_debug_context,
                              m_memory_handle)) {
    lg::print("Read failed during read_symbol_table\n");
    return;
  }
  reads++;
  bytes_read += SYM_TABLE_MEM_SIZE;

  m_symbol_name_to_offset_map.clear();
  m_symbol_offset_to_name_map.clear();
  m_symbol_name_to_value_map.clear();

  // now loop through all the symbols
  for (int i = 0; i < (SYM_TO_STRING_OFFSET + 4) / 4; i++) {
    u32 offset = i * 4;
    if (offset == empty_pair_offset) {
      continue;
    }
    auto sym_val = *(u32*)(mem.data() + offset);
    auto info = *(u32*)(mem.data() + i * 4 + SYM_TO_STRING_OFFSET + 1);
    if (info) {
      // now get the string.
      char str_buff[128];
      if (!xdbg::read_goal_memory((u8*)str_buff, 128, info + 4, m_debug_context, m_memory_handle)) {
        lg::print("Read symbol string failed during read_symbol_table\n");
        return;
      }
      reads++;
      bytes_read += 128;
      // just in case
      str_buff[127] = '\0';

      // GOAL sym - s7
      auto sym_offset = s32(offset + st_base) - s32(m_debug_context.s7);
      ASSERT(sym_offset >= -SYM_TABLE_MEM_SIZE / 4);
      ASSERT(sym_offset < SYM_TABLE_MEM_SIZE / 4);

      std::string str(str_buff);
      if (str.length() >= 50) {
        lg::print("Invalid symbol #x{:x}!\n", sym_offset);
        continue;
      }

      // update maps
      if (m_symbol_name_to_offset_map.find(str) != m_symbol_name_to_offset_map.end()) {
        if (str == "asize-of-basic-func") {
          // this is an actual bug in kscheme. The bug has no effect, but we replicate it so that
          // the symbol table layout is closer.

          // to hide this duplicate symbol, we append "-hack-copy" to the end of it.
          str += "-hack-copy";
        } else {
          lg::print("Symbol {} (#x{:x}) appears multiple times!\n", str, sym_offset);
          continue;
          // ASSERT(false);
        }
      }

      m_symbol_name_to_offset_map[str] = sym_offset;
      m_symbol_offset_to_name_map[sym_offset] = str;
      m_symbol_name_to_value_map[str] = sym_val;
    }
  }

  ASSERT(m_symbol_offset_to_name_map.size() == m_symbol_name_to_offset_map.size());
  lg::print("Read symbol table ({} bytes, {} reads, {} symbols, {:.2f} ms)\n", bytes_read, reads,
            m_symbol_name_to_offset_map.size(), timer.getMs());
}

/*!
 * Read the GOAL Symbol table from an attached and halted target.
 */
void Debugger::read_symbol_table() {
  switch (m_version) {
    case GameVersion::Jak1:
      read_symbol_table_jak1();
      break;
    case GameVersion::Jak2:
      read_symbol_table_jak2();
      break;
    default:
      ASSERT(false);
  }
}

/*!
 * Get the address of a symbol by name. Returns a GOAL address.
 * Returns 0 if the symbol doesn't exist.
 */
u32 Debugger::get_symbol_address(const std::string& sym_name) {
  ASSERT(is_valid());
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
  ASSERT(is_valid());
  auto kv = m_symbol_name_to_value_map.find(sym_name);
  if (kv != m_symbol_name_to_value_map.end()) {
    *output = kv->second;
    return true;
  }
  return false;
}

/*!
 * Get the value of a symbol by name. Returns NULL if symbol does not exist.
 */
const char* Debugger::get_symbol_name_from_offset(s32 ofs) const {
  ASSERT(is_valid());
  auto kv = m_symbol_offset_to_name_map.find(ofs);
  if (kv != m_symbol_offset_to_name_map.end()) {
    return kv->second.c_str();
  }
  return NULL;
}

/*!
 * Attempt to start the debugger watch thread and evaluate attach success. Stops if unsuccessful.
 */
bool Debugger::try_start_watcher() {
#ifdef __linux
  m_attach_response = xdbg::attach_and_break(m_debug_context.tid);
  if (!m_attach_response)
    return false;
  start_watcher();
  return true;
#elif defined(_WIN32)
  start_watcher();
  std::unique_lock<std::mutex> lk(m_watcher_mutex);
  m_attach_cv.wait(lk, [&]() { return m_attach_return; });
  if (!m_attach_response) {
    stop_watcher();
  }
  return m_attach_response;
#else
  return false;
#endif
}

/*!
 * Starts the debugger watch thread which watches the target process to see if it stops.
 */
void Debugger::start_watcher() {
  if (m_watcher_running) {
    stop_watcher();
  }
  ASSERT(!m_watcher_running);
  m_watcher_running = true;
  m_watcher_should_stop = false;
  {
    std::unique_lock<std::mutex> lk(m_watcher_mutex);
    m_attach_return = false;
  }
  m_watcher_thread = std::thread(&Debugger::watcher, this);
}

/*!
 * Stops the debugger watch thread (waits for it to end)
 */
void Debugger::stop_watcher() {
  ASSERT(m_watcher_running);
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
// watcher will now attach to target.
// linux doesn't require the attachment and watching to be on the same thread, but windows does.
#ifdef _WIN32
  m_attach_response = xdbg::attach_and_break(m_debug_context.tid);
  m_attach_return = true;
  m_attach_cv.notify_all();
  if (!m_attach_response)
    return;
#endif

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
        case xdbg::SignalInfo::MATH_EXCEPTION:
          printf("Target has crashed with a MATH_EXCEPTION! Run (:di) to get more information.\n");
          break;
        case xdbg::SignalInfo::DISAPPEARED:
          printf("Target has disappeared. Maybe it quit or was killed.\n");
          handle_disappearance();
          break;
        case xdbg::SignalInfo::ILLEGAL_INSTR:
          printf(
              "Target has crashed due to an illegal instruction. Run (:di) to get more "
              "information.\n");
          break;
        case xdbg::SignalInfo::UNKNOWN:
          printf("Target has encountered an unknown signal. Run (:di) to get more information.\n");
          break;
#ifdef _WIN32
        case xdbg::SignalInfo::EXCEPTION:
          printf("Target raised an exception (%s). Run (:di) to get more information.\n",
                 signal_info.msg.c_str());
          break;
        case xdbg::SignalInfo::NOTHING:
          // printf("Nothing happened.\n");
          break;
#endif
        default:
          ASSERT_MSG(false, fmt::format("[Debugger] unhandled signal in watcher: {}",
                                        int(signal_info.kind)));
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

// watcher will now detach from target.
// again, windows needs the debugger thread to remain consistent
#ifdef _WIN32
  m_attach_response = xdbg::detach_and_resume(m_debug_context.tid);
  m_attach_return = true;
#endif
}

void Debugger::handle_disappearance() {
  m_watcher_should_stop = true;
  xdbg::close_memory(m_debug_context.tid, &m_memory_handle);
  xdbg::detach_and_resume(m_debug_context.tid);
  m_context_valid = false;
  m_attached = false;
}

Debugger::SignalInfo Debugger::pop_signal() {
  {
    std::unique_lock<std::mutex> lock(m_watcher_mutex);
    m_watcher_cv.wait(lock, [&] { return !m_watcher_queue.empty(); });
  }

  Debugger::SignalInfo result;
  if (!try_pop_signal(&result)) {
    ASSERT(false);
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
      lg::print("Breakpoint at address 0x{:08x} already exists as breakpoint {}\n", addr,
                kv->second.id);
      return;
    }

    Breakpoint bp;
    bp.goal_addr = addr;
    bp.id = m_addr_breakpoints.size();
    if (!read_memory(&bp.old_data, 1, addr)) {
      lg::print("Failed to read memory for breakpoint, not adding breakpoint\n");
      return;
    }

    u8 int3 = 0xcc;
    if (!write_memory(&int3, 1, addr)) {
      lg::print("Failed to write memory for breakpoint, not adding breakpoint\n");
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
      lg::print("Breakpoint at address 0x{:08x} does not exist\n", addr);
      return;
    }

    if (!write_memory(&kv->second.old_data, 1, addr)) {
      lg::print("Failed to remove breakpoint\n");
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
    update_break_info({});
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

Debugger::ContinueInfo Debugger::get_continue_info(u64 rip) const {
  ContinueInfo result;
  auto kv = m_addr_breakpoints.find(rip - m_debug_context.base - 1);
  if (kv == m_addr_breakpoints.end()) {
    result.subtract_1 = false;
    result.is_addr_breakpiont = false;
  } else {
    result.subtract_1 = true;
    result.is_addr_breakpiont = true;
    result.addr_breakpoint = kv->second;
  }

  result.valid = true;
  return result;
}

DebugInfo& Debugger::get_debug_info_for_object(const std::string& object_name) {
  auto kv = m_debug_info.find(object_name);
  if (kv != m_debug_info.end()) {
    return kv->second;
  }

  return m_debug_info.insert(std::make_pair(object_name, DebugInfo(object_name))).first->second;
}

bool Debugger::knows_object(const std::string& object_name) const {
  return m_debug_info.find(object_name) != m_debug_info.end();
}

/*!
 * Do x86 disassembly at the specified address and then do some basic string replacement for
 * symbols. It will attempt to detect symbol dereferences (e.g. *active-pool*), symbol references
 * (e.g. 'dead), and a special case to detect #f (outputted as '#f for correctness).
 */
std::string Debugger::disassemble_x86_with_symbols(int len, u64 base_addr) const {
  std::vector<u8> mem;
  mem.resize(len);

  read_memory(mem.data(), len, base_addr);

  auto result = disassemble_x86(mem.data(), mem.size(), get_x86_base_addr() + base_addr);

  // find symbol values!
  const std::string sym_val_string("[r15+r14*1");
  size_t pos = 0;
  while ((pos = result.find(sym_val_string, pos)) != std::string::npos) {
    size_t read;
    auto sym_addr = std::stol(result.substr(pos + sym_val_string.length(), 7), &read,
                              16);  // -0x1234 is 7 characters

    auto sym_name = get_symbol_name_from_offset((s32)sym_addr);
    if (sym_name) {
      std::string sym_str(sym_name);
      result.replace(pos + 1, read + sym_val_string.length() - 1,
                     sym_str);  // the [ is ignored (result is something like: [identity])
      pos += sym_str.length() + 1;
      ASSERT(result.at(pos) == ']');  // maybe?
    } else {
      // symbol not found for whatever reason, just use regular disassembly and skip over
      pos += 1;
    }
  }

  // find symbol references!
  const std::string sym_addr_string("[r14");
  pos = 0;
  while ((pos = result.find(sym_addr_string, pos)) != std::string::npos) {
    size_t read;
    auto sym_addr = std::stol(result.substr(pos + sym_addr_string.length(), 7), &read,
                              16);  // -0x1234 is 7 characters

    auto sym_name = get_symbol_name_from_offset((s32)sym_addr);
    if (sym_name) {
      std::string sym_str(sym_name);
      result.replace(pos, read + sym_addr_string.length() + 1, fmt::format("'{}", sym_str));
      pos += sym_str.length();
    } else {
      // symbol not found for whatever reason, just use regular disassembly and skip over
      pos += 1;
    }
  }

  // find #f references!
  const std::string op_mov_string("] mov ");
  const std::string sym_false_string(", r14");
  pos = 0;
  while ((pos = result.find(op_mov_string, pos)) != std::string::npos) {
    pos += op_mov_string.length();
    auto r14_pos = result.find(sym_false_string, pos);
    if (r14_pos < result.find(op_mov_string, pos)) {
      result.replace(r14_pos, sym_false_string.length(), fmt::format(", '#f"));
    }
  }

  return result;
}
