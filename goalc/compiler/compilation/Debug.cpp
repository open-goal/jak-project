#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/debugger/disassemble.h"

#include "fmt/core.h"

u32 Compiler::parse_address_spec(const goos::Object& form) {
  if (form.is_int()) {
    return form.as_int();
  }

  if (form.is_pair()) {
    auto first = form.as_pair()->car;
    auto rest = form.as_pair()->cdr;
    if (first.is_symbol() && symbol_string(first) == "sym") {
      if (rest.is_pair() && rest.as_pair()->car.is_symbol()) {
        u32 addr = m_debugger.get_symbol_address(symbol_string(rest.as_pair()->car));
        if (!addr) {
          throw_compiler_error(form,
                               "Could not find symbol {} in the target to create an addr-spec",
                               symbol_string(rest.as_pair()->car));
        }
        return addr;
      } else {
        throw_compiler_error(form, "addr-spec sym form must receive exactly one symbol argument.");
        return 0;
      }
    } else if (first.is_symbol() && symbol_string(first) == "sym-val") {
      if (rest.is_pair() && rest.as_pair()->car.is_symbol()) {
        u32 addr = 0;
        if (!m_debugger.get_symbol_value(symbol_string(rest.as_pair()->car), &addr)) {
          throw_compiler_error(form,
                               "Could not find symbol {} in the target to create an addr-spec",
                               symbol_string(rest.as_pair()->car));
        }
        return addr;
      } else {
        throw_compiler_error(form,
                             "addr-spec sym-val form must receive exactly one symbol argument.");
        return 0;
      }
    } else {
      throw_compiler_error(form, "Can't parse this address spec: option {} was not recognized.",
                           first.print());
      return 0;
    }
  } else {
    throw_compiler_error(form, "Invalid address spec.");
    return 0;
  }
}

Val* Compiler::compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;
  if (!m_debugger.is_valid()) {
    lg::print("[Debugger] Could not start debugger because there is no valid debugging context\n");
    return get_none();
  }

  if (m_debugger.is_attached()) {
    lg::print("[Debugger] Could not start debugger because the debugger is already attached.\n");
    return get_none();
  }

  if (m_debugger.attach_and_break()) {
    lg::print("Debugger connected.\n");
  } else {
    lg::print("ERROR\n");
  }

  return get_none();
}

Val* Compiler::compile_dbg_and_continue(const goos::Object& form,
                                        const goos::Object& rest,
                                        Env* env) {
  compile_dbg(form, rest, env);
  compile_cont(form, rest, env);
  return get_none();
}

Val* Compiler::compile_dbs(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  lg::print(" Listener connected? {}\n", m_listener.is_connected());
  lg::print(" Debugger context? {}\n", m_debugger.is_valid());
  if (m_debugger.is_valid()) {
    lg::print(" Attached? {}\n", m_debugger.is_attached());

    if (m_debugger.is_attached()) {
      lg::print(" Halted? {}\n", m_debugger.is_halted());
    }

    lg::print(" Context: {}\n", m_debugger.get_context_string());
  }

  if (m_debugger.is_valid()) {
  } else {
    lg::print("There is no valid debug context from the target.");
  }

  return get_none();
}

Val* Compiler::compile_cont(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  if (m_debugger.is_valid() && m_debugger.is_attached() && m_debugger.is_halted()) {
    m_debugger.do_continue();
  } else {
    lg::print("Couldn't do :cont. Valid {}, attached {}, halted {}\n", m_debugger.is_valid(),
              m_debugger.is_attached(), m_debugger.is_halted());
  }

  return get_none();
}

Val* Compiler::compile_stop(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  if (m_debugger.is_valid() && m_debugger.is_attached() && m_debugger.is_halted()) {
    m_debugger.detach();
  } else {
    lg::print("Couldn't do :stop. Valid {}, attached {}, halted {}\n", m_debugger.is_valid(),
              m_debugger.is_attached(), m_debugger.is_halted());
  }

  return get_none();
}

Val* Compiler::compile_break(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  if (m_debugger.is_valid() && m_debugger.is_attached() && m_debugger.is_running()) {
    m_debugger.do_break();
  } else {
    lg::print("Couldn't do :break. Valid {}, attached {}, running {}\n", m_debugger.is_valid(),
              m_debugger.is_attached(), m_debugger.is_running());
  }

  return get_none();
}

Val* Compiler::compile_dump_all(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  if (!m_debugger.is_halted()) {
    lg::print("Couldn't dump memory. Must be attached and halted.\n");
    return get_none();
  }

  auto args = get_va(form, rest);
  va_check(form, args, {{goos::ObjectType::STRING}}, {});
  auto dest_file = args.unnamed.at(0).as_string()->data;
  auto buffer = new u8[EE_MAIN_MEM_SIZE];
  memset(buffer, 0, EE_MAIN_MEM_SIZE);

  if (!m_debugger.read_memory(buffer + EE_MAIN_MEM_LOW_PROTECT,
                              EE_MAIN_MEM_SIZE - EE_MAIN_MEM_LOW_PROTECT,
                              EE_MAIN_MEM_LOW_PROTECT)) {
    lg::print("Reading memory failed, not dumping.\n");
  } else {
    file_util::write_binary_file(file_util::get_file_path({dest_file}), buffer, EE_MAIN_MEM_SIZE);
  }
  return get_none();
}

namespace {

enum class PrintMode { HEX, UNSIGNED_DEC, SIGNED_DEC, FLOAT };

template <typename T>
void mem_print(T* data, int count, u32 start_addr, PrintMode mode) {
  // always print 16 bytes / line.
  int elt_per_line = 16 / sizeof(T);

  // pad wit the correct number of zeros.
  std::string format_string;

  switch (mode) {
    case PrintMode::HEX:
      format_string = "0x{:0" + std::to_string(2 * sizeof(T)) + "x} ";
      break;
    case PrintMode::UNSIGNED_DEC:
    case PrintMode::SIGNED_DEC:
      format_string = "{:" + std::to_string(3 * sizeof(T)) + "d} ";
      break;
    case PrintMode::FLOAT:
      format_string = "{:8.4f} ";  // todo, is this what we want?
      break;
    default:
      ASSERT(false);
  }

  // loop over elts
  for (int i = 0; i < count; i++) {
    if ((i % elt_per_line) == 0) {
      // first in line, so we should print the GOAL address
      lg::print(" 0x{:08x}: ", start_addr + (i * sizeof(T)));
    }

    // print the thing
    lg::print(format_string, data[i]);

    if ((i % elt_per_line) == (elt_per_line - 1)) {
      // last in line, newline!
      lg::print("\n");
    }
  }
  lg::print("\n");
}
}  // namespace

Val* Compiler::compile_pm(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::INTEGER, {}, goos::ObjectType::INTEGER},
           {{"print-mode", {false, goos::ObjectType::SYMBOL}}});

  int elt_size = args.unnamed.at(0).as_int();
  u32 addr = parse_address_spec(args.unnamed.at(1));
  u32 elts = args.unnamed.at(2).as_int();

  PrintMode mode = PrintMode::HEX;

  if (args.has_named("print-mode")) {
    auto mode_name = symbol_string(args.get_named("print-mode"));
    if (mode_name == "hex") {
      mode = PrintMode::HEX;
    } else if (mode_name == "unsigned-dec") {
      mode = PrintMode::UNSIGNED_DEC;
    } else if (mode_name == "signed-dec") {
      mode = PrintMode::SIGNED_DEC;
    } else if (mode_name == "float") {
      mode = PrintMode::FLOAT;
    } else {
      throw_compiler_error(form, "Unknown print-mode {} for :pm.", mode_name);
    }
  }

  if (!m_debugger.is_halted()) {
    throw_compiler_error(
        form, "Cannot print memory, the debugger must be connected and the target must be halted.");
  }

  auto mem_size = elts * elt_size;
  if (mem_size > 1024 * 1024) {
    throw_compiler_error(form, fmt::format(":pm used on over 1 MB of memory. Not printing."));
  }

  std::vector<u8> mem;
  mem.resize(mem_size);

  if (addr < EE_MAIN_MEM_LOW_PROTECT || (addr + mem_size) > EE_MAIN_MEM_SIZE) {
    throw_compiler_error(form,
                         ":pm memory out of range. Wanted to print 0x{:x} to 0x{:x}, but valid "
                         "memory is 0x{:x} to 0x{:x}.",
                         addr, addr + mem_size, EE_MAIN_MEM_LOW_PROTECT, EE_MAIN_MEM_SIZE);
  }

  m_debugger.read_memory(mem.data(), mem_size, addr);

  switch (mode) {
    case PrintMode::HEX:
    case PrintMode::UNSIGNED_DEC:
      switch (elt_size) {
        case 1:
          mem_print((u8*)mem.data(), elts, addr, mode);
          break;
        case 2:
          mem_print((u16*)mem.data(), elts, addr, mode);
          break;
        case 4:
          mem_print((u32*)mem.data(), elts, addr, mode);
          break;
        case 8:
          mem_print((u64*)mem.data(), elts, addr, mode);
          break;
        default:
          throw_compiler_error(form, ":pm {} is an invalid element size for unsigned", elt_size);
      }
      break;
    case PrintMode::SIGNED_DEC:
      switch (elt_size) {
        case 1:
          mem_print((s8*)mem.data(), elts, addr, mode);
          break;
        case 2:
          mem_print((s16*)mem.data(), elts, addr, mode);
          break;
        case 4:
          mem_print((s32*)mem.data(), elts, addr, mode);
          break;
        case 8:
          mem_print((s64*)mem.data(), elts, addr, mode);
          break;
        default:
          throw_compiler_error(form, ":pm {} is a bad element size for signed", elt_size);
      }
      break;
    case PrintMode::FLOAT:
      switch (elt_size) {
        case 4:
          mem_print((float*)mem.data(), elts, addr, mode);
          break;
        default:
          throw_compiler_error(form, ":pm float can only be printed with size 4, but got size {}",
                               elt_size);
      }
      break;
    default:
      ASSERT(false);
  }

  return get_none();
}

Val* Compiler::compile_di(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)rest;
  (void)env;
  if (!m_debugger.is_halted()) {
    throw_compiler_error(
        form,
        "Cannot get debug info, the debugger must be connected and the target must be halted.");
  }
  auto args = get_va(form, rest);

  std::optional<std::string> dump_path;
  if (args.unnamed.size() > 0 && args.unnamed.at(0).is_string()) {
    dump_path = args.unnamed.at(0).as_string()->data;
  }

  m_debugger.update_break_info(dump_path);
  return get_none();
}

Val* Compiler::compile_disasm(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {{}, goos::ObjectType::INTEGER}, {});
  u32 addr = parse_address_spec(args.unnamed.at(0));
  u32 size = args.unnamed.at(1).as_int();

  if (!m_debugger.is_halted()) {
    throw_compiler_error(
        form,
        "Cannot disassemble memory, the debugger must be connected and the target must be halted.");
  }

  if (size > 1024 * 1024) {
    throw_compiler_error(
        form,
        fmt::format(
            ":disasm used on over 1 MB of memory, this probably isn't what you meant to do."));
  }

  if (addr < EE_MAIN_MEM_LOW_PROTECT || (addr + size) > EE_MAIN_MEM_SIZE) {
    throw_compiler_error(form,
                         ":disasm memory out of range. Wanted to print 0x{:x} to 0x{:x}, but valid "
                         "memory is 0x{:x} to 0x{:x}.",
                         addr, addr + size, EE_MAIN_MEM_LOW_PROTECT, EE_MAIN_MEM_SIZE);
  }

  lg::print("{}\n", m_debugger.get_info_about_addr(addr));
  lg::print("{}\n", m_debugger.disassemble_x86_with_symbols(size, addr));

  return get_none();
}

Val* Compiler::compile_bp(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});

  if (!m_debugger.is_halted()) {
    throw_compiler_error(
        form,
        "Cannot add breakpoint, the debugger must be connected and the target must be halted.");
  }

  u32 addr = parse_address_spec(args.unnamed.at(0));
  m_debugger.add_addr_breakpoint(addr);

  return get_none();
}

Val* Compiler::compile_ubp(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});

  if (!m_debugger.is_halted()) {
    throw_compiler_error(
        form,
        "Cannot remove breakpoint, the debugger must be connected and the target must be halted.");
  }

  u32 addr = parse_address_spec(args.unnamed.at(0));
  m_debugger.remove_addr_breakpoint(addr);

  return get_none();
}

Val* Compiler::compile_d_sym_name(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {goos::ObjectType::INTEGER}, {});
  s32 ofs = args.unnamed.at(0).as_int();

  if (!m_debugger.is_halted()) {
    throw_compiler_error(form,
                         "Cannot get symbol name from offset, the debugger must be connected and "
                         "the target must be halted.");
  }

  auto sym_name = m_debugger.get_symbol_name_from_offset(ofs);
  if (sym_name) {
    lg::print("symbol name for symbol {:X}h is {}\n", ofs, sym_name);
  } else {
    lg::print("symbol {:X}h is not loaded or is invalid\n", ofs);
  }

  return get_none();
}
