#include "goalc/compiler/Compiler.h"
#include "goalc/emitter/disassemble.h"
#include "common/util/FileUtil.h"
#include "third-party/fmt/core.h"

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
          throw_compile_error(form, "debugger doesn't know where the symbol is");
        }
        return addr;
      } else {
        throw_compile_error(form, "invalid sym form");
        return 0;
      }
    } else if (first.is_symbol() && symbol_string(first) == "sym-val") {
      if (rest.is_pair() && rest.as_pair()->car.is_symbol()) {
        u32 addr = 0;
        if (!m_debugger.get_symbol_value(symbol_string(rest.as_pair()->car), &addr)) {
          throw_compile_error(form, "debugger doesn't know where the symbol is");
        }
        return addr;
      } else {
        throw_compile_error(form, "invalid sym-val form");
        return 0;
      }
    }

    else {
      throw_compile_error(form, "can't parse this address spec");
      return 0;
    }
  } else {
    throw_compile_error(form, "can't parse this address spec");
    return 0;
  }
}

Val* Compiler::compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;
  if (!m_debugger.is_valid()) {
    fmt::print("[Debugger] Could not start debugger because there is no valid debugging context\n");
    return get_none();
  }

  if (m_debugger.is_attached()) {
    fmt::print("[Debugger] Could not start debugger because the debugger is already attached.\n");
    return get_none();
  }

  if (m_debugger.attach_and_break()) {
    fmt::print("Debugger connected.\n");
  } else {
    fmt::print("ERROR\n");
  }

  return get_none();
}

Val* Compiler::compile_dbs(const goos::Object& form, const goos::Object& rest, Env* env) {
  // todo - do something with args.
  (void)form;
  (void)rest;
  (void)env;

  fmt::print(" Listener connected? {}\n", m_listener.is_connected());
  fmt::print(" Debugger context? {}\n", m_debugger.is_valid());
  if (m_debugger.is_valid()) {
    fmt::print(" Attached? {}\n", m_debugger.is_attached());

    if (m_debugger.is_attached()) {
      fmt::print(" Halted? {}\n", m_debugger.is_halted());
    }

    fmt::print(" Context: {}\n", m_debugger.get_context_string());
  }

  if (m_debugger.is_valid()) {
  } else {
    fmt::print("There is no valid debug context from the target.");
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
    fmt::print("Couldn't do :cont. Valid {}, attached {}, halted {}\n", m_debugger.is_valid(),
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
    fmt::print("Couldn't do :break. Valid {}, attached {}, running {}\n", m_debugger.is_valid(),
               m_debugger.is_attached(), m_debugger.is_running());
  }

  return get_none();
}

Val* Compiler::compile_dump_all(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  if (!m_debugger.is_halted()) {
    fmt::print("Couldn't dump memory. Must be attached and halted.\n");
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
    fmt::print("Reading memory failed, not dumping.\n");
  } else {
    file_util::write_binary_file(file_util::get_file_path({dest_file}), buffer, EE_MAIN_MEM_SIZE);
  }
  return get_none();
}

namespace {

enum PrintMode { HEX, UNSIGNED_DEC, SIGNED_DEC, FLOAT };

template <typename T>
void mem_print(T* data, int count, u32 start_addr, PrintMode mode) {
  // always print 16 bytes / line.
  int elt_per_line = 16 / sizeof(T);

  // pad wit the correct number of zeros.
  std::string format_string;

  switch (mode) {
    case HEX:
      format_string = "0x{:0" + std::to_string(2 * sizeof(T)) + "x} ";
      break;
    case UNSIGNED_DEC:
    case SIGNED_DEC:
      format_string = "{:" + std::to_string(3 * sizeof(T)) + "d} ";
      break;
    case FLOAT:
      format_string = "{:8.4f} ";  // todo, is this what we want?
      break;
    default:
      assert(false);
  }

  // loop over elts
  for (int i = 0; i < count; i++) {
    if ((i % elt_per_line) == 0) {
      // first in line, so we should print the GOAL address
      fmt::print(" 0x{:08x}: ", start_addr + (i * sizeof(T)));
    }

    // print the thing
    fmt::print(format_string, data[i]);

    if ((i % elt_per_line) == (elt_per_line - 1)) {
      // last in line, newline!
      fmt::print("\n");
    }
  }
  fmt::print("\n");
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

  PrintMode mode = HEX;

  if (args.has_named("print-mode")) {
    auto mode_name = symbol_string(args.get_named("print-mode"));
    if (mode_name == "hex") {
      mode = HEX;
    } else if (mode_name == "unsigned-dec") {
      mode = UNSIGNED_DEC;
    } else if (mode_name == "signed-dec") {
      mode = SIGNED_DEC;
    } else if (mode_name == "float") {
      mode = FLOAT;
    } else {
      throw_compile_error(form, "Unknown print-mode for :pm " + mode_name);
    }
  }

  if (!m_debugger.is_halted()) {
    throw_compile_error(
        form, "Cannot print memory, the debugger must be connected and the target must be halted.");
  }

  auto mem_size = elts * elt_size;
  if (mem_size > 1024 * 1024) {
    throw_compile_error(
        form,
        fmt::format(":pm used on over 1 MB of memory, this probably isn't what you meant to do."));
  }

  std::vector<u8> mem;
  mem.resize(mem_size);

  if (addr < EE_MAIN_MEM_LOW_PROTECT || (addr + mem_size) > EE_MAIN_MEM_SIZE) {
    throw_compile_error(form, ":pm memory out of range");
  }

  m_debugger.read_memory(mem.data(), mem_size, addr);

  switch (mode) {
    case HEX:
    case UNSIGNED_DEC:
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
          throw_compile_error(form, ":pm bad element size");
      }
      break;
    case SIGNED_DEC:
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
          throw_compile_error(form, ":pm bad element size");
      }
      break;
    case FLOAT:
      switch (elt_size) {
        case 4:
          mem_print((float*)mem.data(), elts, addr, mode);
          break;
        default:
          throw_compile_error(form, ":pm bad element size");
      }
      break;
    default:
      assert(false);
  }

  return get_none();
}

Val* Compiler::compile_di(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)form;
  (void)rest;
  (void)env;
  if (!m_debugger.is_halted()) {
    throw_compile_error(
        form,
        "Cannot get debug info, the debugger must be connected and the target must be halted.");
  }

  m_debugger.get_break_info();
  return get_none();
}

Val* Compiler::compile_disasm(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {{}, goos::ObjectType::INTEGER}, {});
  u32 addr = parse_address_spec(args.unnamed.at(0));
  u32 size = args.unnamed.at(1).as_int();

  if (!m_debugger.is_halted()) {
    throw_compile_error(
        form,
        "Cannot disassemble memory, the debugger must be connected and the target must be halted.");
  }

  if (size > 1024 * 1024) {
    throw_compile_error(
        form,
        fmt::format(
            ":disasm used on over 1 MB of memory, this probably isn't what you meant to do."));
  }

  std::vector<u8> mem;
  mem.resize(size);

  if (addr < EE_MAIN_MEM_LOW_PROTECT || (addr + size) > EE_MAIN_MEM_SIZE) {
    throw_compile_error(form, ":disasm memory out of range");
  }

  m_debugger.read_memory(mem.data(), size, addr);

  fmt::print("{}\n",
             disassemble_x86(mem.data(), mem.size(), m_debugger.get_x86_base_addr() + addr));

  return get_none();
}

Val* Compiler::compile_bp(const goos::Object& form, const goos::Object& rest, Env* env) {
  (void)env;
  auto args = get_va(form, rest);
  va_check(form, args, {{}}, {});

  if (!m_debugger.is_halted()) {
    throw_compile_error(
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
    throw_compile_error(
        form,
        "Cannot remove breakpoint, the debugger must be connected and the target must be halted.");
  }

  u32 addr = parse_address_spec(args.unnamed.at(0));
  m_debugger.remove_addr_breakpoint(addr);

  return get_none();
}