#include "DebugServer.h"

#include <algorithm>
#include <cmath>

#include "common/cross_sockets/XSocket.h"
#include "common/goal_constants.h"
#include "common/util/math_util.h"
#include "common/log/log.h"
#include "common/versions/versions.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/emitter/Register.h"

#include "fmt/format.h"

namespace {

struct GprName {
  const char* name;
  const char* label;
  const char* role;
  const char* group;
};

constexpr GprName GPR_NAMES[16] = {
    {"rax", "v0", "return value (rax)", "general"},
    {"rcx", "arg3", "rcx", "arg"},
    {"rdx", "arg2", "rdx", "arg"},
    {"rbx", "rbx", "saved", "general"},
    {"rsp", "rsp", "stack pointer", "special"},
    {"rbp", "rbp", "saved", "general"},
    {"rsi", "arg1", "rsi", "arg"},
    {"rdi", "arg0", "rdi", "arg"},
    {"r8", "arg4", "r8", "arg"},
    {"r9", "arg5", "r9", "arg"},
    {"r10", "arg6", "r10", "arg"},
    {"r11", "arg7", "r11", "arg"},
    {"r12", "r12", "saved", "general"},
    {"r13", "pp", "current process (r13)", "special"},
    {"r14", "s7", "symbol table (r14)", "special"},
    {"r15", "ee", "EE memory base (r15)", "special"},
};

constexpr int ARG_REGISTER_ORDER[8] = {
    emitter::RDI, emitter::RSI, emitter::RDX, emitter::RCX,
    emitter::R8,  emitter::R9,  emitter::R10, emitter::R11,
};

s64 sign_extend_from(u64 raw, int size) {
  switch (size) {
    case 1:
      return (s8)raw;
    case 2:
      return (s16)raw;
    case 4:
      return (s32)raw;
    default:
      return (s64)raw;
  }
}

constexpr double METER_LENGTH = 4096.0;
constexpr double DEGREES_PER_ROT = 65536.0;
constexpr double TICKS_PER_SECOND = 300.0;

std::string format_unit_number(double value) {
  if (!std::isfinite(value)) {
    return fmt::format("{}", value);
  }
  std::string result = fmt::format("{:.4f}", value);
  if (result.find('.') != std::string::npos) {
    result.erase(result.find_last_not_of('0') + 1);
    if (!result.empty() && result.back() == '.') {
      result.pop_back();
    }
  }
  return result;
}

json float_special_types(double value) {
  json out = json::array();
  out.push_back(fmt::format("(meters {})", format_unit_number(value / METER_LENGTH)));
  out.push_back(fmt::format("(degrees {})", format_unit_number(value * 360.0 / DEGREES_PER_ROT)));
  return out;
}

json int_special_types(double value) {
  json out = json::array();
  out.push_back(fmt::format("(seconds {})", format_unit_number(value / TICKS_PER_SECOND)));
  return out;
}

std::string format_enum_value(const EnumType* enum_type, u64 raw, int size) {
  const std::string enum_name = enum_type->get_name();

  if (enum_type->is_bitfield()) {
    std::vector<std::pair<s64, std::string>> set_bits;
    u64 unaccounted = raw;
    for (const auto& [name, bit] : enum_type->entries()) {
      const u64 mask = (u64)1 << (u64)bit;
      if (raw & mask) {
        set_bits.emplace_back(bit, name);
        unaccounted &= ~mask;
      }
    }
    std::sort(set_bits.begin(), set_bits.end());

    std::string form = "(" + enum_name;
    for (const auto& [bit, name] : set_bits) {
      (void)bit;
      form += " " + name;
    }
    form += ")";

    if (unaccounted) {
      return fmt::format("{} 0x{:x} - unknown bits 0x{:x}", form, raw, unaccounted);
    }
    return fmt::format("{} 0x{:x}", form, raw);
  }

  const s64 value = sign_extend_from(raw, size);
  for (const auto& [name, entry_value] : enum_type->entries()) {
    if (entry_value == value) {
      return fmt::format("({} {}) {}", enum_name, name, value);
    }
  }
  return fmt::format("{} - no matching entry in {}", value, enum_name);
}

std::string format_symbol_name(const std::string& name) {
  if (name == "#f" || name == "#t") {
    return name;
  }
  return "'" + name;
}

int element_stride(const Type* type, bool is_inline) {
  if (!type) {
    return 4;
  }
  if (is_inline && type->is_reference()) {
    return align(type->get_size_in_memory(), type->get_inline_array_stride_alignment());
  }
  return type->get_load_size();
}

std::string signal_kind_to_reason(xdbg::SignalInfo::Kind kind) {
  switch (kind) {
    case xdbg::SignalInfo::BREAK:
      return "breakpoint";
    case xdbg::SignalInfo::SEGFAULT:
      return "segfault";
    case xdbg::SignalInfo::MATH_EXCEPTION:
      return "math exception";
    case xdbg::SignalInfo::ILLEGAL_INSTR:
      return "illegal instruction";
    case xdbg::SignalInfo::DISAPPEARED:
      return "exited";
    default:
      return "unknown";
  }
}

}  // namespace

DebugServer::~DebugServer() {
  for (const int& sock : m_client_sockets) {
    close_socket(sock);
  }
}

void DebugServer::post_init() {
  lg::debug("[DebugServer:{}:{}] awaiting connections", tcp_port, listening_socket);
}

void DebugServer::set_compiler(Compiler* compiler, std::mutex* compiler_mutex) {
  m_compiler = compiler;
  m_compiler_mutex = compiler_mutex;
  m_stop_callback_installed = false;
}

void DebugServer::install_stop_callback() {
  if (m_stop_callback_installed || !m_compiler) {
    return;
  }
  m_compiler->get_debugger().set_stop_callback([this](xdbg::SignalInfo::Kind kind) {
    json body;
    body["reason"] = signal_kind_to_reason(kind);
    push_event(kind == xdbg::SignalInfo::DISAPPEARED ? "terminated" : "stopped", body);
  });
  m_stop_callback_installed = true;
}

void DebugServer::push_event(const std::string& event_name, const json& body) {
  json event;
  event["event"] = event_name;
  event["body"] = body;

  std::lock_guard<std::mutex> lock(m_event_mutex);
  m_event_queue.push(event.dump());
}

void DebugServer::send_line(int socket, const std::string& line) {
  const std::string payload = line + "\n";
  auto resp = write_to_socket(socket, payload.c_str(), (int)payload.size());
  if (resp == -1) {
    lg::warn("[DebugServer:{}] client disconnected while writing", tcp_port);
    close_socket(socket);
    m_client_sockets.erase(socket);
  }
}

void DebugServer::accept_new_clients() {
#ifdef OS_POSIX
  socklen_t addr_len = sizeof(addr);
#else
  int addr_len = sizeof(addr);
#endif
  auto new_socket = accept_socket(listening_socket, (sockaddr*)&addr, &addr_len);
  if (new_socket < 0) {
    return;
  }

  if ((int)m_client_sockets.size() >= max_clients) {
    lg::warn("[DebugServer:{}] maximum clients reached, rejecting connection", tcp_port);
    close_socket(new_socket);
    return;
  }

  lg::info("[DebugServer:{}] new connection: {}", tcp_port, address_to_string(addr));
  m_client_sockets.insert(new_socket);

  // say hello, so the client can confirm it's talking to the right thing
  json hello;
  hello["event"] = "hello";
  hello["body"]["version"] =
      fmt::format("{}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  send_line(new_socket, hello.dump());
}

void DebugServer::service_client(int socket) {
  const int got = read_from_socket(socket, m_read_buffer.data(), (int)m_read_buffer.size());
  if (got == 0) {
    lg::warn("[DebugServer:{}] client disconnected", tcp_port);
    close_socket(socket);
    m_client_sockets.erase(socket);
    return;
  }
  if (got < 0) {
    // nothing to read right now (the socket is non-blocking / timed out)
    return;
  }

  m_pending_input.append(m_read_buffer.data(), got);

  // requests are newline delimited, handle every complete one we have
  size_t newline_pos;
  while ((newline_pos = m_pending_input.find('\n')) != std::string::npos) {
    const std::string line = m_pending_input.substr(0, newline_pos);
    m_pending_input.erase(0, newline_pos + 1);
    if (line.empty()) {
      continue;
    }

    json response;
    auto parsed = safe_parse_json(line);
    if (!parsed) {
      response["ok"] = false;
      response["error"] = "malformed json request";
    } else {
      response = handle_request(*parsed);
    }
    send_line(socket, response.dump());

    if (m_client_sockets.find(socket) == m_client_sockets.end()) {
      // the write above disconnected us
      return;
    }
  }
}

void DebugServer::flush_events() {
  std::queue<std::string> to_send;
  {
    std::lock_guard<std::mutex> lock(m_event_mutex);
    std::swap(to_send, m_event_queue);
  }

  while (!to_send.empty()) {
    const std::string line = to_send.front();
    to_send.pop();
    // events go to everyone connected
    for (auto it = m_client_sockets.begin(); it != m_client_sockets.end();) {
      const int sock = *it;
      ++it;
      send_line(sock, line);
    }
  }
}

void DebugServer::run_once() {
  install_stop_callback();

  // wait for activity on the listening socket or any client with a short timeout, so that queued
  // events still go out promptly when nobody is sending us anything
  fd_set read_sockets;
  FD_ZERO(&read_sockets);
  FD_SET(listening_socket, &read_sockets);
  int max_sd = listening_socket;
  for (const int& sock : m_client_sockets) {
    if (sock > max_sd) {
      max_sd = sock;
    }
    if (sock > 0) {
      FD_SET(sock, &read_sockets);
    }
  }

  struct timeval timeout = {0, 50000};
  const auto activity = select(max_sd + 1, &read_sockets, nullptr, nullptr, &timeout);
  if (activity < 0 && errno != EINTR) {
    lg::error("[DebugServer:{}] select error: {}", tcp_port, strerror(errno));
    return;
  }

  if (FD_ISSET(listening_socket, &read_sockets)) {
    accept_new_clients();
  }

  // copy, because servicing a client can close it and mutate the set
  const std::set<int> clients = m_client_sockets;
  for (const int sock : clients) {
    if (FD_ISSET(sock, &read_sockets) && m_client_sockets.find(sock) != m_client_sockets.end()) {
      service_client(sock);
    }
  }

  flush_events();
}

json DebugServer::handle_request(const json& request) {
  json response;
  if (request.contains("seq")) {
    response["seq"] = request["seq"];
  }

  const std::string cmd = request.value("cmd", "");
  const json args = request.contains("args") ? request["args"] : json::object();

  if (!m_compiler || !m_compiler_mutex) {
    response["ok"] = false;
    response["error"] = "compiler is not available";
    return response;
  }

  try {
    std::lock_guard<std::mutex> lock(*m_compiler_mutex);
    m_compiler->get_debugger().refresh_break_state();

    json body;
    if (cmd == "status") {
      body = cmd_status();
    } else if (cmd == "attach") {
      body = cmd_attach();
    } else if (cmd == "detach") {
      body = cmd_detach();
    } else if (cmd == "pause") {
      body = cmd_pause();
    } else if (cmd == "continue") {
      body = cmd_continue();
    } else if (cmd == "step") {
      body = cmd_step(args);
    } else if (cmd == "set-breakpoints") {
      body = cmd_set_breakpoints(args);
    } else if (cmd == "stack") {
      body = cmd_stack();
    } else if (cmd == "registers") {
      body = cmd_registers();
    } else if (cmd == "read-memory") {
      body = cmd_read_memory(args);
    } else if (cmd == "evaluate") {
      body = cmd_evaluate(args);
    } else if (cmd == "inspect") {
      body = cmd_inspect(args);
    } else if (cmd == "locals") {
      body = cmd_locals();
    } else {
      response["ok"] = false;
      response["error"] = fmt::format("unknown command '{}'", cmd);
      return response;
    }

    response["ok"] = true;
    response["body"] = body;
  } catch (const std::exception& e) {
    response["ok"] = false;
    response["error"] = e.what();
  }

  return response;
}

json DebugServer::cmd_status() {
  auto& dbg = m_compiler->get_debugger();
  json body;
  body["valid"] = dbg.is_valid();
  body["attached"] = dbg.is_attached();
  body["halted"] = dbg.is_halted();
  body["running"] = dbg.is_running();
  return body;
}

json DebugServer::cmd_attach() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_valid()) {
    throw std::runtime_error("no valid debug context - is the game running and connected?");
  }
  if (dbg.is_attached()) {
    return cmd_status();
  }
  if (!dbg.attach_and_break()) {
    throw std::runtime_error("failed to attach to the target");
  }
  return cmd_status();
}

json DebugServer::cmd_detach() {
  auto& dbg = m_compiler->get_debugger();
  if (dbg.is_attached()) {
    if (dbg.is_running()) {
      dbg.set_suppress_stop_reporting(true);
      dbg.do_break();
    }

    m_file_breakpoints.clear();
    clear_inspect_handles();
    dbg.detach();
    dbg.set_suppress_stop_reporting(false);
  }
  return cmd_status();
}

json DebugServer::cmd_pause() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached()) {
    throw std::runtime_error("not attached");
  }
  if (dbg.is_halted()) {
    return cmd_status();
  }
  if (!dbg.do_break()) {
    throw std::runtime_error("failed to break");
  }
  return cmd_status();
}

json DebugServer::cmd_continue() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }
  clear_inspect_handles();
  if (!dbg.resume_from_break()) {
    throw std::runtime_error("failed to continue");
  }
  push_event("continued", json::object());
  return cmd_status();
}

json DebugServer::cmd_step(const json& args) {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }

  const std::string kind_str = args.value("kind", "over");
  StepKind kind = StepKind::OVER;
  if (kind_str == "in") {
    kind = StepKind::INTO;
  } else if (kind_str == "out") {
    kind = StepKind::OUT;
  }

  clear_inspect_handles();
  if (!dbg.do_step(kind)) {
    throw std::runtime_error(fmt::format("failed to step {}", kind_str));
  }

  // the watcher stays quiet during a step, so announce the stop ourselves
  push_event("stopped", describe_stop("step"));
  return cmd_status();
}

json DebugServer::cmd_set_breakpoints(const json& args) {
  auto& dbg = m_compiler->get_debugger();
  const std::string file = args.value("file", "");
  if (file.empty()) {
    throw std::runtime_error("set-breakpoints requires a file");
  }

  // we cannot set breakpoints while the game is running because we need to write to the memory,
  // so stop first and temporarily suppress reporting stops
  const bool was_running = dbg.is_attached() && dbg.is_running();
  if (was_running) {
    dbg.set_suppress_stop_reporting(true);
    dbg.do_break();
  }

  // DAP hands us the complete set for the file every time, so clear what we had
  auto existing = m_file_breakpoints.find(file);
  if (existing != m_file_breakpoints.end()) {
    if (dbg.is_attached() && dbg.is_halted()) {
      for (u32 addr : existing->second) {
        dbg.remove_addr_breakpoint(addr);
      }
    }
    m_file_breakpoints.erase(existing);
  }

  json result = json::array();
  std::vector<u32> armed;

  if (args.contains("lines")) {
    for (const auto& line_json : args["lines"]) {
      const int line = line_json.get<int>();
      json entry;
      entry["line"] = line;

      auto resolved = dbg.resolve_source_breakpoint(file, line);
      if (resolved.empty()) {
        entry["verified"] = false;
        entry["message"] =
            "no compiled code on or after this line (has the file been compiled by goalc?)";
        result.push_back(entry);
        continue;
      }

      // a line can compile into more than one function (inlining, macros), so add all of them,
      // but report the first back as the resolved location
      bool armed_any = false;
      bool any_loaded = false;
      for (const auto& bp : resolved) {
        if (!bp.loaded) {
          continue;
        }
        any_loaded = true;
        if (dbg.is_attached() && dbg.is_halted()) {
          dbg.add_addr_breakpoint(bp.goal_addr);
          armed.push_back(bp.goal_addr);
          armed_any = true;
        }
      }

      entry["verified"] = armed_any;
      entry["line"] = resolved.front().line;
      entry["addr"] = resolved.front().goal_addr;
      entry["function"] = resolved.front().function_name;
      entry["object"] = resolved.front().object_name;
      if (!armed_any) {
        entry["message"] =
            any_loaded
                ? "resolved, but the target must be attached and halted to arm this breakpoint"
                : fmt::format("resolved to {}, but object '{}' is not loaded in the target",
                              resolved.front().function_name, resolved.front().object_name);
      }
      result.push_back(entry);
    }
  }

  m_file_breakpoints[file] = armed;

  if (was_running) {
    // put the target back the way we found it
    if (dbg.is_halted()) {
      dbg.resume_from_break();
    }
    dbg.set_suppress_stop_reporting(false);
  }

  json body;
  body["breakpoints"] = result;
  return body;
}

json DebugServer::cmd_stack() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }

  json frames = json::array();
  int id = 0;
  for (const auto& frame : dbg.get_source_stack_frames()) {
    json f;
    f["id"] = id++;
    f["name"] = frame.function_name;
    f["object"] = frame.object_name;
    f["addr"] = frame.goal_rip;
    f["rsp"] = fmt::format("0x{:016x}", frame.rsp);
    if (frame.source) {
      f["file"] = frame.source->filename;
      f["line"] = frame.source->line;
      f["column"] = frame.source->column;
      f["lineText"] = frame.source->line_text;
    }
    frames.push_back(f);
  }

  json body;
  body["frames"] = frames;
  return body;
}

json DebugServer::cmd_registers() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }
  if (!dbg.regs_valid()) {
    throw std::runtime_error("register values are not available");
  }

  const auto& regs = dbg.get_regs();
  // 64 bit values go out as hex strings, but JSON numbers are doubles on the other end, which
  // would silently round anything past 2^53
  auto describe_gpr = [&](int i) {
    json r;
    r["name"] = GPR_NAMES[i].name;
    r["label"] = GPR_NAMES[i].label;
    r["role"] = GPR_NAMES[i].role;
    r["group"] = GPR_NAMES[i].group;
    r["value"] = fmt::format("0x{:016x}", regs.gprs[i]);

    // GOAL pointer offset
    const u32 goal_value = u32(regs.gprs[i]);
    if (i == emitter::R13) {
      r["goalValue"] = goal_value;
      // try to get the type of the current process pointer and display it
      auto type_name = runtime_type_of_basic(goal_value);
      if (type_name) {
        r["detail"] = *type_name;
      }
    } else if (i == emitter::R14 || i == emitter::R15) {
      r["goalValue"] = goal_value;
    }
    return r;
  };

  json special = json::array();
  json args = json::array();
  json general = json::array();

  for (int i = 0; i < 16; i++) {
    const std::string group = GPR_NAMES[i].group;
    if (group == "special") {
      special.push_back(describe_gpr(i));
    } else if (group == "general") {
      general.push_back(describe_gpr(i));
    }
  }
  // arguments go in arg order, not register order
  for (int i = 0; i < 8; i++) {
    args.push_back(describe_gpr(ARG_REGISTER_ORDER[i]));
  }

  json gprs = json::array();
  for (int i = 0; i < 16; i++) {
    gprs.push_back(describe_gpr(i));
  }

  json xmms = json::array();
  for (int i = 0; i < 16; i++) {
    json r;
    r["name"] = fmt::format("xmm{}", i);
    float as_float[4];
    memcpy(as_float, &regs.xmms[i], sizeof(as_float));
    r["floats"] = {as_float[0], as_float[1], as_float[2], as_float[3]};
    u64 as_u64[2];
    memcpy(as_u64, &regs.xmms[i], sizeof(as_u64));
    r["lo"] = fmt::format("0x{:016x}", as_u64[0]);
    r["hi"] = fmt::format("0x{:016x}", as_u64[1]);
    xmms.push_back(r);
  }

  json body;
  body["special"] = special;
  body["args"] = args;
  body["general"] = general;
  body["gprs"] = gprs;
  body["xmms"] = xmms;
  body["rip"] = fmt::format("0x{:016x}", regs.rip);
  body["goalRip"] = u32(dbg.get_normalized_rip() - dbg.get_x86_base_addr());
  return body;
}

json DebugServer::cmd_read_memory(const json& args) {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }

  const u32 addr = args.value("addr", 0u);
  const int size = std::min(args.value("size", 16), 4096);
  if (size <= 0) {
    throw std::runtime_error("size must be positive");
  }

  std::vector<u8> buffer(size);
  if (!dbg.read_memory_if_safe(buffer.data(), size, addr)) {
    throw std::runtime_error(fmt::format("could not read {} bytes at 0x{:x}", size, addr));
  }

  std::string hex;
  hex.reserve(size * 2);
  for (u8 b : buffer) {
    hex += fmt::format("{:02x}", b);
  }

  json body;
  body["addr"] = addr;
  body["size"] = size;
  body["data"] = hex;
  return body;
}

json DebugServer::cmd_evaluate(const json& args) {
  auto& dbg = m_compiler->get_debugger();
  std::string expr = args.value("expr", "");
  // trim
  while (!expr.empty() && std::isspace((unsigned char)expr.front())) {
    expr.erase(expr.begin());
  }
  while (!expr.empty() && std::isspace((unsigned char)expr.back())) {
    expr.pop_back();
  }
  if (expr.empty()) {
    throw std::runtime_error("nothing to evaluate");
  }

  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }

  json body;

  const bool is_hex = expr.size() > 2 && expr[0] == '0' && (expr[1] == 'x' || expr[1] == 'X');
  const bool is_dec =
      std::all_of(expr.begin(), expr.end(), [](char c) { return std::isdigit((unsigned char)c); });
  if (is_hex || is_dec) {
    u32 addr = 0;
    try {
      addr = (u32)std::stoul(expr, nullptr, is_hex ? 16 : 10);
    } catch (const std::exception&) {
      throw std::runtime_error(fmt::format("could not parse '{}' as an address", expr));
    }
    u32 word = 0;
    if (!dbg.read_memory_if_safe<u32>(&word, addr)) {
      throw std::runtime_error(fmt::format("could not read memory at 0x{:x}", addr));
    }
    body["kind"] = "memory";
    body["addr"] = addr;
    body["result"] = fmt::format("0x{:08x} ({})", word, (s32)word);
    body["info"] = dbg.get_info_about_addr(addr);
    if (auto type_name = runtime_type_of_basic(addr)) {
      body["result"] = fmt::format("0x{:08x} ({})", addr, *type_name);
      body["objectType"] = *type_name;
      body["ref"] = make_inspect_handle({addr, *type_name, 0, false});
    }
    return body;
  }

  // try as symbol
  const u32 sym_addr = dbg.get_symbol_address(expr);
  if (!sym_addr) {
    throw std::runtime_error(fmt::format("no symbol named '{}'", expr));
  }
  u32 value = 0;
  if (!dbg.get_symbol_value(expr, &value)) {
    throw std::runtime_error(fmt::format("could not read the value of '{}'", expr));
  }

  body["kind"] = "symbol";
  body["addr"] = sym_addr;
  body["value"] = value;
  body["result"] = fmt::format("0x{:08x} ({})", value, (s32)value);
  body["info"] = dbg.get_info_about_addr(value);

  // try to grab all the fields for basics
  if (auto type_name = runtime_type_of_basic(value)) {
    if (*type_name == "string") {
      body["result"] = fmt::format("\"{}\"", read_goal_string(value));
    } else {
      body["result"] = fmt::format("0x{:08x} ({})", value, *type_name);
    }
    body["objectType"] = *type_name;
    body["ref"] = make_inspect_handle({value, *type_name, 0, false});
  }
  return body;
}

int DebugServer::make_inspect_handle(const InspectTarget& target) {
  const int handle = m_next_inspect_handle++;
  m_inspect_handles[handle] = target;
  return handle;
}

// clear inspect handles if we step forward since they may become invalid
void DebugServer::clear_inspect_handles() {
  m_inspect_handles.clear();
}

std::string DebugServer::read_goal_string(u32 str_addr) {
  auto& dbg = m_compiler->get_debugger();
  auto& types = m_compiler->type_system();

  int data_offset = 8;
  int tag_offset = BASIC_OFFSET;
  auto* string_type = types.lookup_type_no_throw("string");
  if (string_type) {
    tag_offset = string_type->get_offset();
    if (auto* structure = dynamic_cast<StructureType*>(string_type)) {
      for (const auto& field : structure->fields()) {
        if (field.name() == "data") {
          data_offset = field.offset();
          break;
        }
      }
    }
  }

  const u32 base_addr = str_addr - tag_offset;

  constexpr int kMaxChars = 128;
  std::string result;
  for (int i = 0; i < kMaxChars; i++) {
    u8 c = 0;
    if (!dbg.read_memory_if_safe<u8>(&c, base_addr + data_offset + i)) {
      break;
    }
    if (c == 0) {
      return result;
    }
    result.push_back((char)c);
  }
  return result + "...";
}

std::optional<std::string> DebugServer::runtime_type_of_basic(u32 ptr) {
  auto name = m_compiler->get_debugger().get_type_name_of_basic(ptr);
  if (name && m_compiler->type_system().lookup_type_no_throw(*name)) {
    return name;
  }
  return {};
}

std::optional<s32> DebugServer::read_word_field(const StructureType* structure,
                                                u32 base_addr,
                                                const std::string& name) {
  auto& dbg = m_compiler->get_debugger();
  for (const auto& field : structure->fields()) {
    if (field.name() == name && !field.is_dynamic() && !field.is_array()) {
      s32 value = 0;
      if (dbg.read_memory_if_safe<s32>(&value, base_addr + field.offset())) {
        return value;
      }
      return {};
    }
  }
  return {};
}

// try to generate a field description with the given TypeSpec
json DebugServer::describe_field_value(u32 addr, const TypeSpec& type_spec, bool is_inline) {
  auto& dbg = m_compiler->get_debugger();
  auto& types = m_compiler->type_system();

  const std::string base = type_spec.base_type();
  auto* type = types.lookup_type_no_throw(base);

  if (is_inline) {
    json out;
    out["type"] = type_spec.print();
    const u32 tagged = addr + (type ? type->get_offset() : 0);
    out["value"] = fmt::format("0x{:08x}", tagged);
    out["inline"] = true;
    out["ref"] = make_inspect_handle({tagged, base, 0, false});
    return out;
  }

  u64 raw = 0;
  if (type && !type->is_reference()) {
    const int size = type->get_load_size();
    if (!dbg.read_memory_if_safe((u8*)&raw, std::min(size, 8), addr)) {
      json out;
      out["type"] = type_spec.print();
      out["value"] = "<unreadable>";
      return out;
    }
  } else {
    u32 ptr = 0;
    if (!dbg.read_memory_if_safe<u32>(&ptr, addr)) {
      json out;
      out["type"] = type_spec.print();
      out["value"] = "<unreadable>";
      return out;
    }
    raw = ptr;
  }

  return describe_value_bits(raw, type_spec, addr);
}

json DebugServer::describe_value_bits(u64 raw, const TypeSpec& type_spec, std::optional<u32> addr) {
  auto& dbg = m_compiler->get_debugger();
  auto& types = m_compiler->type_system();

  json out;
  out["type"] = type_spec.print();

  const std::string base = type_spec.base_type();
  auto* type = types.lookup_type_no_throw(base);

  if (type && !type->is_reference()) {
    const int size = type->get_load_size();

    if (auto* enum_type = types.try_enum_lookup(type_spec)) {
      out["value"] = format_enum_value(enum_type, raw, size);
      return out;
    }

    auto* bitfield_type = dynamic_cast<BitFieldType*>(type);
    const bool is_duration =
        types.tc(TypeSpec("uint64"), type_spec) ||
        (types.lookup_type_no_throw("time-frame") && types.tc(TypeSpec("time-frame"), type_spec));

    // print #f instead of the address of s7 on handles, pointers and inline-arrays
    if (raw == (u64)(u32)raw && (bitfield_type || base == "pointer" || base == "inline-array")) {
      if (auto symbol_name = dbg.get_symbol_name_at_address((u32)raw)) {
        out["value"] = format_symbol_name(*symbol_name);
        return out;
      }
    }

    // print all fields of a bitfield type
    if (bitfield_type && !bitfield_type->fields().empty()) {
      out["value"] = fmt::format("0x{:x}", raw);
      if (addr) {
        out["ref"] = make_inspect_handle({*addr, base, 0, false});
      }
      return out;
    }

    if (base == "float") {
      float as_float = 0;
      memcpy(&as_float, &raw, sizeof(as_float));
      out["value"] = fmt::format("{}", as_float);
      out["units"] = float_special_types(as_float);
    } else if (types.tc(TypeSpec("uinteger"), type_spec)) {
      out["value"] = fmt::format("{} (0x{:x})", raw, raw);
      if (is_duration) {
        out["units"] = int_special_types((double)raw);
      }
    } else {
      const s64 as_signed = sign_extend_from(raw, size);
      out["value"] = fmt::format("{} (0x{:x})", as_signed, raw);
      if (is_duration) {
        out["units"] = int_special_types((double)as_signed);
      }
    }
    return out;
  }

  // reference types
  const u32 ptr = u32(raw);
  if (ptr == 0) {
    out["value"] = "0x0 (null)";
    return out;
  }

  if (base == "symbol") {
    auto name = dbg.get_symbol_name_at_address(ptr);
    out["value"] = name ? format_symbol_name(*name) : fmt::format("0x{:08x}", ptr);
    return out;
  }
  if (base == "type") {
    auto name = dbg.get_symbol_name_for_value(ptr);
    out["value"] = name ? *name : fmt::format("0x{:08x}", ptr);
    return out;
  }

  // if the value is inside the symbol table, print the symbol name
  if (auto symbol_name = dbg.get_symbol_name_at_address(ptr)) {
    out["value"] = format_symbol_name(*symbol_name);
    return out;
  }

  // resolve function names for function pointer fields
  if (types.tc(TypeSpec("function"), type_spec)) {
    std::string name;
    if (auto symbol_name = dbg.get_symbol_name_for_value(ptr)) {
      name = *symbol_name;
    } else {
      auto rip_info = dbg.get_rip_info(ptr + dbg.get_x86_base_addr());
      if (rip_info.knows_function) {
        name = rip_info.function_name;
      }
    }
    out["value"] =
        name.empty() ? fmt::format("0x{:08x}", ptr) : fmt::format("0x{:08x} ({})", ptr, name);
    if (!name.empty()) {
      out["function"] = name;
    }
    return out;
  }

  std::optional<std::string> runtime_type;
  if (types.tc(TypeSpec("basic"), type_spec)) {
    runtime_type = runtime_type_of_basic(ptr);
  }
  const std::string expand_as = runtime_type.value_or(base);

  if (expand_as == "string") {
    out["value"] = fmt::format("\"{}\"", read_goal_string(ptr));
    return out;
  }

  // for overlay fields (e.g. root in process-drawable), only print the runtime type
  if (runtime_type && *runtime_type != base) {
    out["type"] = *runtime_type;
  }
  out["value"] = fmt::format("0x{:08x}", ptr);
  out["ref"] = make_inspect_handle({ptr, expand_as, 0, false});
  return out;
}

// for dynamic fields of special types like inline-array-class, try to determine the length of the
// array in order to be able to read and print all array entries
void DebugServer::describe_dynamic_field(json& entry,
                                         const StructureType* structure,
                                         const std::string& object_type,
                                         u32 base_addr,
                                         u32 field_addr,
                                         const Field& field) {
  auto& dbg = m_compiler->get_debugger();
  auto& types = m_compiler->type_system();

  entry["type"] = field.type().print();
  entry["value"] = "<dynamic>";

  std::string element_type = field.type().base_type();
  bool inline_elements = field.is_inline();

  // boxed arrays
  if (types.tc(TypeSpec("array"), TypeSpec(object_type))) {
    if (auto content_type = read_word_field(structure, base_addr, "content-type")) {
      if (auto name = dbg.get_symbol_name_for_value((u32)*content_type)) {
        if (types.lookup_type_no_throw(*name)) {
          element_type = *name;
          inline_elements = false;
        }
      }
    }
  }

  if (!types.lookup_type_no_throw(element_type)) {
    return;
  }

  const auto length = read_word_field(structure, base_addr, "length");
  const auto allocated = read_word_field(structure, base_addr, "allocated-length");
  const auto count = length ? length : allocated;
  if (!count || *count < 0) {
    return;
  }

  entry["type"] = fmt::format("{} [{}]", element_type, *count);
  entry["value"] = (length && allocated && *length != *allocated)
                       ? fmt::format("0x{:08x} ({} of {} used)", field_addr, *length, *allocated)
                       : fmt::format("0x{:08x}", field_addr);
  if (*count > 0) {
    entry["ref"] = make_inspect_handle({field_addr, element_type, *count, inline_elements});
  }
}

json DebugServer::inspect_target(const InspectTarget& target) {
  auto& dbg = m_compiler->get_debugger();
  auto& types = m_compiler->type_system();

  json body;
  body["addr"] = target.addr;
  body["type"] = target.type;
  json fields = json::array();

  if (target.array_count > 0) {
    auto* element_type = types.lookup_type_no_throw(target.type);
    const int stride = element_stride(element_type, target.inline_elements);

    constexpr int kMaxElements = 256;
    const int count = std::min(target.array_count, kMaxElements);
    for (int i = 0; i < count; i++) {
      json entry = describe_field_value(
          target.addr + i * stride, TypeSpec(target.type),
          target.inline_elements && element_type && element_type->is_reference());
      entry["name"] = fmt::format("[{}]", i);
      fields.push_back(entry);
    }
    if (count < target.array_count) {
      json truncated;
      truncated["name"] = "...";
      truncated["type"] = "";
      truncated["value"] = fmt::format("{} more elements not shown", target.array_count - count);
      fields.push_back(truncated);
    }

    body["fields"] = fields;
    return body;
  }

  auto* type = types.lookup_type_no_throw(target.type);
  if (!type) {
    throw std::runtime_error(fmt::format("unknown type '{}'", target.type));
  }

  // bitfield types
  if (auto* bitfield_type = dynamic_cast<BitFieldType*>(type)) {
    u64 raw = 0;
    const int load_size = bitfield_type->get_load_size();
    if (!dbg.read_memory_if_safe((u8*)&raw, std::min(load_size, 8), target.addr)) {
      throw std::runtime_error(fmt::format("could not read memory at 0x{:x}", target.addr));
    }
    body["summary"] = fmt::format("0x{:x}", raw);

    for (const auto& bit_field : bitfield_type->fields()) {
      json entry;
      entry["name"] = bit_field.name();
      entry["type"] = bit_field.type().print();
      entry["bitOffset"] = bit_field.offset();
      entry["bitSize"] = bit_field.size();

      const u64 mask = bit_field.size() >= 64 ? ~(u64)0 : (((u64)1 << (u64)bit_field.size()) - 1);
      const u64 extracted = (raw >> (u64)bit_field.offset()) & mask;

      if (auto* enum_type = types.try_enum_lookup(bit_field.type())) {
        entry["value"] = format_enum_value(enum_type, extracted, (bit_field.size() + 7) / 8);
      } else if (types.tc(TypeSpec("uinteger"), bit_field.type())) {
        entry["value"] = fmt::format("{} (0x{:x})", extracted, extracted);
      } else {
        const int width = bit_field.size();
        s64 as_signed = (s64)extracted;
        if (width < 64 && (extracted & ((u64)1 << (u64)(width - 1)))) {
          as_signed = (s64)(extracted | ~mask);
        }
        entry["value"] = fmt::format("{} (0x{:x})", as_signed, extracted);
      }
      fields.push_back(entry);
    }

    body["fields"] = fields;
    return body;
  }

  // value types
  if (!type->is_reference()) {
    json entry = describe_field_value(target.addr, TypeSpec(target.type), false);
    entry["name"] = target.type;
    fields.push_back(entry);
    body["fields"] = fields;
    return body;
  }

  auto* structure = dynamic_cast<StructureType*>(type);
  if (!structure) {
    throw std::runtime_error(fmt::format("'{}' has no fields to inspect", target.type));
  }

  if (target.type == "string") {
    body["summary"] = fmt::format("\"{}\"", read_goal_string(target.addr));
  }

  // basic offset
  const u32 base_addr = target.addr - type->get_offset();

  // only display the last dynamic field (the others are inherited from parent types, we only care
  // about the last one)
  const Field* dynamic_field = nullptr;
  for (const auto& field : structure->fields()) {
    if (field.is_dynamic()) {
      dynamic_field = &field;
    }
  }

  for (const auto& field : structure->fields()) {
    if (field.is_dynamic() && &field != dynamic_field) {
      continue;
    }

    json entry;
    entry["name"] = field.name();
    entry["offset"] = field.offset();

    const u32 field_addr = base_addr + field.offset();

    if (field.is_dynamic()) {
      describe_dynamic_field(entry, structure, target.type, base_addr, field_addr, field);
      fields.push_back(entry);
      continue;
    }

    if (field.is_array()) {
      auto* element_type = types.lookup_type_no_throw(field.type().base_type());
      const int count = field.array_size();
      entry["type"] = fmt::format("{} [{}]", field.type().print(), count);
      entry["value"] = fmt::format("0x{:08x}", field_addr);
      if (element_type) {
        entry["ref"] =
            make_inspect_handle({field_addr, field.type().base_type(), count, field.is_inline()});
      }
      fields.push_back(entry);
      continue;
    }

    json described = describe_field_value(field_addr, field.type(), field.is_inline());
    described["name"] = field.name();
    described["offset"] = field.offset();
    fields.push_back(described);
  }

  body["fields"] = fields;
  return body;
}

json DebugServer::cmd_inspect(const json& args) {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }

  // either re-open something we handed out earlier...
  if (args.contains("ref")) {
    const int ref = args["ref"].get<int>();
    auto kv = m_inspect_handles.find(ref);
    if (kv == m_inspect_handles.end()) {
      throw std::runtime_error("that value is from an earlier stop and is no longer available");
    }
    return inspect_target(kv->second);
  }

  // ...or start from an address
  InspectTarget target;
  target.addr = args.value("addr", 0u);
  target.type = args.value("type", "");
  if (target.addr == 0) {
    throw std::runtime_error("inspect needs an addr or a ref");
  }

  if (target.type.empty()) {
    auto runtime_type = runtime_type_of_basic(target.addr);
    if (!runtime_type) {
      throw std::runtime_error(fmt::format(
          "can't work out the type of the object at 0x{:x} - pass one explicitly", target.addr));
    }
    target.type = *runtime_type;
  }

  return inspect_target(target);
}

json DebugServer::cmd_locals() {
  auto& dbg = m_compiler->get_debugger();
  if (!dbg.is_attached() || !dbg.is_halted()) {
    throw std::runtime_error("not attached and halted");
  }
  if (!dbg.regs_valid()) {
    throw std::runtime_error("register values are not available");
  }

  const auto& regs = dbg.get_regs();
  json variables = json::array();

  for (const auto& var : dbg.get_live_variables()) {
    const TypeSpec& type_spec = var.type;

    json entry;
    if (var.in_register) {
      u64 raw = 0;
      if (var.reg >= emitter::XMM0 && var.reg <= emitter::XMM15) {
        memcpy(&raw, &regs.xmms[var.reg - emitter::XMM0], sizeof(raw));
      } else if (var.reg >= 0 && var.reg < 16) {
        raw = regs.gprs[var.reg];
      } else {
        continue;
      }
      entry = describe_value_bits(raw, type_spec, {});
      entry["storage"] = var.reg >= emitter::XMM0 ? fmt::format("xmm{}", var.reg - emitter::XMM0)
                                                  : GPR_NAMES[var.reg].name;
    } else {
      entry = describe_field_value(var.stack_addr, type_spec, false);
      entry["storage"] = fmt::format("stack 0x{:x}", var.stack_addr);
      entry["addr"] = var.stack_addr;
    }

    entry["name"] = var.name;
    entry["parameter"] = var.is_parameter;
    variables.push_back(entry);
  }

  json body;
  body["variables"] = variables;
  return body;
}

json DebugServer::describe_stop(const std::string& reason) {
  auto& dbg = m_compiler->get_debugger();
  json body;
  body["reason"] = reason;

  if (dbg.is_attached() && dbg.is_halted() && dbg.regs_valid()) {
    const u32 goal_rip = u32(dbg.get_normalized_rip() - dbg.get_x86_base_addr());
    body["addr"] = goal_rip;
    auto loc = dbg.get_source_location(goal_rip);
    if (loc) {
      body["file"] = loc->filename;
      body["line"] = loc->line;
      body["column"] = loc->column;
    }
  }

  return body;
}
