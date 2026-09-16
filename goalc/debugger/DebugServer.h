#pragma once

/*!
 * @file DebugServer.h
 * Front end for the OpenGOAL VS Code extension debugger.
 */

#include <atomic>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <queue>
#include <set>
#include <string>
#include <thread>
#include <vector>

#include "common/cross_sockets/XSocketServer.h"
#include "common/type_system/TypeSpec.h"
#include "common/util/json_util.h"

class Compiler;
class Field;
class StructureType;

class DebugServer : public XSocketServer {
 public:
  using XSocketServer::XSocketServer;
  virtual ~DebugServer();

  void post_init() override;
  void set_compiler(Compiler* compiler, std::mutex* compiler_mutex);
  void run_once();
  void push_event(const std::string& event_name, const json& body);

 private:
  Compiler* m_compiler = nullptr;
  std::mutex* m_compiler_mutex = nullptr;

  int max_clients = 4;
  std::vector<char> m_read_buffer = std::vector<char>(64 * 1024);
  std::set<int> m_client_sockets = {};
  std::string m_pending_input;

  std::mutex m_event_mutex;
  std::queue<std::string> m_event_queue;
  std::map<std::string, std::vector<u32>> m_file_breakpoints;

  struct InspectTarget {
    u32 addr = 0;
    std::string type;
    int array_count = 0;
    bool inline_elements = false;
  };
  std::map<int, InspectTarget> m_inspect_handles;
  int m_next_inspect_handle = 1;

  int make_inspect_handle(const InspectTarget& target);
  void clear_inspect_handles();

  bool m_stop_callback_installed = false;

  void accept_new_clients();
  void service_client(int socket);
  void flush_events();
  void send_line(int socket, const std::string& line);
  void install_stop_callback();

  json handle_request(const json& request);

  // command handlers
  json cmd_status();
  json cmd_attach();
  json cmd_detach();
  json cmd_pause();
  json cmd_continue();
  json cmd_step(const json& args);
  json cmd_set_breakpoints(const json& args);
  json cmd_stack();
  json cmd_registers();
  json cmd_read_memory(const json& args);
  json cmd_evaluate(const json& args);
  json cmd_inspect(const json& args);
  json cmd_locals();

  json inspect_target(const InspectTarget& target);
  json describe_field_value(u32 addr, const TypeSpec& type_spec, bool is_inline);
  json describe_value_bits(u64 raw, const TypeSpec& type_spec, std::optional<u32> addr);
  std::string read_goal_string(u32 str_addr);
  std::optional<std::string> runtime_type_of_basic(u32 ptr);
  std::optional<s32> read_word_field(const StructureType* structure,
                                     u32 base_addr,
                                     const std::string& name);
  void describe_dynamic_field(json& entry,
                              const StructureType* structure,
                              const std::string& object_type,
                              u32 base_addr,
                              u32 field_addr,
                              const Field& field);
  json describe_stop(const std::string& reason);
};
