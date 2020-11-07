#pragma once

/*!
 * @file Listener.h
 * The Listener can connect to a Deci2Server for debugging.
 */

#ifndef JAK1_LISTENER_H
#define JAK1_LISTENER_H

#include <string>
#include <vector>
#include <thread>
#include <mutex>
#include <unordered_map>
#include "common/common_types.h"
#include "common/listener_common.h"
#include "common/cross_os_debug/xdbg.h"
#include "goalc/debugger/Debugger.h"

namespace listener {

struct LoadEntry {
  uint32_t segments[3] = {0, 0, 0};
  std::string load_string;
};

class Listener {
 public:
  static constexpr int BUFFER_SIZE = 32 * 1024 * 1024;
  Listener();
  ~Listener();
  bool connect_to_target(int n_tries = 1,
                         const std::string& ip = "127.0.0.1",
                         int port = DECI2_PORT);
  void record_messages(ListenerMessageKind kind);
  std::vector<std::string> stop_recording_messages();
  bool is_connected() const;
  void send_reset(bool shutdown);
  void send_poke();
  void disconnect();
  void send_code(std::vector<uint8_t>& code);
  void add_debugger(Debugger* debugger);
  bool most_recent_send_was_acked() const { return got_ack; }
  bool get_load_entry(const std::string& name, LoadEntry* out = nullptr);
  std::vector<std::string> get_all_loaded();

 private:
  void add_load(const std::string& name, const LoadEntry& le);
  void do_unload(const std::string& name);

  void send_buffer(int sz);
  bool wait_for_ack();
  void handle_output_message(const char* msg);

  char* m_buffer = nullptr;             //! buffer for incoming messages
  bool m_connected = false;             //! do we think we are connected?
  bool receive_thread_running = false;  //! is the receive thread unjoined?
  int listen_socket = -1;               //! socket
  bool got_ack = false;
  bool waiting_for_ack = false;

  Debugger* m_debugger = nullptr;

  std::thread rcv_thread;
  std::mutex rcv_mtx;
  void receive_func();
  ListenerMessageKind filter = ListenerMessageKind::MSG_INVALID;
  std::vector<std::string> message_record;
  std::unordered_map<std::string, LoadEntry> m_load_entries;
  char ack_recv_buff[512];
};
}  // namespace listener

#endif  // JAK1_LISTENER_H
