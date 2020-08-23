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
#include "common/common_types.h"
#include "common/listener_common.h"

namespace listener {
class Listener {
 public:
  static constexpr int BUFFER_SIZE = 32 * 1024 * 1024;
  Listener();
  ~Listener();
  bool connect_to_target(const std::string& ip = "127.0.0.1", int port = DECI2_PORT);
  void record_messages(ListenerMessageKind kind);
  void stop_recording_messages();
  bool is_connected() const;
  void disconnect();


 private:
  char* m_buffer = nullptr; //! buffer for incoming messages
  bool m_connected = false; //! do we think we are connected?
  bool receive_thread_running = false; //! is the receive thread unjoined?
  int socket_fd = -1; //! socket
  bool got_ack = false;
  bool waiting_for_ack = false;

  std::thread rcv_thread;
  std::mutex rcv_mtx;
  void receive_func();
  ListenerMessageKind filter = ListenerMessageKind::MSG_INVALID;
  std::vector<std::string> message_record;
  char ack_recv_buff[512];
};
}


#endif  // JAK1_LISTENER_H
