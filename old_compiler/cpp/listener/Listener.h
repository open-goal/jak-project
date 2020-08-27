#ifndef JAK_LISTENER_H
#define JAK_LISTENER_H

#include <string>
#include <thread>
#include <mutex>
#include <vector>
#include "shared_config.h"

class Listener {
 public:
  static constexpr int BUFFER_SIZE = 4096 * 4096;
  Listener();
  ~Listener();
  void listen_to_target(std::string ip = "127.0.0.1", int port = 8112);
  bool is_connected() { return connected; }

  void send_raw_data(const char* data, int size);
  void send_reset();
  void send_poke();
  void send_code(std::vector<uint8_t>& code);

  void receive_data();
  bool wait_for_ack();
  void clear_pending_incoming();
  std::string pop_pending();
  bool has_pending();
  void set_connected(bool con);
  bool receiver_got_ack = false;
  bool thread_running = false;

 private:
  void send_buffer(int sz);
  char* buffer;
  int socket_fd;
  bool connected = false;

  std::thread rcv_thread;
  std::mutex rcv_mtx;
  std::vector<std::string> pending_messages;

  char ack_recv_buff[512];
};

#endif  // JAK_LISTENER_H
