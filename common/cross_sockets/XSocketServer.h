#pragma once

#include "common/cross_sockets/XSocket.h"

#include <thread>
#include "common/common_types.h"
#include <functional>
#include <mutex>

/// @brief A cross platform generic socket server implementation
class XSocketServer {
 public:
  static constexpr int DEF_BUFFER_SIZE = 32 * 1024 * 1024;
  XSocketServer(std::function<bool()> shutdown_callback,
                int _tcp_port,
                int _buffer_size = DEF_BUFFER_SIZE);
  ~XSocketServer();
  bool init_server();
  void shutdown_server();
  void close_server_socket();

  bool wait_for_connection();
  void lock();
  void unlock();

  // Abstract methods -- use-case dependent
  virtual void write_on_accept() = 0;
  virtual void read_data() = 0;
  virtual void send_data(void* buf, u16 len) = 0;

 protected:
  int buffer_size;
  int tcp_port;
  struct sockaddr_in addr = {};
  int listening_socket = -1;
  int accepted_socket = -1;
  char* buffer = nullptr;

  bool kill_accept_thread = false;
  bool server_initialized = false;
  bool accept_thread_running = false;
  bool client_connected = false;

  std::function<bool()> want_exit_callback;
  std::thread accept_thread;

  std::mutex server_mutex;

  void accept_thread_func();
};
