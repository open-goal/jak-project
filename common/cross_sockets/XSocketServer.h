#pragma once

#include <functional>
#include <mutex>
#include <thread>
#include <vector>

#include "common/common_types.h"
#include "common/cross_sockets/XSocket.h"

/// @brief A cross platform generic socket server implementation
class XSocketServer {
 public:
  static constexpr int DEF_BUFFER_SIZE = 32 * 1024 * 1024;
  XSocketServer(std::function<bool()> shutdown_callback,
                int _tcp_port,
                int _buffer_size = DEF_BUFFER_SIZE);
  virtual ~XSocketServer();

  XSocketServer(const XSocketServer&) = delete;
  XSocketServer& operator=(const XSocketServer&) = delete;

  bool init_server();
  void shutdown_server();
  void close_server_socket();

  // Abstract methods -- use-case dependent
  virtual void post_init() = 0;

 protected:
  int tcp_port;
  struct sockaddr_in addr = {};
  int listening_socket = -1;
  std::vector<char> buffer;

  bool server_initialized = false;

  std::function<bool()> want_exit_callback;
};
