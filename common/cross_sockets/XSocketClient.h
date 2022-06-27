#pragma once

#include <functional>
#include <mutex>
#include <thread>

#include "common/common_types.h"
#include "common/cross_sockets/XSocket.h"

/// @brief A cross platform generic socket client implementation
class XSocketClient {
 public:
  XSocketClient(int _tcp_port);
  ~XSocketClient();

  XSocketClient(const XSocketClient&) = delete;
  XSocketClient& operator=(const XSocketClient&) = delete;

  bool connect();
  void disconnect();

  bool is_connected() { return client_socket != -1; }

 protected:
  int tcp_port;
  struct sockaddr_in addr = {};
  int client_socket = -1;
};
