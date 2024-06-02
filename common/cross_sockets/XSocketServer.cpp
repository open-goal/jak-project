// clang-format off
#include "XSocketServer.h"

#include "common/cross_sockets/XSocket.h"
#include "common/common_types.h"

#include "fmt/core.h"

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif
#include "common/log/log.h"
// clang-format on

XSocketServer::XSocketServer(std::function<bool()> shutdown_callback,
                             int _tcp_port,
                             int _buffer_size)
    : want_exit_callback(std::move(shutdown_callback)) {
  tcp_port = _tcp_port;
  buffer.resize(_buffer_size);
}

XSocketServer::~XSocketServer() {
  if (listening_socket >= 0) {
    close_server_socket();
  }
}

void XSocketServer::shutdown_server() {
  // Close the listening and accepted socket socket
  close_server_socket();
}

bool XSocketServer::init_server(bool failure_may_occur) {
  listening_socket = open_socket(AF_INET, SOCK_STREAM, 0);
  if (listening_socket < 0) {
    listening_socket = -1;
    return false;
  }

  int yes = 1;

#ifdef OS_POSIX
  if (set_socket_option(listening_socket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)) < 0) {
    close_server_socket();
    return false;
  }
  // macOS doesn't support setting multiple options at once, so we have to do this separately.
  if (set_socket_option(listening_socket, SOL_SOCKET, SO_REUSEPORT, &yes, sizeof(yes)) < 0) {
    close_server_socket();
    return false;
  }
#elif _WIN32
  if (set_socket_option(listening_socket, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, &yes, sizeof(yes)) < 0) {
    close_server_socket();
    return false;
  };
#endif

  if (set_socket_option(listening_socket, TCP_SOCKET_LEVEL, TCP_NODELAY, &yes, sizeof(yes)) < 0) {
    close_server_socket();
    return false;
  }

  if (set_socket_timeout(listening_socket, 100000) < 0) {
    close_server_socket();
    return false;
  }

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(tcp_port);

  if (bind(listening_socket, (sockaddr*)&addr, sizeof(addr)) < 0) {
    if (failure_may_occur) {
      lg::debug("[XSocketServer:{}] failed to bind", tcp_port);
    } else {
      lg::error("[XSocketServer:{}] failed to bind", tcp_port);
    }
    close_server_socket();
    return false;
  }

  if (listen(listening_socket, 0) < 0) {
    if (failure_may_occur) {
      lg::debug("[XSocketServer:{}] failed to listen", tcp_port);
    } else {
      lg::error("[XSocketServer:{}] failed to listen", tcp_port);
    }
    close_server_socket();
    return false;
  }

  server_initialized = true;
  lg::debug("[XSocketServer:{}] initialized", tcp_port);
  post_init();
  return true;
}

void XSocketServer::close_server_socket() {
  close_socket(listening_socket);
  listening_socket = -1;
}
