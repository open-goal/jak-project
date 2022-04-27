#include "XSocketServer.h"

#include "third-party/fmt/core.h"

#include "common/cross_sockets/XSocket.h"

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif

XSocketServer::XSocketServer(std::function<bool()> shutdown_callback,
                             int _tcp_port,
                             int _buffer_size)
    : want_exit_callback(std::move(shutdown_callback)) {
  tcp_port = _tcp_port;
  buffer_size = _buffer_size;
  buffer = new char[_buffer_size];
}

XSocketServer::~XSocketServer() {
  shutdown_server();
}

void XSocketServer::shutdown_server() {
  // Close the listening and accepted socket socket
  close_server_socket();
  close_socket(accepted_socket);

  // If the accept thread is still running (nothing ever connected)
  // kill it and clean it up
  if (accept_thread_running) {
    kill_accept_thread = true;
    accept_thread.join();
    accept_thread_running = false;
  }

  // Cleanup our buffer
  delete[] buffer;
}

bool XSocketServer::init_server() {
  listening_socket = open_socket(AF_INET, SOCK_STREAM, 0);
  if (listening_socket < 0) {
    listening_socket = -1;
    return false;
  }

#ifdef __linux
  int server_socket_opt = SO_REUSEADDR | SO_REUSEPORT;
#elif _WIN32
  int server_socket_opt = SO_EXCLUSIVEADDRUSE;
#endif

  int opt = 1;
  if (set_socket_option(listening_socket, SOL_SOCKET, server_socket_opt, &opt, sizeof(opt)) < 0) {
    close_server_socket();
    return false;
  };

  if (set_socket_option(listening_socket, TCP_SOCKET_LEVEL, TCP_NODELAY, &opt, sizeof(opt)) < 0) {
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
    fmt::print("[XSocketServer:{}] failed to bind\n", tcp_port);
    close_server_socket();
    return false;
  }

  if (listen(listening_socket, 0) < 0) {
    fmt::print("[XSocketServer:{}] failed to listen\n", tcp_port);
    close_server_socket();
    return false;
  }

  server_initialized = true;
  accept_thread_running = true;
  kill_accept_thread = false;
  accept_thread = std::thread(&XSocketServer::accept_thread_func, this);
  fmt::print("[XSocketServer:{}] awaiting connections\n", tcp_port);
  return true;
}

void XSocketServer::close_server_socket() {
  close_socket(listening_socket);
  listening_socket = -1;
}

void XSocketServer::accept_thread_func() {
  socklen_t l = sizeof(addr);
  while (!kill_accept_thread) {
    accepted_socket = accept_socket(listening_socket, (sockaddr*)&addr, &l);
    if (accepted_socket >= 0) {
      set_socket_timeout(accepted_socket, 100000);
      write_on_accept();
      client_connected = true;
      return;
    }
  }
}

bool XSocketServer::wait_for_connection() {
  if (client_connected) {
    if (accept_thread_running) {
      accept_thread.join();
      accept_thread_running = false;
    }
    return true;
  } else {
    return false;
  }
}

void XSocketServer::lock() {
  server_mutex.lock();
}

void XSocketServer::unlock() {
  server_mutex.unlock();
}
