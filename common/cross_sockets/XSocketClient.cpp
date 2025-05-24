#include "XSocketClient.h"

#include <string>

#include "common/cross_sockets/XSocket.h"
#include "common/log/log.h"

#include "fmt/core.h"

// clang-format off
#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif

// clang-format on

XSocketClient::XSocketClient(int _tcp_port) {
  tcp_port = _tcp_port;
}

XSocketClient::~XSocketClient() {
  disconnect();
  client_socket = -1;
}

void XSocketClient::disconnect() {
  close_socket(client_socket);
  client_socket = -1;
}

bool XSocketClient::connect() {
  // Open Socket
  client_socket = open_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (client_socket < 0) {
    // TODO - log
    disconnect();
    return false;
  }

  addr.sin_family = AF_INET;
  // Convert IP address from text to binary form using inet_pton()
  if (inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr) <= 0) {
    lg::error("inet_pton() failed");
    return false;
  }
  addr.sin_port = htons(tcp_port);

  // Connect to server
  int result = connect_socket(client_socket, (sockaddr*)&addr, sizeof(addr));
  if (result == -1) {
    // TODO - log and close
    disconnect();
    return false;
  }

  return true;
}
