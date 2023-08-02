/*!
 * @file xsocket.cpp
 * Cross platform socket library used for the listener.
 */

// clang-format off
#include "common/common_types.h"
#ifdef OS_POSIX
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <unistd.h>
#elif _WIN32
#define WIN32_LEAN_AND_MEAN
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "third-party/fmt/core.h"
#include "common/log/log.h"
// clang-format on

int open_socket(int af, int type, int protocol) {
#ifdef OS_POSIX
  return socket(af, type, protocol);
#elif _WIN32
  WSADATA wsaData = {0};
  int iResult = 0;
  // Initialize Winsock
  iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
  if (iResult != 0) {
    lg::error("WSAStartup failed: {}", iResult);
    return 1;
  }
  return socket(af, type, protocol);
#endif
}

int connect_socket(int socket, sockaddr* addr, int nameLen) {
  int result = connect(socket, addr, nameLen);
  if (result == -1) {
    return -1;
  }
  return result;
}

#ifdef OS_POSIX
int accept_socket(int socket, sockaddr* addr, socklen_t* addrLen) {
  return accept(socket, addr, addrLen);
}
int select_and_accept_socket(int socket, sockaddr* addr, socklen_t* addrLen, int microSeconds) {
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = microSeconds;
  // Use select so it can timeout, accept on the returned socket if it is correct
  fd_set read_sockets;
  FD_ZERO(&read_sockets);
  FD_SET(socket, &read_sockets);
  auto activity = select(socket + 1, &read_sockets, NULL, NULL, &timeout);
  if (activity > 0) {
    return accept(socket, addr, addrLen);
  }
  return -1;
}
#endif

#ifdef _WIN32
int accept_socket(int socket, sockaddr* addr, int* addrLen) {
  WSADATA wsaData = {0};
  int iResult = 0;
  // Initialize Winsock
  iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
  if (iResult != 0) {
    lg::error("WSAStartup failed: {}", iResult);
    return 1;
  }
  return accept(socket, addr, addrLen);
}

int select_and_accept_socket(int socket, sockaddr* addr, int* addrLen, int microSeconds) {
  WSADATA wsaData = {0};
  int iResult = 0;
  // Initialize Winsock
  iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
  if (iResult != 0) {
    printf("WSAStartup failed: %d\n", iResult);
    return 1;
  }
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = microSeconds;
  // Use select so it can timeout, accept on the returned socket if it is correct
  fd_set read_sockets;
  FD_ZERO(&read_sockets);
  FD_SET(socket, &read_sockets);
  auto activity = select(socket + 1, &read_sockets, NULL, NULL, &timeout);
  if (activity > 0) {
    return accept(socket, addr, addrLen);
  }
  return -1;
}
#endif

void close_socket(int sock) {
  if (sock < 0) {
    return;
  }
#ifdef OS_POSIX
  close(sock);
#elif _WIN32
  closesocket(sock);
  WSACleanup();
#endif
}

int set_socket_option(int socket, int level, int optname, const void* optval, int optlen) {
  int ret = setsockopt(socket, level, optname, (const char*)optval, optlen);
  if (ret < 0) {
    lg::error("Failed to setsockopt({},{}, {}, _, _) - Error: {}", socket, level, optname,
              strerror(errno));
  }
#ifdef _WIN32
  if (ret < 0) {
    int err = WSAGetLastError();
    lg::error("WSAGetLastError: {}", err);
  }
#endif
  return ret;
}

int set_socket_timeout(int socket, long microSeconds) {
#ifdef OS_POSIX
  struct timeval timeout = {};
  timeout.tv_sec = 0;
  timeout.tv_usec = microSeconds;
  int ret = setsockopt(socket, SOL_SOCKET, SO_RCVTIMEO, (struct timeval*)&timeout, sizeof(timeout));
  if (ret < 0) {
    printf("Failed to setsockopt(%d, %d, %d, _, _) - Error: %s\n", socket, SOL_SOCKET, SO_RCVTIMEO,
           strerror(errno));
  }
  return ret;
#elif _WIN32
  DWORD timeout = microSeconds / 1000;  // milliseconds
  // TODO - NOTE this might be bad / unreliable if the socket ends up being in a bad state
  // select() instead should be used, will do so if ends up being an issue in practice
  return set_socket_option(socket, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
#endif
}

int write_to_socket(int socket, const char* buf, int len) {
  int bytes_wrote = 0;
#ifdef OS_POSIX
  bytes_wrote = send(socket, buf, len, MSG_NOSIGNAL);
#elif _WIN32
  bytes_wrote = send(socket, buf, len, 0);
#endif
  if (bytes_wrote < 0) {
    lg::error("[XSocket:{}] Error writing to socket", socket);
  }
  return bytes_wrote;
}

int read_from_socket(int socket, char* buf, int len) {
#ifdef OS_POSIX
  return read(socket, buf, len);
#elif _WIN32
  return recv(socket, buf, len, 0);
#endif
}

bool socket_timed_out() {
#ifdef OS_POSIX
  return errno == EAGAIN;
#elif _WIN32
  auto err = WSAGetLastError();
  return err == WSAETIMEDOUT;
#endif
}

std::string address_to_string(const sockaddr_in& addr) {
  char ip_str[INET_ADDRSTRLEN];  // Buffer to hold the IP address string
  // Convert binary IP address to human-readable string using inet_ntop
  if (inet_ntop(AF_INET, &(addr.sin_addr), ip_str, INET_ADDRSTRLEN) == nullptr) {
    // Handle the error (e.g., log, throw an exception, etc.)
    return "Error: Unable to convert IP address";
  }
  return ip_str;
}
