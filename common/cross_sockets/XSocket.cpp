/*!
 * @file xsocket.cpp
 * Cross platform socket library used for the listener.
 */

#ifdef __linux
#include <sys/socket.h>
#include <netinet/tcp.h>
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

int open_socket(int af, int type, int protocol) {
#ifdef __linux
  return socket(af, type, protocol);
#elif _WIN32
  WSADATA wsaData = {0};
  int iResult = 0;
  // Initialize Winsock
  iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
  if (iResult != 0) {
    printf("WSAStartup failed: %d\n", iResult);
    return 1;
  }
  return socket(af, type, protocol);
#endif
}

#ifdef __linux
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
    printf("WSAStartup failed: %d\n", iResult);
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
#ifdef __linux
  close(sock);
#elif _WIN32
  closesocket(sock);
  WSACleanup();
#endif
}

int set_socket_option(int socket, int level, int optname, const void* optval, int optlen) {
  int ret = setsockopt(socket, level, optname, (const char*)optval, optlen);
  if (ret < 0) {
    printf("Failed to setsockopt(%d, %d, %d, _, _) - Error: %s\n", socket, level, optname,
           strerror(errno));
  }
#ifdef _WIN32
  if (ret < 0) {
    int err = WSAGetLastError();
    printf("WSAGetLastError: %d\n", err);
  }
#endif
  return ret;
}

int set_socket_timeout(int socket, long microSeconds) {
#ifdef __linux
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
  return set_socket_option(socket, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
#endif
}

int write_to_socket(int socket, const char* buf, int len) {
  int bytes_wrote = 0;
#ifdef __linux
  bytes_wrote = write(socket, buf, len);
#elif _WIN32
  bytes_wrote = send(socket, buf, len, 0);
#endif
  if (bytes_wrote < 0) {
    fmt::print(stderr, "[XSocket:{}] Error writing to socket\n", socket);
  }
  return bytes_wrote;
}

int read_from_socket(int socket, char* buf, int len) {
#ifdef __linux
  return read(socket, buf, len);
#elif _WIN32
  return recv(socket, buf, len, 0);
#endif
}

bool socket_timed_out() {
#ifdef __linux
  return errno == EAGAIN;
#elif _WIN32
  auto err = WSAGetLastError();
  return err == WSAETIMEDOUT;
#endif
}
