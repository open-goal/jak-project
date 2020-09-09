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
  printf("setting timeout to %d ms\n", timeout);
  return set_socket_option(socket, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
#endif
}

int write_to_socket(int socket, const char* buf, int len) {
#ifdef __linux
  return write(socket, buf, len);
#elif _WIN32
  return send(socket, buf, len, 0);
#endif
}

int read_from_socket(int socket, char* buf, int len) {
#ifdef __linux
  return read(socket, buf, len);
#elif _WIN32
  return recv(socket, buf, len, 0);
#endif
}
