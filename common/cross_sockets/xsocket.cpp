#ifdef __linux
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <unistd.h>
#elif _WIN32
#define WIN32_LEAN_AND_MEAN
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif

void close_socket(int sock) {
  if (sock < 0) {
    return;
  }
#ifdef __linux
  close(sock);
#elif _WIN32
  closesocket(sock);
#endif
}

int set_socket_option(int socket, int level, int optname, int optval, int optlen) {
#ifdef __linux
  return setsockopt(socket, level, optname, &optval, optlen);
#elif _WIN32
  const char optVal = optval;
  return setsockopt(socket, level, optname, &optVal, optlen);
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