#pragma once

/*!
 * @file XSocket.h
 * Cross platform socket library used for the listener.
 */

// clang-format off
#include "common/common_types.h"
#ifdef OS_POSIX
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <unistd.h>
#elif _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#endif
// clang-format on

#ifdef __linux
const int TCP_SOCKET_LEVEL = SOL_TCP;
#elif _WIN32
const int TCP_SOCKET_LEVEL = IPPROTO_TCP;
#elif __APPLE__
const int TCP_SOCKET_LEVEL = IPPROTO_TCP;
#endif

int open_socket(int af, int type, int protocol);
int connect_socket(int socket, sockaddr* addr, int nameLen);
#ifdef OS_POSIX
int accept_socket(int socket, sockaddr* addr, socklen_t* addrLen);
int select_and_accept_socket(int socket, sockaddr* addr, socklen_t* addrLen, int microSeconds);
#elif _WIN32
int accept_socket(int socket, sockaddr* addr, int* addrLen);
int select_and_accept_socket(int socket, sockaddr* addr, int* addrLen, int microSeconds);
#endif
void close_socket(int sock);
int set_socket_option(int socket, int level, int optname, const void* optval, int optlen);
int set_socket_timeout(int socket, long microSeconds);
int write_to_socket(int socket, const char* buf, int len);
int read_from_socket(int socket, char* buf, int len);
bool socket_timed_out();
std::string address_to_string(const sockaddr_in& addr);
