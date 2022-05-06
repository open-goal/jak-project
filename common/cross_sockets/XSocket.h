#pragma once

/*!
 * @file XSocket.h
 * Cross platform socket library used for the listener.
 */

#ifdef __linux
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#elif _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#endif

#ifdef __linux
const int TCP_SOCKET_LEVEL = SOL_TCP;
#elif _WIN32
const int TCP_SOCKET_LEVEL = IPPROTO_TCP;
#endif

int open_socket(int af, int type, int protocol);
#ifdef __linux
int accept_socket(int socket, sockaddr* addr, socklen_t* addrLen);
#elif _WIN32
int accept_socket(int socket, sockaddr* addr, int* addrLen);
#endif
void close_socket(int sock);
int set_socket_option(int socket, int level, int optname, const void* optval, int optlen);
int set_socket_timeout(int socket, long microSeconds);
int write_to_socket(int socket, const char* buf, int len);
int read_from_socket(int socket, char* buf, int len);
bool socket_timed_out();
