#pragma once

int get_socket_level();
int open_socket(int af, int type, int protocol);
void close_socket(int sock);
int set_socket_option(int socket, int level, int optname, const void* optval, int optlen);
int set_socket_timeout(int socket, long microSeconds);
int write_to_socket(int socket, const char* buf, int len);
int read_from_socket(int socket, char* buf, int len);
bool socket_timed_out();
