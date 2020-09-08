#ifdef __linux
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <unistd.h>
#elif _WIN32
#include <WinSock2.h>
#endif

#ifdef __linux
const int TCP_SOCKET_LEVEL = SOL_TCP;
#elif _WIN32
const int TCP_SOCKET_LEVEL = IPPROTO_TCP;
#endif

int open_socket(int af, int type, int protocol);
void close_socket(int sock);
int set_socket_option(int socket, int level, int optname, const char* optval, int optlen);
int write_to_socket(int socket, const char* buf, int len);
int read_from_socket(int socket, char* buf, int len);
