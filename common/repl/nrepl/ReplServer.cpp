// clang-format off
#include "ReplServer.h"

#include "common/cross_sockets/XSocket.h"
#include "common/versions/versions.h"

#include "third-party/fmt/core.h"

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif
#include "common/log/log.h"
// clang-format on

// TODO - basically REPL to listen and inject commands into a running REPL
// - we will need a C++ side client as well which will let us communicate with the repl via for
// example, ImgUI
//
// TODO - The server also needs to eventually return the result of the evaluation

ReplServer::~ReplServer() {
  // Close all our client sockets!
  for (const int& sock : client_sockets) {
    close_socket(sock);
  }
}

void ReplServer::post_init() {
  // Add the listening socket to our set of sockets
  lg::info("[nREPL:{}:{}] awaiting connections", tcp_port, listening_socket);
}

void ReplServer::ping_response(int socket) {
  std::string ping = fmt::format("Connected to OpenGOAL v{}.{} nREPL!",
                                 versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  auto resp = write_to_socket(socket, ping.c_str(), ping.size());
  if (resp == -1) {
    lg::warn("[nREPL:{}] Client Disconnected: {}", tcp_port, address_to_string(addr),
             ntohs(addr.sin_port), socket);
    close_socket(socket);
    client_sockets.erase(socket);
  }
}

std::optional<std::string> ReplServer::get_msg() {
  // Clear the sockets we are listening on
  FD_ZERO(&read_sockets);

  // Add the server's main listening socket (where we accept clients from)
  FD_SET(listening_socket, &read_sockets);

  int max_sd = listening_socket;
  for (const int& sock : client_sockets) {
    if (sock > max_sd) {
      max_sd = sock;
    }
    if (sock > 0) {
      FD_SET(sock, &read_sockets);
    }
  }

  // Wait for activity on _something_, with a timeout so we don't get stuck here on exit.
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 100000;
  auto activity = select(max_sd + 1, &read_sockets, NULL, NULL, &timeout);

  if (activity < 0) {  // TODO - || error!
    return std::nullopt;
  }

  // If something happened on the master socket - it's a new connection
  if (FD_ISSET(listening_socket, &read_sockets)) {
    socklen_t addr_len = sizeof(addr);
    auto new_socket = accept_socket(listening_socket, (sockaddr*)&addr, &addr_len);
    if (new_socket < 0) {
      // TODO - handle error
    } else {
      lg::info("[nREPL:{}]: New socket connection: {}:{}:{}", tcp_port, address_to_string(addr),
               ntohs(addr.sin_port), new_socket);

      // Say hello
      ping_response(new_socket);
      // Track the new socket
      if ((int)client_sockets.size() < max_clients) {
        client_sockets.insert(new_socket);
      } else {
        // TODO - Respond with NO
      }
    }
  }

  // otherwise (and no matter what) check all the clients to see if they have sent us anything
  // else its some IO operation on some other socket
  //
  // RACE - the first client wins

  // TODO - there are ways to do this with iterators but, couldn't figure it out!
  std::vector<int> sockets_to_scan(client_sockets.begin(), client_sockets.end());
  for (const int& sock : sockets_to_scan) {
    if (FD_ISSET(sock, &read_sockets)) {
      // Attempt to read a header
      // TODO - should this be in a loop?
      auto req_bytes = read_from_socket(sock, header_buffer.data(), header_buffer.size());
      if (req_bytes == 0) {
        // Socket disconnected
        // TODO - add a queue of messages in the REPL::Wrapper so we can print _BEFORE_ the prompt
        // is output
        lg::warn("[nREPL:{}] Client Disconnected: {}", tcp_port, address_to_string(addr),
                 ntohs(addr.sin_port), sock);

        // Cleanup the socket and remove it from our set
        close_socket(sock);
        client_sockets.erase(sock);
      } else {
        // Otherwise, process the message
        auto* header = (ReplServerHeader*)(header_buffer.data());
        // get the body of the message
        int expected_size = header->length;
        int got = 0;
        int tries = 0;
        while (got < expected_size) {
          tries++;
          if (tries > 100) {
            break;
          }
          if (got + expected_size > (int)buffer.size()) {
            lg::error(
                "[nREPL:{}]: Bad message, aborting the read.  Got :{}, Expected: {}, Buffer "
                "Size: {}",
                tcp_port, got, expected_size, buffer.size());
            return std::nullopt;
          }
          auto x = read_from_socket(sock, buffer.data() + got, expected_size - got);
          if (want_exit_callback()) {
            return std::nullopt;
          }
          got += x > 0 ? x : 0;
        }

        switch (header->type) {
          case ReplServerMessageType::PING:
            ping_response(sock);
            return std::nullopt;
          case ReplServerMessageType::EVAL:
            std::string msg(buffer.data(), header->length);
            lg::debug("[nREPL:{}] Received Message: {}", tcp_port, msg);
            return std::make_optional(msg);
        }
      }
    }
  }
  return std::nullopt;
}
