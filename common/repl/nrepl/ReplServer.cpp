// clang-format off
#include "ReplServer.h"

#include "common/cross_sockets/XSocket.h"
#include "common/versions/versions.h"

#include "fmt/core.h"

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif
#include "common/log/log.h"
// clang-format on

// TODO - The server also needs to eventually return the result of the evaluation

ReplServer::~ReplServer() {
  // Close all our client sockets!
  for (const int& sock : client_sockets) {
    close_socket(sock);
  }
}

void ReplServer::post_init() {
  // Add the listening socket to our set of sockets
  lg::debug("[nREPL:{}:{}] awaiting connections", tcp_port, listening_socket);
}

void ReplServer::error_response(int socket, const std::string& error) {
  std::string msg = fmt::format("[ERROR]: {}", error);
  auto resp = write_to_socket(socket, msg.c_str(), msg.size());
  if (resp == -1) {
    lg::warn("[nREPL:{}] Client Disconnected: {}", tcp_port, address_to_string(addr),
             ntohs(addr.sin_port), socket);
    close_socket(socket);
    client_sockets.erase(socket);
  }
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
  struct timeval timeout = {0, 100000};
  auto activity = select(max_sd + 1, &read_sockets, NULL, NULL, &timeout);
  if (activity < 0 && errno != EINTR) {
    lg::error("[nREPL:{}] select error, returned: {}, errno: {}", tcp_port, activity,
              strerror(errno));
    return std::nullopt;
  }

  // If something happened on the master socket - it's a new connection
  if (FD_ISSET(listening_socket, &read_sockets)) {
    socklen_t addr_len = sizeof(addr);
    auto new_socket = accept_socket(listening_socket, (sockaddr*)&addr, &addr_len);
    if (new_socket < 0) {
      if (new_socket != -1) {
        lg::error("[nREPL:{}] accept error, returned: {}, errono: {}", tcp_port, new_socket,
                  strerror(errno));
      }
    } else {
      lg::info("[nREPL:{}]: New socket connection: {}:{}:{}", tcp_port, address_to_string(addr),
               ntohs(addr.sin_port), new_socket);
      // Say hello
      ping_response(new_socket);
      // Track the new socket
      if ((int)client_sockets.size() < max_clients) {
        client_sockets.insert(new_socket);
      } else {
        // Respond with NO and close the socket
        lg::warn("[nREPL:{}]: Maximum clients reached. Rejecting connection.", tcp_port);
        error_response(new_socket, "Maximum clients reached. Rejecting connection.");
        close_socket(new_socket);
      }
    }
  }

  // Check all clients for activity
  for (auto it = client_sockets.begin(); it != client_sockets.end();) {
    int sock = *it;
    if (FD_ISSET(sock, &read_sockets)) {
      // Attempt to read a header
      auto req_bytes = read_from_socket(sock, header_buffer.data(), header_buffer.size());
      if (req_bytes <= 0) {
        // TODO - add a queue of messages in the REPL::Wrapper so we can print _BEFORE_ the prompt
        // is output
        if (req_bytes == 0) {
          lg::warn("[nREPL:{}] Client Disconnected: {}", tcp_port, address_to_string(addr));
        } else {
          lg::warn("[nREPL:{}] Error reading from socket on {}: {}", tcp_port,
                   address_to_string(addr), strerror(errno));
        }
        // Cleanup the socket and remove it from our set
        close_socket(sock);
        it = client_sockets.erase(it);  // Erase and move to the next element
        continue;
      } else {
        // Otherwise, process the message
        auto* header = (ReplServerHeader*)(header_buffer.data());
        // get the body of the message
        int expected_size = header->length;
        int got = 0;
        int tries = 0;
        bool skip_to_next_socket = false;
        while (got < expected_size) {
          if (want_exit_callback()) {
            lg::warn("[nREPL:{}] Terminating nREPL early", tcp_port);
            return std::nullopt;
          }
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
          auto bytes_read = read_from_socket(sock, buffer.data() + got, expected_size - got);
          if (bytes_read <= 0) {
            if (bytes_read == 0) {
              lg::warn("[nREPL:{}] Client Disconnected: {}", tcp_port, address_to_string(addr));
            } else {
              lg::warn("[nREPL:{}] Error reading from socket on {}: {}", tcp_port,
                       address_to_string(addr), strerror(errno));
            }
            close_socket(sock);
            it = client_sockets.erase(it);  // Erase and move to the next element
            skip_to_next_socket = true;
            break;
          }
          got += bytes_read;
          std::this_thread::sleep_for(std::chrono::milliseconds(1));
        }

        if (skip_to_next_socket) {
          continue;
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
    ++it;
  }
  return std::nullopt;
}
