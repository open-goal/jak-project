#include "ReplServer.h"

#include "common/cross_sockets/XSocket.h"

#include "third-party/fmt/core.h"
#include <common/versions.h>

// TODO - basically REPL to listen and inject commands into a running REPL
// - we will need a C++ side client as well which will let us communicate with the repl via for
// example, ImgUI
//
// TODO - The server also needs to eventually return the result of the evaluation

void ReplServer::write_on_accept() {
  ping_response();
}

std::optional<std::string> ReplServer::read_data() {
  if (accepted_socket == -1) {
    return std::nullopt;
  }
  int got = 0;

  lock();

  while (got < header_buffer.size()) {
    if (got > header_buffer.size()) {
      fmt::print(stderr, "[nREPL]: Bad header, aborting the read.  Got :{}, Expected Size: {}", got,
                 header_buffer.size());
      unlock();
      return std::nullopt;
    }
    auto x =
        read_from_socket(accepted_socket, header_buffer.data() + got, header_buffer.size() - got);
    if (want_exit_callback()) {
      unlock();
      return std::nullopt;
    }
    if (x == 0 || x == -1) {
      accepted_socket = -1;
      unlock();
      return std::nullopt;
    }
    got += x > 0 ? x : 0;
  }

  auto* header = (ReplServerHeader*)(header_buffer.data());

  // get the body of the message
  int expected_size = header->length;
  got = 0;
  while (got < expected_size) {
    if (got + expected_size > buffer.size()) {
      fmt::print(stderr,
                 "[nREPL]: Bad message, aborting the read.  Got :{}, Expected: {}, Buffer Size: {}",
                 got, expected_size, buffer.size());
      unlock();
      return std::nullopt;
    }
    auto x = read_from_socket(accepted_socket, buffer.data() + got, expected_size - got);
    if (want_exit_callback()) {
      unlock();
      return std::nullopt;
    }
    if (x == 0 || x == -1) {
      accepted_socket = -1;
      unlock();
      return std::nullopt;
    }
    got += x > 0 ? x : 0;
  }

  switch (header->type) {
    case ReplServerMessageType::PING:
      ping_response();
      unlock();
      return std::nullopt;
    case ReplServerMessageType::EVAL:
      std::string msg(buffer.data(), header->length);
      unlock();
      return std::make_optional(msg);
  }

  unlock();
  return std::nullopt;
}

void ReplServer::ping_response() {
  std::string ping = fmt::format("Connected to OpenGOAL v{}.{} nREPL!",
                                 versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  lock();
  fmt::print("Accept Socket in ReplServer: {}\n", this->accepted_socket);
  write_to_socket(this->accepted_socket, ping.c_str(), ping.size());
  unlock();
}
