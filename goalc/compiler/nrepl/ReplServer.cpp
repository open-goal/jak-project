#include "ReplServer.h"

#include "common/cross_sockets/XSocket.h"

#include "third-party/fmt/core.h"
#include <common/versions.h>

// TODO - basically REPL to listen and inject commands into a running REPL
// - we will need a C++ side client as well which will let us communicate with the repl via for
// example, ImgUI
//
// TODO - The server also needs to eventually return the result of the evaluation

// Known Issues:
// - doesn't handle disconnects/reconnects

void ReplServer::write_on_accept() {
  ping_response();
}

void ReplServer::read_data() {
  int desired_size = 1;
  int got = 0;

  while (got < desired_size) {
    ASSERT(got + desired_size < buffer_size);
    int sock = accepted_socket;
    auto x = read_from_socket(sock, buffer + got, desired_size - got);
    if (want_exit_callback()) {
      return;
    }
    got += x > 0 ? x : 0;
  }

  auto* header = (ReplServerHeader*)(buffer);

  lock();

  // get the body of the message
  desired_size = header->length;
  got = 0;
  while (got < desired_size) {
    ASSERT(got + desired_size < buffer_size);
    auto x = read_from_socket(accepted_socket, buffer + got, desired_size - got);
    if (want_exit_callback()) {
      return;
    }
    got += x > 0 ? x : 0;
  }

  auto* body = (char*)(buffer);

  switch (header->type) {
    case ReplServerMessageType::PING:
      ping_response();
      break;
    case ReplServerMessageType::EVAL:
      compile_msg("(repl-help)");
      break;
  }

  unlock();
}

void ReplServer::send_data(void* buf, u16 len) {
  lock();
  if (client_connected) {
    int bytes_sent = 0;
    while (bytes_sent < len) {
      int wrote = write_to_socket(accepted_socket, (char*)(buf) + bytes_sent, len - bytes_sent);
      bytes_sent += wrote;
      if (!client_connected || want_exit_callback()) {
        unlock();
        return;
      }
    }
  }
  unlock();
}

void ReplServer::set_compiler(std::shared_ptr<Compiler> _compiler) {
  compiler = std::move(_compiler);
}

void ReplServer::ping_response() {
  u32 versions[2] = {versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR};
  char* ye = "sanity";
  lock();
  write_to_socket(accepted_socket, (char*)&ye, 6);
  unlock();
}

void ReplServer::compile_msg(const std::string_view& msg) {
  if (compiler == nullptr) {
    return;
  }
  compiler->lock();
  compiler->read_eval_print(msg.data());
  compiler->unlock();
}
