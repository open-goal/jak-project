#pragma once

#include "common/cross_sockets/XSocketServer.h"

#include "goalc/compiler/Compiler.h"

enum ReplServerMessageType { PING = 0, EVAL = 10, SHUTDOWN = 20 };

struct ReplServerHeader {
  u32 length;
  u32 type;
};

class ReplServer : public XSocketServer {
 public:
  using XSocketServer::XSocketServer;

  void write_on_accept() override;
  std::optional<std::string> read_data();

 private:
  std::vector<char> header_buffer = std::vector<char>((int)sizeof(ReplServerHeader));

  void ping_response();
};
