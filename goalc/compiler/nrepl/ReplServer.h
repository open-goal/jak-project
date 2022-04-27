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
  void read_data() override;
  void send_data(void* buf, u16 len) override;

  void set_compiler(std::shared_ptr<Compiler> _compiler);

 private:
  std::shared_ptr<Compiler> compiler = nullptr;
  char* header_buffer = new char[(int)sizeof(ReplServerHeader)];

  void ping_response();
  void compile_msg(const std::string_view& msg);
};
