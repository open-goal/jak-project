#pragma once

#include <optional>
#include <set>

#include "common/cross_sockets/XSocketServer.h"

enum ReplServerMessageType { PING = 0, EVAL = 10, SHUTDOWN = 20 };

struct ReplServerHeader {
  u32 length;
  u32 type;
};

class ReplServer : public XSocketServer {
 public:
  using XSocketServer::XSocketServer;
  virtual ~ReplServer();

  void post_init() override;

  std::optional<std::string> get_msg();

 private:
  int max_clients = 50;
  std::vector<char> header_buffer = std::vector<char>((int)sizeof(ReplServerHeader));
  fd_set read_sockets;
  std::set<int> client_sockets = {};

  void error_response(int socket, const std::string& error);
  void ping_response(int socket);
};
