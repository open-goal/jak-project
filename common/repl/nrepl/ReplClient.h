#pragma once

#include "ReplServer.h"

#include "common/cross_sockets/XSocketClient.h"

class ReplClient : public XSocketClient {
 public:
  using XSocketClient::XSocketClient;
  virtual ~ReplClient() = default;

  ReplClient& operator=(const ReplClient&) { return *this; }

  // TODO - just void for now :(
  void eval(std::string form);
};
