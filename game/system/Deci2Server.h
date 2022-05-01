#pragma once

#include "common/cross_sockets/XSocketServer.h"

#include "deci_common.h"
#include <condition_variable>

/// @brief Basic implementation of a DECI2 server.
/// Works with deci2.cpp(sceDeci2) to implement the networking on target
class Deci2Server : public XSocketServer {
 public:
  using XSocketServer::XSocketServer;

  void write_on_accept() override;
  void read_data();
  void send_data(void* buf, u16 len);

  void wait_for_protos_ready();
  void send_proto_ready(Deci2Driver* drivers, int* driver_count);

 private:
  bool protocols_ready = false;
  std::condition_variable cv;
  Deci2Driver* d2_drivers = nullptr;
  int* d2_driver_count = nullptr;
};
