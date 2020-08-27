/*!
 * @file Deci2Server.h
 * Basic implementation of a DECI2 server.
 * Works with deci2.cpp (sceDeci2) to implement the networking on target
 */

#ifndef JAK1_DECI2SERVER_H
#define JAK1_DECI2SERVER_H

#include <netinet/in.h>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <functional>
#include "game/system/deci_common.h"

class Deci2Server {
 public:
  static constexpr int BUFFER_SIZE = 32 * 1024 * 1024;
  Deci2Server(std::function<bool()> shutdown_callback);
  ~Deci2Server();
  bool init();
  bool check_for_listener();
  void send_data(void* buf, u16 len);

  void lock();
  void unlock();
  void wait_for_protos_ready();
  void send_proto_ready(Deci2Driver* drivers, int* driver_count);

  void run();

 private:
  void accept_thread_func();
  bool kill_accept_thread = false;
  char* buffer = nullptr;
  int server_fd;
  sockaddr_in addr;
  int new_sock;
  bool server_initialized = false;
  bool accept_thread_running = false;
  bool server_connected = false;
  std::function<bool()> want_exit;
  std::thread accept_thread;

  std::condition_variable cv;
  bool protocols_ready = false;
  std::mutex deci_mutex;
  Deci2Driver* d2_drivers = nullptr;
  int* d2_driver_count = nullptr;
};

#endif  // JAK1_DECI2SERVER_H
