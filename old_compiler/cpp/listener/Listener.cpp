#include <cstdio>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <cassert>
#include <netinet/tcp.h>
#include "Listener.h"

constexpr bool debug_listener = false;

struct DeciHeader {
  uint16_t len;
  uint16_t rsvd;
  uint16_t proto;
  uint8_t src;
  uint8_t dest;
};

struct MessageHeader {
  DeciHeader deci2_hdr;
  uint16_t msg_kind;
  uint16_t u6;
  uint32_t msg_size;
  uint64_t u8;
};

#include "logger/Logger.h"

Listener::Listener() {
  buffer = new char[BUFFER_SIZE];
}

Listener::~Listener() {
  delete[] buffer;
}

void Listener::listen_to_target(std::string ip, int port) {
  if (connected) {
    printf("already connected, doing nothing!\n");
    return;
  }
  printf("Connecting to target...\n");

  int tries = 0;
  int rv = -1;
  socket_fd = -1;
  while (tries < 50) {
    if (tries) {
      usleep(10000);
    }
    if (socket_fd >= 0)
      close(socket_fd);
    tries++;

    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (socket_fd < 0) {
      printf("[Error] Listener failed to create socket.\n");
      continue;
    }

    timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 100000;

    if (setsockopt(socket_fd, SOL_SOCKET, SO_RCVTIMEO, (char*)&timeout, sizeof(timeout)) < 0) {
      printf("[Error] setsockopt failed\n");
      continue;
    }

    int one = 1;
    if (setsockopt(socket_fd, SOL_TCP, TCP_NODELAY, &one, sizeof(one))) {
      printf("[Error] failed to TCP_NODELAY\n");
      continue;
    }

    sockaddr_in server_address;
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);

    if (inet_pton(AF_INET, ip.c_str(), &server_address.sin_addr) <= 0) {
      printf("[Error] Listener was given invalid IP address.\n");
      continue;
    }

    rv = connect(socket_fd, (sockaddr*)&server_address, sizeof(server_address));
    if (rv < 0) {
      continue;
    }

    int32_t version_buffer[2] = {-1, -1};

    int read_tries = 0;
    int prog = 0;
    bool ok = true;
    while (prog < 8) {
      auto r = read(socket_fd, version_buffer + prog, 8 - prog);
      if (r < 0) {
        ok = false;
        break;
      }
      prog += r;
      read_tries++;
      if (read_tries > 50) {
        ok = false;
        break;
      }
    }
    if (!ok)
      continue;

    printf("Got version %d.%d", version_buffer[0], version_buffer[1]);
    if (version_buffer[0] == GOAL_VERSION_MAJOR && version_buffer[1] == GOAL_VERSION_MINOR) {
      printf(" OK!\n");
      set_connected(true);
      return;
    } else {
      printf(", expected %d.%d. Cannot connect.\n", GOAL_VERSION_MAJOR, GOAL_VERSION_MINOR);
      return;
    }
  }

  printf("Failed to connect.\n");
}

void Listener::send_raw_data(const char* data, int size) {
  int total_size = size + sizeof(MessageHeader);
  if (total_size > BUFFER_SIZE) {
    printf("[ERROR] DECI2 send_raw_data got too big of a message!\n");
    return;
  }

  auto* header = (MessageHeader*)buffer;
  auto* buffer_data = (char*)(header + 1);
  header->deci2_hdr.rsvd = 0;
  header->deci2_hdr.len = total_size;
  header->deci2_hdr.proto = 0xe042;  // todo don't hardcode
  header->deci2_hdr.src = 'H';
  header->deci2_hdr.dest = 'E';
  header->msg_size = size;
  header->msg_kind = 0;
  header->u6 = 0;
  header->u8 = 0;
  memcpy(buffer_data, data, size);

  send_buffer(total_size);
}

void Listener::send_code(std::vector<uint8_t>& code) {
  receiver_got_ack = false;
  int total_size = code.size() + sizeof(MessageHeader);
  if (total_size > BUFFER_SIZE) {
    printf("[ERROR] Listener send_code got too big of a message\n");
    return;
  }

  auto* header = (MessageHeader*)buffer;
  auto* buffer_data = (char*)(header + 1);
  header->deci2_hdr.rsvd = 0;
  header->deci2_hdr.len = total_size;
  header->deci2_hdr.proto = 0xe042;  // todo don't hardcode
  header->deci2_hdr.src = 'H';
  header->deci2_hdr.dest = 'E';
  header->msg_size = code.size();
  header->msg_kind = LTT_MSG_CODE;
  header->u6 = 0;
  header->u8 = 0;
  memcpy(buffer_data, code.data(), code.size());
  send_buffer(total_size);
}

void Listener::send_reset() {
  if (!connected) {
    printf("Not connected, so cannot reset target.\n");
    return;
  }
  auto* header = (MessageHeader*)buffer;
  header->deci2_hdr.rsvd = 0;
  header->deci2_hdr.len = sizeof(MessageHeader);
  header->deci2_hdr.proto = 0xe042;  // todo don't hardcode
  header->deci2_hdr.src = 'H';
  header->deci2_hdr.dest = 'E';
  header->msg_size = 0;
  header->msg_kind = LTT_MSG_RESET;
  header->u6 = 0;
  header->u8 = 0;
  send_buffer(sizeof(MessageHeader));
  set_connected(false);
  close(socket_fd);
  printf("closed connection to target\n");
}

void Listener::send_buffer(int sz) {
  int wrote = 0;

  if (debug_listener) {
    printf("[L -> T] sending %d bytes...\n", sz);
  }

  receiver_got_ack = false;
  while (wrote < sz) {
    auto to_send = std::min(512, sz - wrote);
    auto x = write(socket_fd, buffer + wrote, to_send);
    wrote += x;
  }

  if (debug_listener) {
    printf("  waiting for ack...\n");
  }

  if (wait_for_ack()) {
    if (debug_listener) {
      printf("ack buff:\n");
      printf("%s\n", ack_recv_buff);
      printf("  OK\n");
    }
  } else {
    printf("  NG - target has timed out.  If it has died, disconnect with (disconnect-target)\n");
  }
}

void Listener::send_poke() {
  if (!connected) {
    printf("Not connected, so cannot set target status!\n");
    return;
  }
  auto* header = (MessageHeader*)buffer;
  header->deci2_hdr.rsvd = 0;
  header->deci2_hdr.len = sizeof(MessageHeader);
  header->deci2_hdr.proto = 0xe042;  // todo don't hardcode
  header->deci2_hdr.src = 'H';
  header->deci2_hdr.dest = 'E';
  header->msg_size = 0;
  header->msg_kind = LTT_MSG_POKE;
  header->u6 = 0;
  header->u8 = 0;

  send_buffer(sizeof(MessageHeader));
}

void Listener::receive_data() {
  while (connected) {
    int rcvd = 0;
    int rcvd_desired = sizeof(MessageHeader);
    char buff[sizeof(MessageHeader)];
    while (rcvd < rcvd_desired) {
      auto got = read(socket_fd, buff + rcvd, rcvd_desired - rcvd);
      rcvd += got > 0 ? got : 0;

      if (got == 0 || (got == -1 && errno != EAGAIN)) {
        connected = false;
      }

      if (!connected)
        return;
    }

    MessageHeader* hdr = (MessageHeader*)buff;
    if (debug_listener) {
      printf("[T -> L] received %d bytes, kind %d\n", hdr->deci2_hdr.len, hdr->msg_kind);
    }

    switch (hdr->msg_kind) {
      case MSG_ACK:
        if (hdr->deci2_hdr.len < 512) {
          int ack_recv_prog = 0;
          while (rcvd < hdr->deci2_hdr.len) {
            if (!connected)
              return;
            int got = read(socket_fd, ack_recv_buff + ack_recv_prog, hdr->deci2_hdr.len - rcvd);
            got = got > 0 ? got : 0;
            rcvd += got;
            ack_recv_prog += got;
          }
          ack_recv_buff[ack_recv_prog] = '\0';
          assert(ack_recv_prog < 512);
          receiver_got_ack = true;
        } else {
          printf("got invalid ack!\n");
        }
        break;

      case MSG_OUTPUT:
      case MSG_PRINT: {
        auto* str_buff = new char[hdr->msg_size + 1];
        int msg_prog = 0;
        while (rcvd < hdr->deci2_hdr.len) {
          if (!connected)
            return;
          int got = read(socket_fd, str_buff + msg_prog, hdr->deci2_hdr.len - rcvd);
          got = got > 0 ? got : 0;
          rcvd += got;
          msg_prog += got;
        }
        str_buff[hdr->msg_size] = '\0';

        if (hdr->msg_kind == MSG_PRINT) {
          rcv_mtx.lock();
          pending_messages.emplace_back(str_buff);
          gLogger.log(MSG_TGT, "%s\n", pending_messages.back().c_str());
          rcv_mtx.unlock();
        } else {
          gLogger.log(MSG_TGT_INFO, "NOTE: %s\n", str_buff);
        }

      } break;

      default:
        printf("unhandled message type %d from target\n", hdr->msg_kind);
        break;
    }
  }
}

void Listener::clear_pending_incoming() {
  rcv_mtx.lock();
  pending_messages.clear();
  rcv_mtx.unlock();
}

std::string Listener::pop_pending() {
  std::string result;
  rcv_mtx.lock();
  if (!pending_messages.empty()) {
    result = pending_messages.back();
    pending_messages.pop_back();
  }
  rcv_mtx.unlock();
  return result;
}

bool Listener::has_pending() {
  rcv_mtx.lock();
  bool r = !pending_messages.empty();
  rcv_mtx.unlock();
  return r;
}

void Listener::set_connected(bool con) {
  if (con) {
    connected = true;
    receiver_got_ack = false;
    if (thread_running) {
      rcv_thread.join();
      thread_running = false;
    }
    rcv_thread = std::thread(&Listener::receive_data, this);
    thread_running = true;
  } else {
    connected = false;
    if (thread_running)
      rcv_thread.join();
    thread_running = false;
  }
}

bool Listener::wait_for_ack() {
  if (!connected) {
    printf("Can't wait for ack if we aren't connected!\n");
  }

  for (int i = 0; i < 2000; i++) {
    if (receiver_got_ack)
      return true;
    usleep(1000);
  }

  return false;
}