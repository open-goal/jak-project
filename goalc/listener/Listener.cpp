/*!
 * @file Listener.cpp
 * The Listener can connect to a Deci2Server for debugging.
 *
 * TODO - msg ID?
 */

#ifdef __linux__
#include <stdexcept>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#elif _WIN32
#define WIN32_LEAN_AND_MEAN
#include <WinSock2.h>
#include <WS2tcpip.h>
#endif

// TODO - i think im not including the dependency right..?
#include "common/cross_sockets/xsocket.h"

#include <stdexcept>
#include <cassert>
#include <cstring>
#include <chrono>
#include <thread>
#include "Listener.h"
#include "common/versions.h"

using namespace versions;
constexpr bool debug_listener = false;

namespace listener {
Listener::Listener() {
  m_buffer = new char[BUFFER_SIZE];
}

Listener::~Listener() {
  // shut down the receive thread if needed
  disconnect();

  delete[] m_buffer;
  if (listen_socket >= 0) {
    close_socket(listen_socket);
  }
}

void Listener::disconnect() {
  m_connected = false;
  if (receive_thread_running) {
    rcv_thread.join();
    receive_thread_running = false;
  }
}

bool Listener::is_connected() const {
  return m_connected;
}

/*!
 * Attempt to connect to the target. If the target isn't running, this should fail quickly.
 * Returns true if successfully connected.
 */
bool Listener::connect_to_target(int n_tries, const std::string& ip, int port) {
  if (m_connected) {
    printf("already connected!\n");
    return true;
  }

  if (listen_socket >= 0) {
    close_socket(listen_socket);
  }

  // construct socket
  listen_socket = open_socket(AF_INET, SOCK_STREAM, 0);
  if (listen_socket < 0) {
    printf("[Listener] Failed to create socket.\n");
    listen_socket = -1;
    return false;
  }

  if (set_socket_timeout(listen_socket, 100000) < 0) {
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }

  // set nodelay, which makes small rapid messages faster, but large messages slower
  int one = 1;
  if (set_socket_option(listen_socket, TCP_SOCKET_LEVEL, TCP_NODELAY, &one, sizeof(one))) {
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }

  // setup address
  sockaddr_in server_address = {};
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(port);
  if (inet_pton(AF_INET, ip.c_str(), &server_address.sin_addr) <= 0) {
    printf("[Listener] Invalid IP address.\n");
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }

  // connect!
  int rv, i;
  for (i = 0; i < n_tries; i++) {
    rv = connect(listen_socket, (sockaddr*)&server_address, sizeof(server_address));
    if (rv >= 0) {
      break;
    }
    std::this_thread::sleep_for(std::chrono::microseconds(100000));
  }
  if (rv < 0) {
    printf("[Listener] Failed to connect\n");
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  } else {
    printf("[Listener] Socket connected established! (took %d tries)\n", i);
  }

  // get the GOAL version number, to make sure we connected to the right thing
  int32_t version_buffer[2] = {-1, -1};
  int read_tries = 0;
  int prog = 0;
  bool ok = true;
  while (prog < 8) {
    auto r = read_from_socket(listen_socket, (char*)version_buffer + prog, 8 - prog);
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
  if (!ok) {
    printf("[Listener] Failed to get version number\n");
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }

  printf("Got version %d.%d", version_buffer[0], version_buffer[1]);
  if (version_buffer[0] == GOAL_VERSION_MAJOR && version_buffer[1] == GOAL_VERSION_MINOR) {
    printf(" OK!\n");
    rcv_thread = std::thread(&Listener::receive_func, this);
    m_connected = true;
    receive_thread_running = true;
    return true;
  } else {
    printf(", expected %d.%d. Cannot connect.\n", GOAL_VERSION_MAJOR, GOAL_VERSION_MINOR);
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }
}

/*!
 * Runs in a separate thread to receive messages.
 * Will print messages to stdout, or optionally save them.
 */
void Listener::receive_func() {
  while (m_connected) {
    // attempt to receive a ListenerMessageHeader
    int rcvd = 0;
    int rcvd_desired = sizeof(ListenerMessageHeader);
    char buff[sizeof(ListenerMessageHeader)];
    while (rcvd < rcvd_desired) {
      auto got = read_from_socket(listen_socket, buff + rcvd, rcvd_desired - rcvd);
      rcvd += got > 0 ? got : 0;

      // kick us out if we got a bogus read result
      if (got == 0 || (got == -1 && errno != EAGAIN)) {
        m_connected = false;
      }

      // exit this loop if we don't want to be running any more
      if (!m_connected) {
        return;
      }
    }

    ListenerMessageHeader* hdr = (ListenerMessageHeader*)buff;
    //    if(debug_listener) {
    //      printf("[T -> L] received %d bytes, kind %d\n",
    //             hdr->deci2_hdr.len, hdr->msg_kind);
    //    }

    switch (hdr->msg_kind) {
      case ListenerMessageKind::MSG_ACK:
        // an "ack" message, sent by the target to indicate it got something.
        if (!waiting_for_ack) {
          printf("[Listener] Got an ack message when we weren't expecting one.");
        }

        if (hdr->deci2_header.len < 512) {
          // ack's should be < 512 bytes (they are just "ack").
          int ack_recv_prog = 0;
          while (rcvd < hdr->deci2_header.len) {
            if (!m_connected)
              return;
            int got = read_from_socket(listen_socket, ack_recv_buff + ack_recv_prog,
                                       hdr->deci2_header.len - rcvd);
            got = got > 0 ? got : 0;
            rcvd += got;
            ack_recv_prog += got;
          }
          ack_recv_buff[ack_recv_prog] = '\0';
          assert(ack_recv_prog < 512);
          got_ack = true;
        } else {
          printf("[Listener] got invalid ack!\n");
        }
        break;

      case ListenerMessageKind::MSG_OUTPUT:
      case ListenerMessageKind::MSG_PRINT: {
        auto* str_buff = new char[hdr->msg_size + 1];  // plus one for the null terminator
        int msg_prog = 0;
        while (rcvd < hdr->deci2_header.len) {
          if (!m_connected) {
            return;
          }

          int got =
              read_from_socket(listen_socket, str_buff + msg_prog, hdr->deci2_header.len - rcvd);
          got = got > 0 ? got : 0;
          rcvd += got;
          msg_prog += got;
          if (got == 0 || (got == -1 && errno != EAGAIN)) {
            m_connected = false;
          }
        }
        str_buff[hdr->msg_size] = '\0';

        if (hdr->msg_kind == ListenerMessageKind::MSG_PRINT) {
          printf("%s\n", str_buff);
        } else {
          printf("[OUTPUT] %s\n", str_buff);
        }

        rcv_mtx.lock();
        if (hdr->msg_kind == filter) {
          message_record.emplace_back(str_buff);
        }
        rcv_mtx.unlock();

      } break;

      default:
        printf("unhandled message type %d from target\n", int(hdr->msg_kind));
        break;
    }
  }
}

void Listener::record_messages(ListenerMessageKind kind) {
  if (filter != ListenerMessageKind::MSG_INVALID) {
    printf("[Listener] Already recording!\n");
  }
  filter = kind;
}

std::vector<std::string> Listener::stop_recording_messages() {
  filter = ListenerMessageKind::MSG_INVALID;
  auto result = message_record;
  message_record.clear();
  return result;
}

void Listener::send_code(std::vector<uint8_t>& code) {
  got_ack = false;
  int total_size = code.size() + sizeof(ListenerMessageHeader);
  if (total_size > BUFFER_SIZE) {
    printf("[ERROR] Listener send_code got too big of a message\n");
    return;
  }

  auto* header = (ListenerMessageHeader*)m_buffer;
  auto* buffer_data = (char*)(header + 1);
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = total_size;
  header->deci2_header.proto = 0xe042;  // todo don't hardcode
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = code.size();
  header->ltt_msg_kind = LTT_MSG_CODE;
  header->u6 = 0;
  header->u8 = 0;
  memcpy(buffer_data, code.data(), code.size());
  send_buffer(total_size);
}

void Listener::send_reset(bool shutdown) {
  if (!m_connected) {
    printf("Not connected, so cannot reset target.\n");
    return;
  }
  auto* header = (ListenerMessageHeader*)m_buffer;
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = sizeof(ListenerMessageHeader);
  header->deci2_header.proto = 0xe042;  // todo don't hardcode
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = 0;
  header->ltt_msg_kind = LTT_MSG_RESET;
  header->u6 = 0;
  header->u8 = shutdown ? UINT64_MAX : 0;
  send_buffer(sizeof(ListenerMessageHeader));
  disconnect();
  close_socket(listen_socket);
  printf("closed connection to target\n");
}

void Listener::send_poke() {
  if (!m_connected) {
    printf("Not connected, so cannot poke target.\n");
    return;
  }
  auto* header = (ListenerMessageHeader*)m_buffer;
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = sizeof(ListenerMessageHeader);
  header->deci2_header.proto = 0xe042;  // todo don't hardcode
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = 0;
  header->ltt_msg_kind = LTT_MSG_POKE;
  header->u6 = 0;
  header->u8 = 0;
  send_buffer(sizeof(ListenerMessageHeader));
}

void Listener::send_buffer(int sz) {
  int wrote = 0;

  if (debug_listener) {
    printf("[L -> T] sending %d bytes...\n", sz);
  }

  got_ack = false;
  waiting_for_ack = true;
  while (wrote < sz) {
    auto to_send = std::min(512, sz - wrote);
    auto x = write_to_socket(listen_socket, m_buffer + wrote, to_send);
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

bool Listener::wait_for_ack() {
  if (!m_connected) {
    printf("wait_for_ack called when not connected!\n");
    return false;
  }

  for (int i = 0; i < 2000; i++) {
    if (got_ack)
      return true;
    usleep(1000);
  }

  waiting_for_ack = false;
  return false;
}

}  // namespace listener
