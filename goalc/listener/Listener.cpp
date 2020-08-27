/*!
 * @file Listener.cpp
 * The Listener can connect to a Deci2Server for debugging.
 

#include <stdexcept>
#include <Winsock2.h>
#include <io.h>
#include <cassert>
#include "Listener.h"
#include "common/versions.h"

using namespace versions;

namespace listener {
Listener::Listener() {
  m_buffer = new char[BUFFER_SIZE];
}

Listener::~Listener() {
  // shut down the receive thread if needed
  disconnect();

  delete[] m_buffer;
  if (socket_fd >= 0) {
    close(socket_fd);
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


/*
bool Listener::connect_to_target(const std::string& ip, int port) {
  if (m_connected) {
    throw std::exception("attempted a Listener::connect_to_target when already connected!");
  }

  if (socket_fd >= 0) {
    close(socket_fd);
  }

  // construct socket
  socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_fd < 0) {
    printf("[Listener] Failed to create socket.\n");
    socket_fd = -1;
    return false;
  }

  // set timeout for receive
  timeval timeout = {};
  timeout.tv_sec = 0;
  timeout.tv_usec = 100000;
  if (setsockopt(socket_fd, SOL_SOCKET, SO_RCVTIMEO, (char*)&timeout, sizeof(timeout)) < 0) {
    printf("[Listener] setsockopt failed\n");
    close(socket_fd);
    socket_fd = -1;
    return false;
  }

  // set nodelay, which makes small rapid messages faster, but large messages slower
  const char one = 1;
  if (setsockopt(socket_fd, SOL_SOCKET, TCP_NODELAY, &one, sizeof(one))) {
    printf("[Listener] failed to TCP_NODELAY\n");
    close(socket_fd);
    socket_fd = -1;
    return false;
  }

  // setup address
  sockaddr_in server_address = {};
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(port);
  if (inet_pton(AF_INET, ip.c_str(), &server_address.sin_addr) <= 0) {
    printf("[Listener] Invalid IP address.\n");
    close(socket_fd);
    socket_fd = -1;
    return false;
  }

  // connect!
  int rv = connect(socket_fd, (sockaddr*)&server_address, sizeof(server_address));
  if (rv < 0) {
    printf("[Listener] Failed to connect\n");
    close(socket_fd);
    socket_fd = -1;
    return false;
  }

  // get the GOAL version number, to make sure we connected to the right thing
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
  if (!ok) {
    printf("[Listener] Failed to get version number\n");
    close(socket_fd);
    socket_fd = -1;
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
    close(socket_fd);
    socket_fd = -1;
    return false;
  }
}

/*!
 * Runs in a separate thread to receive messages.
 * Will print messages to stdout, or optionally save them.




/*!
 * Attempt to connect to the target. If the target isn't running, this should fail quickly.
 * Returns true if successfully connected.
 

void Listener::receive_func() {
  while (m_connected) {
    // attempt to receive a ListenerMessageHeader
    int rcvd = 0;
    int rcvd_desired = sizeof(ListenerMessageHeader);
    char buff[sizeof(ListenerMessageHeader)];
    while (rcvd < rcvd_desired) {
      auto got = read(socket_fd, buff + rcvd, rcvd_desired - rcvd);
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
            int got = read(socket_fd, ack_recv_buff + ack_recv_prog, hdr->deci2_header.len - rcvd);
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

          int got = read(socket_fd, str_buff + msg_prog, hdr->deci2_header.len - rcvd);
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

}  // namespace listener

*/
