/*!
 * @file Listener.cpp
 * The Listener can connect to a Deci2Server for debugging.
 */

// clang-format off
#ifdef __linux__
#include <stdexcept>

#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <unistd.h>
#elif _WIN32
#define WIN32_LEAN_AND_MEAN
#include <WinSock2.h>
#include <WS2tcpip.h>

// remove the evil windows min/max macros!
#undef min
#undef max
#endif

#include <algorithm>
#include <chrono>
#include <cstring>
#include <stdexcept>
#include <thread>

#include "common/cross_sockets/XSocket.h"
#include "common/util/Assert.h"
#include "common/versions/versions.h"
#include "common/log/log.h"

#include "Listener.h"

#include "third-party/fmt/core.h"
// clang-format on

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

/*!
 * Disconnect if we are connected and shut down the receiving thread.
 */
void Listener::disconnect() {
  if (m_debugger && m_debugger->is_halted()) {
    printf(
        "[Listener] The listener was shut down while the debugger has paused the runtime, "
        "resuming\n");
    m_debugger->detach();
  }

  if (m_debugger) {
    m_debugger->invalidate();
  }
  m_connected = false;
  if (receive_thread_running) {
    rcv_thread.join();
    receive_thread_running = false;
  }
}

/*!
 * Are we currently connected? Returns false if we are currently disconnecting.
 */
bool Listener::is_connected() const {
  return m_connected;
}

/*!
 * Attempt to connect to the target. If the target isn't running, this should fail quickly.
 * Returns true if successfully connected.
 */
bool Listener::connect_to_target(int n_tries, const std::string& ip, int port) {
  if (port == -1) {
    port = m_default_port;
  }

  if (m_connected) {
    printf("already connected!\n");
    return true;
  }

  disconnect();

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

  if (set_socket_timeout(listen_socket, 500000) < 0) {
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
    printf("[Listener] Socket connected established! (took %d tries). Waiting for version...\n", i);
  }

  // get the GOAL version number, to make sure we connected to the right thing
  int32_t version_buffer[2] = {-1, -1};
  int read_tries = 0;
  int prog = 0;
  bool ok = true;
  while (prog < 8) {
    auto r = read_from_socket(listen_socket, (char*)version_buffer + prog, 8 - prog);
    std::this_thread::sleep_for(std::chrono::microseconds(100000));
    prog += r > 0 ? r : 0;
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
    m_connected = true;
    rcv_thread = std::thread(&Listener::receive_func, this);
    receive_thread_running = true;
    return true;
  } else {
    printf(", expected %d.%d. Cannot connect.\n", GOAL_VERSION_MAJOR, GOAL_VERSION_MINOR);
    close_socket(listen_socket);
    listen_socket = -1;
    return false;
  }

  last_recvd_id = 0;
  last_sent_id = 0;
}

/*!
 * Runs in a separate thread to receive messages.
 * Will print messages to stdout, or optionally save them.
 */
void Listener::receive_func() {
  while (m_connected) {
    // attempt to receive a ListenerMessageHeader
    u32 rcvd = 0;
    u32 rcvd_desired = sizeof(ListenerMessageHeader);
    char buff[sizeof(ListenerMessageHeader)];
    while (rcvd < rcvd_desired) {
      auto got = read_from_socket(listen_socket, buff + rcvd, rcvd_desired - rcvd);
      rcvd += got > 0 ? got : 0;

      // kick us out if we got a bogus read result
      if (got == 0 || (got == -1 && !socket_timed_out())) {
        m_connected = false;
      }

      // exit this loop if we don't want to be running any more
      if (!m_connected) {
        return;
      }
    }

    ListenerMessageHeader* hdr = (ListenerMessageHeader*)buff;
    if (debug_listener) {
      printf("[T -> L] received %d bytes, kind %d\n", hdr->deci2_header.len, int(hdr->msg_kind));
    }

    switch (hdr->msg_kind) {
      case ListenerMessageKind::MSG_ACK:
        // an "ack" message, sent by the target to indicate it got something.
        if (!waiting_for_ack) {
          if (hdr->msg_id == last_sent_id) {
            printf("[Listener] Received ACK for most recent message late.\n");
            if (last_recvd_id != hdr->msg_id - 1) {
              lg::print(
                  "[Listener] WARNING: message ID jumped from {} to {}. Some messages may have "
                  "been lost. You must wait for an ACK before sending the next message.\n",
                  last_recvd_id, hdr->msg_id);
            }
          } else {
            printf("[Listener] Got an unexpcted ACK message.");
          }
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
          ASSERT(ack_recv_prog < 512);
          got_ack = true;
          last_recvd_id = hdr->msg_id;
          if (last_recvd_id > last_sent_id) {
            lg::print(
                "[Listener] ERROR: Got an ack message with id of {}, but the last message sent "
                "had an ID of {}.\n",
                last_recvd_id, last_sent_id);
          }
        } else {
          printf("[Listener] got invalid ack!\n");
        }
        break;

      case ListenerMessageKind::MSG_OUTPUT:
      case ListenerMessageKind::MSG_PRINT: {
        auto* str_buff = new char[hdr->msg_size + 1];  // plus one for the null terminator
        int msg_prog = 0;
        ASSERT(hdr->msg_id == 0);
        while (rcvd < hdr->deci2_header.len) {
          if (!m_connected) {
            return;
          }

          int got =
              read_from_socket(listen_socket, str_buff + msg_prog, hdr->deci2_header.len - rcvd);
          got = got > 0 ? got : 0;
          rcvd += got;
          msg_prog += got;
          if (got == 0 || (got == -1 && !socket_timed_out())) {
            m_connected = false;
          }
        }
        str_buff[hdr->msg_size] = '\0';

        if (hdr->msg_kind == ListenerMessageKind::MSG_PRINT) {
          printf("%s\n", str_buff);
        }

        rcv_mtx.lock();
        if (hdr->msg_kind == filter) {
          message_record.emplace_back(str_buff);
        }

        if (hdr->msg_kind == ListenerMessageKind::MSG_OUTPUT) {
          handle_output_message(str_buff);
        }
        rcv_mtx.unlock();
        delete[] str_buff;
      } break;

      default:
        printf("unhandled message type %d from target\n", int(hdr->msg_kind));
        break;
    }
  }
}

/*!
 * Start recording messages of the given kind.
 */
void Listener::record_messages(ListenerMessageKind kind) {
  if (filter != ListenerMessageKind::MSG_INVALID) {
    printf("[Listener] Already recording!\n");
  }
  filter = kind;
}

/*!
 * Stop recording messages and return a list of messages.
 */
std::vector<std::string> Listener::stop_recording_messages() {
  rcv_mtx.lock();
  filter = ListenerMessageKind::MSG_INVALID;
  auto result = message_record;
  message_record.clear();
  rcv_mtx.unlock();
  return result;
}

/*!
 * Get the number of messages recorded so far.
 */
int Listener::get_received_message_count() {
  rcv_mtx.lock();
  auto result = message_record.size();
  rcv_mtx.unlock();
  return result;
}

/*!
 * Send a "CODE" message for the target to execute as the Listener Function.
 * Returns once the target acks the code.
 *
 * The load name is not actually sent to the target.  Instead, if the target loads successfully
 * and outputs a *listener* load message, this will be remapped to a load of the given name.
 */
void Listener::send_code(std::vector<uint8_t>& code, const std::optional<std::string>& load_name) {
  got_ack = false;
  int total_size = code.size() + sizeof(ListenerMessageHeader);
  if (total_size > BUFFER_SIZE) {
    printf("[ERROR] Listener send_code got too big of a message\n");
    return;
  }

  rcv_mtx.lock();
  m_pending_listener_load_object_name = load_name;
  rcv_mtx.unlock();

  auto* header = (ListenerMessageHeader*)m_buffer;
  auto* buffer_data = (char*)(header + 1);
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = total_size;
  header->deci2_header.proto = DECI2_PROTOCOL;
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = code.size();
  header->ltt_msg_kind = LTT_MSG_CODE;
  header->u6 = 0;
  last_sent_id++;
  header->msg_id = last_sent_id;
  memcpy(buffer_data, code.data(), code.size());
  send_buffer(total_size);
}

/*!
 * Send a message to tell the target to reset. The shutdown parameter tells the target to shutdown.
 * Waits for the target to ack the shutdown message.
 */
void Listener::send_reset(bool shutdown) {
  if (!m_connected) {
    printf("Not connected, so cannot reset target.\n");
    return;
  }

  if (m_debugger && m_debugger->is_halted()) {
    printf("Tried to reset a halted target, detaching...\n");
    m_debugger->detach();
  }

  auto* header = (ListenerMessageHeader*)m_buffer;
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = sizeof(ListenerMessageHeader);
  header->deci2_header.proto = DECI2_PROTOCOL;
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = 0;
  header->ltt_msg_kind = shutdown ? LTT_MSG_SHUTDOWN : LTT_MSG_RESET;
  header->u6 = 0;
  last_sent_id++;
  header->msg_id = last_sent_id;
  send_buffer(sizeof(ListenerMessageHeader));
  disconnect();
  close_socket(listen_socket);
  printf("[Listener] Closed connection to target\n");
}

/*!
 * Send a "poke" message.
 * This makes the target think its connect and flush any buffers that are pending.
 */
void Listener::send_poke() {
  if (!m_connected) {
    printf("Not connected, so cannot poke target.\n");
    return;
  }
  auto* header = (ListenerMessageHeader*)m_buffer;
  header->deci2_header.rsvd = 0;
  header->deci2_header.len = sizeof(ListenerMessageHeader);
  header->deci2_header.proto = DECI2_PROTOCOL;
  header->deci2_header.src = 'H';
  header->deci2_header.dst = 'E';
  header->msg_size = 0;
  header->ltt_msg_kind = LTT_MSG_POKE;
  header->u6 = 0;
  last_sent_id++;
  header->msg_id = last_sent_id;
  send_buffer(sizeof(ListenerMessageHeader));
}

/*!
 * Low level send of the m_buffer.
 * Waits for the target to respond or times out and prints an error.
 */
void Listener::send_buffer(int sz) {
  int wrote = 0;

  if (debug_listener) {
    fprintf(stderr, "[L -> T] sending %d bytes...\n", sz);
  }

  got_ack = false;
  waiting_for_ack = true;
  while (wrote < sz) {
    auto to_send = std::min(512, sz - wrote);
    auto x = write_to_socket(listen_socket, m_buffer + wrote, to_send);
    wrote += x > 0 ? x : 0;
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
    printf("  Timed out waiting for ack.\n");
  }
}

/*!
 * Wait for the target to send an ack.
 */
bool Listener::wait_for_ack() {
  // todo, check the message ID.
  if (!m_connected) {
    printf("wait_for_ack called when not connected!\n");
    return false;
  }

  for (int i = 0; i < 2000; i++) {
    if (got_ack)
      return true;
    std::this_thread::sleep_for(std::chrono::microseconds(1000));
  }

  waiting_for_ack = false;
  return false;
}

/*!
 * Handle an output message from the runtime.
 * This is used to update the memory map and get initial information for the debugger.
 */
void Listener::handle_output_message(const char* msg) {
  std::string all(msg);

  std::string::size_type last_line = 0, line = all.find('\n');

  while (line != std::string::npos) {
    auto str = all.substr(last_line, line - last_line);
    last_line = line + 1;
    line = all.find('\n', last_line);

    auto x = str.find(' ');
    auto kind = str.substr(0, x);
    if (kind == "reset") {
      ASSERT(x + 1 < str.length());
      auto next = str.find(' ', x + 1);
      auto s7_str = str.substr(x, next - x);
      x = next;

      ASSERT(x + 1 < str.length());
      next = str.find(' ', x + 1);
      auto base_str = str.substr(x, next - x);
      x = next;

      ASSERT(x + 1 < str.length());
      next = str.find(' ', x + 1);
      auto tid_str = str.substr(x, next - x);

      if (m_debugger) {
        m_debugger->set_context(std::stoul(s7_str.substr(3), nullptr, 16),
                                std::stoull(base_str.substr(3), nullptr, 16), tid_str.substr(1));
        printf("[Debugger] Context: %s\n", m_debugger->get_context_string().c_str());
      }

    } else if (kind == "load") {
      ASSERT(x + 1 < str.length());
      auto next = str.find(' ', x + 1);
      auto name_str = str.substr(x, next - x);
      x = next;

      ASSERT(x + 1 < str.length());
      next = str.find(' ', x + 1);
      auto load_kind_str = str.substr(x, next - x);
      x = next;

      std::string seg_strings[6];

      for (auto& seg_string : seg_strings) {
        ASSERT(x + 1 < str.length());
        next = str.find(' ', x + 1);
        seg_string = str.substr(x, next - x);
        x = next;
      }

      LoadEntry entry;
      entry.load_string = load_kind_str.substr(1);
      for (int i = 0; i < 3; i++) {
        entry.segments[i] = std::stoul(seg_strings[i].substr(3), nullptr, 16);
      }

      for (int i = 0; i < 3; i++) {
        entry.segment_sizes[i] = std::stoul(seg_strings[i + 3].substr(3), nullptr, 16);
      }

      add_load(name_str.substr(2, name_str.length() - 3), entry);
      // lg::print("LOAD:\n{}", entry.print());
    } else if (kind == "sql-query") {
      lg::info("SQL Query - '{}'", msg);
    } else {
      // todo unload
      printf("[Listener Warning] unknown output message \"%s\"\n", msg);
    }
  }
}

/*!
 * Add a load to the load listing.
 */
void Listener::add_load(const std::string& name, const LoadEntry& le) {
  // if we load a file through the listener, the compiler will set the pending load name,
  // and the runtime will send a load message with *listener*.
  if (name == "*listener*" && m_pending_listener_load_object_name) {
    m_load_entries[*m_pending_listener_load_object_name] = le;
    m_pending_listener_load_object_name = {};
  } else {
    // if we load over an existing thing, kick it out.
    for (auto it = m_load_entries.begin(); it != m_load_entries.end();) {
      if (it->second.overlaps_with(le)) {
        it = m_load_entries.erase(it);
      } else {
        it++;
      }
    }
    m_load_entries[name] = le;
  }
}

/*!
 * Add a debugger that the listener should inform.
 */
void Listener::add_debugger(Debugger* debugger) {
  m_debugger = debugger;
}

MemoryMap Listener::build_memory_map() {
  return MemoryMap(m_load_entries);
}

}  // namespace listener
