/*!
 * @file deci2.cpp
 * Implementation of SCE DECI2 library.
 */

#include <cassert>
#include <cstdio>
#include <cstring>
#include "deci2.h"
#include "game/system/Deci2Server.h"

namespace ee {

namespace {
// TODO-WINDOWS
#ifdef __linux__
constexpr int MAX_DECI2_PROTOCOLS = 4;
Deci2Driver protocols[MAX_DECI2_PROTOCOLS];  // info for each deci2 protocol registered
int protocol_count;                          // number of registered protocols
Deci2Driver* sending_driver;                 // currently sending protocol driver
::Deci2Server* server;                       // the server to send data to
#endif

}  // namespace

/*
 * Initialize the library.
 */
void LIBRARY_INIT_sceDeci2() {
// TODO-WINDOWS
#ifdef __linux__
  // reset protocols
  for (auto& p : protocols) {
    p = Deci2Driver();
  }
  protocol_count = 0;
  server = nullptr;
  sending_driver = nullptr;
#endif
}

/*!
 * Run any pending requested sends.
 */
void LIBRARY_sceDeci2_run_sends() {
// TODO-WINDOWS
#ifdef __linux__
  for (auto& prot : protocols) {
    if (prot.active && prot.pending_send == 'H') {
      sending_driver = &prot;
      (prot.handler)(DECI2_WRITE, 0, prot.opt);
      sending_driver = nullptr;
      prot.pending_send = 0;
      (prot.handler)(DECI2_WRITEDONE, 0, prot.opt);
    }
  }
#endif
}

/*!
 * Register a Deci2Server with this library.
 */
void LIBRARY_sceDeci2_register(::Deci2Server* s) {
// TODO-WINDOWS
#ifdef __linux__
  server = s;
#endif
}

/*!
 * Open a new socket with given protocol number and handler.
 * The "opt" pointer is passed to the handler function.
 * I don't know why it's like this.
 */
s32 sceDeci2Open(u16 protocol, void* opt, void (*handler)(s32 event, s32 param, void* opt)) {
// TODO-WINDOWS
#ifdef __linux__
  server->lock();
  Deci2Driver drv;
  drv.protocol = protocol;
  drv.opt = opt;
  drv.handler = handler;
  drv.id = protocol_count + 1;
  drv.active = true;
  protocols[protocol_count++] = drv;
  printf("[DECI2] Add new protocol driver %d for 0x%x\n", drv.id, drv.protocol);
  server->unlock();

  if (protocol_count == 1) {
    // if we have our first protocol, inform the server we are ready to receive!
    // then the server will accept incoming data.
    server->send_proto_ready(protocols, &protocol_count);
  }

  return drv.id;
#elif _WIN32
  return 0;
#endif
}

/*!
 * Deactivate a DECI2 protocol by socket descriptor.
 */
s32 sceDeci2Close(s32 s) {
// TODO-WINDOWS
#ifdef __linux__
  assert(s - 1 < protocol_count);
  protocols[s - 1].active = false;
#endif
  return 1;
}

/*!
 * Start a send.
 */
s32 sceDeci2ReqSend(s32 s, char dest) {
// TODO-WINDOWS
#ifdef __linux__
  assert(s - 1 < protocol_count);
  auto& proto = protocols[s - 1];
  proto.pending_send = dest;
#endif
  return 0;
}

/*!
 * Do a receive from socket s into buf of size len.
 * Returns after data is copied.
 */
s32 sceDeci2ExRecv(s32 s, void* buf, u16 len) {
// TODO-WINDOWS
#ifdef __linux__
  assert(s - 1 < protocol_count);
  protocols[s - 1].recv_size = len;
  auto avail = protocols[s - 1].available_to_receive;
  if (len <= avail) {
    memcpy(buf, protocols[s - 1].recv_buffer, len);
    return len;
  } else {
    printf("[DECI2] Error: ExRecv %d, only %d available!\n", len, avail);
    return -1;
  }
#elif _WIN32
  return 0;
#endif
}

/*!
 * Do a send.
 */
s32 sceDeci2ExSend(s32 s, void* buf, u16 len) {
// TODO-WINDOWS
#ifdef __linux__
  assert(s - 1 < protocol_count);
  if (!sending_driver) {
    printf("sceDeci2ExSend called at illegal time!\n");
  }

  if (&protocols[s - 1] != sending_driver) {
    printf("sceDeci2ExSend called with the wrong socket!\n");
  }

  server->send_data(buf, len);
  return len;
#elif _WIN32
  return 0;
#endif
}
}  // namespace ee
