/*!
 * @file deci2.cpp
 * Implementation of SCE DECI2 library.
 */

#include "deci2.h"

#include <cstdio>
#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/system/Deci2Server.h"

namespace ee {

namespace {
constexpr int MAX_DECI2_PROTOCOLS = 4;
Deci2Driver protocols[MAX_DECI2_PROTOCOLS];  // info for each deci2 protocol registered
int protocol_count;                          // number of registered protocols
Deci2Driver* sending_driver;                 // currently sending protocol driver
::Deci2Server* server;                       // the server to send data to

}  // namespace

/*
 * Initialize the library.
 */
void LIBRARY_INIT_sceDeci2() {
  // reset protocols
  for (auto& p : protocols) {
    p = Deci2Driver();
  }
  protocol_count = 0;
  server = nullptr;
  sending_driver = nullptr;
}

/*!
 * Run any pending requested sends.
 */
void LIBRARY_sceDeci2_run_sends() {
  for (auto& prot : protocols) {
    if (prot.active && prot.pending_send == 'H') {
      sending_driver = &prot;
      (prot.handler)(DECI2_WRITE, 0, prot.opt);
      sending_driver = nullptr;
      prot.pending_send = 0;
      (prot.handler)(DECI2_WRITEDONE, 0, prot.opt);
    }
  }
}

/*!
 * Register a Deci2Server with this library.
 */
void LIBRARY_sceDeci2_register(::Deci2Server* s) {
  server = s;
}

/*!
 * Open a new socket with given protocol number and handler.
 * The "opt" pointer is passed to the handler function.
 * I don't know why it's like this.
 */
s32 sceDeci2Open(u16 protocol, void* opt, void (*handler)(s32 event, s32 param, void* opt)) {
  server->lock();
  Deci2Driver drv;
  drv.protocol = protocol;
  drv.opt = opt;
  drv.handler = handler;
  drv.id = protocol_count + 1;
  drv.active = true;
  protocols[protocol_count++] = drv;
  lg::info("[DECI2] Add new protocol driver {} for 0x{:x}", drv.id, drv.protocol);
  server->unlock();

  if (protocol_count == 1) {
    // if we have our first protocol, inform the server we are ready to receive!
    // then the server will accept incoming data.
    server->send_proto_ready(protocols, &protocol_count);
  }

  return drv.id;
}

/*!
 * Deactivate a DECI2 protocol by socket descriptor.
 */
s32 sceDeci2Close(s32 s) {
  ASSERT(s - 1 < protocol_count);
  protocols[s - 1].active = false;
  return 1;
}

/*!
 * Start a send.
 */
s32 sceDeci2ReqSend(s32 s, char dest) {
  ASSERT(s - 1 < protocol_count);
  auto& proto = protocols[s - 1];
  proto.pending_send = dest;
  return 0;
}

/*!
 * Do a receive from socket s into buf of size len.
 * Returns after data is copied.
 */
s32 sceDeci2ExRecv(s32 s, void* buf, u16 len) {
  ASSERT(s - 1 < protocol_count);
  protocols[s - 1].recv_size = len;
  auto avail = protocols[s - 1].available_to_receive;
  if (len <= avail) {
    memcpy(buf, protocols[s - 1].recv_buffer, len);
    return len;
  } else {
    printf("[DECI2] Error: ExRecv %d, only %d available!\n", len, avail);
    return -1;
  }
}

/*!
 * Do a send.
 */
s32 sceDeci2ExSend(s32 s, void* buf, u16 len) {
  ASSERT(s - 1 < protocol_count);
  if (!sending_driver) {
    printf("sceDeci2ExSend called at illegal time!\n");
  }

  if (&protocols[s - 1] != sending_driver) {
    printf("sceDeci2ExSend called with the wrong socket!\n");
  }

  server->send_data(buf, len);
  return len;
}

void sceDeci2Disable() {
  server->send_shutdown();
}
}  // namespace ee
