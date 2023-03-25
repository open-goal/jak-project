#pragma once

/*!
 * @file deci2.h
 * Implementation of SCE DECI2 library.
 */

#include "common/listener_common.h"

class Deci2Server;

namespace ee {

void LIBRARY_INIT_sceDeci2();
void LIBRARY_sceDeci2_run_sends();
void LIBRARY_sceDeci2_register(::Deci2Server* server);

s32 sceDeci2Open(u16 protocol, void* opt, void (*handler)(s32 event, s32 param, void* opt));
s32 sceDeci2Close(s32 s);
s32 sceDeci2ReqSend(s32 s, char dest);
s32 sceDeci2ExRecv(s32 s, void* buf, u16 len);
s32 sceDeci2ExSend(s32 s, void* buf, u16 len);
void sceDeci2Disable();

}  // namespace ee
