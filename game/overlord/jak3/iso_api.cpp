#include "iso_api.h"

#include "common/common_types.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_structs.h"

namespace jak3 {
using namespace iop;

u32 LoadISOFileToIOP(const ISOFileDef* file, void* addr, u32 length) {
  ISO_LoadSingle msg;

  msg.msg_kind = 0x101;
  msg.mbx_to_reply = 0;
  msg.thread_id = GetThreadId();
  msg.fd = file;
  msg.address = addr;
  msg.length = length;
  SendMbx(g_nISOMbx, &msg);
  SleepThread();

  u32 ret = 0;
  if (msg.m_nStatus == 0) {
    ret = msg.length_to_copy;
  }

  return ret;
}

}  // namespace jak3
