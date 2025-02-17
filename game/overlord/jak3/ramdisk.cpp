#include "ramdisk.h"

#include "common/util/Assert.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/rpc_interface.h"
#include "game/sce/iop.h"

namespace jak3 {

constexpr int kRamdiskBufSize = 512;
u8 gRamdiskRpcBuf[kRamdiskBufSize];

void jak3_overlord_init_globals_ramdisk() {}

/*!
 * RPC Handler for "load to ee" rpc.
 */
void* RPC_LoadToEE(unsigned int fno, void* msg_ptr, int) {
  switch (fno) {
    case LoadToEEFno::LOAD_FILE: {
      RpcLoadToEEMsg* msg = (RpcLoadToEEMsg*)(msg_ptr);
      auto f = FindISOFile(msg->name);
      ASSERT(f);
      LoadISOFileToEE(f, msg->addr, msg->length);
    } break;
    default:
      ASSERT_NOT_REACHED();
  }
  return nullptr;
}

u32 LoadToEE_RPC_Thread() {
  using namespace iop;

  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RpcId::LoadToEE, RPC_LoadToEE, gRamdiskRpcBuf, kRamdiskBufSize, nullptr,
                    nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}
}  // namespace jak3