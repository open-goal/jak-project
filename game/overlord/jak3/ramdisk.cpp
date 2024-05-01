#include "ramdisk.h"

#include "common/log/log.h"

#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

static u8 gRamDisk_RPCBUF[40];

static void* RPC_Ramdisk(u32 fno, void* data, int size) {
  lg::error("RPC_RAMDISK UNIMPLEMENTED");
  return nullptr;
}

void InitRamdisk() {}

u32 Thread_Server() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab2, RPC_Ramdisk, gRamDisk_RPCBUF, sizeof(gRamDisk_RPCBUF), nullptr,
                    nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

}  // namespace jak3
