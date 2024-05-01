#include "common/common_types.h"
#include "common/log/log.h"

#include "game/common/str_rpc_types.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

static RPC_Str_Cmd_Jak2 sRPCBuf[1];
static RPC_Str_Cmd_Jak2 sRPCBuf2[4];

static void* RPC_STR(unsigned int /*fno*/, void* _cmd, int /*y*/);
static void* RPC_PLAY(unsigned int /*fno*/, void* _cmd, int size);

u32 STRThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab4, RPC_STR, sRPCBuf, sizeof(sRPCBuf), nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

u32 PLAYThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab5, RPC_PLAY, sRPCBuf2, sizeof(sRPCBuf2), nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

static void* RPC_STR(unsigned int /*fno*/, void* _cmd, int /*y*/) {
  lg::error("RPC_STR UNIMPLEMENTED");
  return nullptr;
}

static void* RPC_PLAY(unsigned int /*fno*/, void* _cmd, int size) {
  lg::error("RPC_PLAY UNIMPLEMENTED");
  return nullptr;
}

}  // namespace jak3
