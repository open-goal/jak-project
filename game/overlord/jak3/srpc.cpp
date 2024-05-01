#include "srpc.h"

#include "common/log/log.h"

#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

constexpr int SRPC_MESSAGE_SIZE = 0x50;
constexpr int SRPC_MESSAGE_COUNT = 128;

static u8 s_anSRPC_PlayerBuf[SRPC_MESSAGE_SIZE * SRPC_MESSAGE_COUNT];
static u8 s_anSRPC_LoaderBuf[SRPC_MESSAGE_SIZE];

static void* RPC_Player(u32 fno, void* data, int size) {
  lg::error("RPC_PLAYER UNIMPLEMENTED");
  return nullptr;
}

static void* RPC_Loader(u32 fno, void* data, int size) {
  lg::error("RPC_LOADER UNIMPLEMENTED");
  return nullptr;
}

u32 Thread_Player() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab0, RPC_Player, s_anSRPC_PlayerBuf, sizeof(s_anSRPC_PlayerBuf),
                    nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

u32 Thread_Loader() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab1, RPC_Loader, s_anSRPC_LoaderBuf, sizeof(s_anSRPC_LoaderBuf),
                    nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

}  // namespace jak3
