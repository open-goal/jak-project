#include "iso.h"

#include <cstring>

#include "common/log/log.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/jak3/basefilesystem.h"
#include "game/overlord/jak3/iso_fake.h"

namespace jak3 {
using namespace iop;

static int s_nISOInitFlag;
static ISO_LoadDGO s_LoadDGO;
int s_nSyncMbx;
CBaseFileSystem* g_pFileSystem;
MsgPacket s_MsgPacket_NotOnStackSync[2];
RPC_Dgo_Cmd s_aISO_RPCBuf[1];

static void* RPC_DGO(u32 fno, void* data, int size) {
  lg::error("RPC_DGO UNIMPLEMENTED");
  return nullptr;
}

static u32 DGOThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, 0xfab3, RPC_DGO, s_aISO_RPCBuf, sizeof(s_aISO_RPCBuf), nullptr, nullptr,
                    &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);

  return 0;
}

/* COMPLETE */
void InitDriver() {
  if (g_pFileSystem->Init() == 0) {
    s_nISOInitFlag = 0;
  }

  SendMbx(s_nSyncMbx, &s_MsgPacket_NotOnStackSync[0]);
}

/* TODO unfinished */
static int LookMbx() {
  MsgPacket* pkt;

  int ret = PollMbx(&pkt, s_nSyncMbx);
  if (ret != KE_MBOX_NOMSG) {
    // FIXME
  }

  return ret != KE_MBOX_NOMSG;
}

/* COMPLETE */
static void WaitMbx(int mbx) {
  MsgPacket* pkt;
  ReceiveMbx(&pkt, mbx);
}

/* TODO unfinished */
int InitISOFS(const char* fs_mode, const char* loading_sceeen) {
  ThreadParam thp;
  MbxParam mbx;

  memset(&s_LoadDGO, 0, sizeof(s_LoadDGO));
  s_nISOInitFlag = 1;
  g_pFileSystem = &g_FakeISOCDFileSystem;

  return s_nISOInitFlag;
}

/* COMPLETE */
const ISOFileDef* FindISOFile(char* name) {
  return g_pFileSystem->Find(name);
}

/* COMPLETE */
s32 GetISOFileLength(const ISOFileDef* fd) {
  return g_pFileSystem->GetLength(fd);
}

}  // namespace jak3
