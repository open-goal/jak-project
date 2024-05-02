#include "iso.h"

#include <cstring>

#include "common/log/log.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/jak3/basefilesystem.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_fake.h"
#include "game/overlord/jak3/stream.h"

namespace jak3 {
using namespace iop;

CBaseFileSystem* g_pFileSystem;
int g_nISOThreadID;
int g_nDGOThreadID;
int g_nSTRThreadID;
int g_nPlayThreadID;
int g_nISOMbx;
VagDirJak3 g_VagDir;

static int s_nISOInitFlag;
static ISO_LoadDGO s_LoadDGO;
static int s_nSyncMbx;
static MsgPacket s_MsgPacket_NotOnStackSync[2];
static RPC_Dgo_Cmd s_aISO_RPCBuf[1];
static int s_nDGOMbx;

static u32 DGOThread();
u32 ISOThread();

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
  const ISOFileDef* fd;
  ThreadParam thp;
  MbxParam mbx;

  memset(&s_LoadDGO, 0, sizeof(s_LoadDGO));
  /* TODO INIT s_LoadDgo fields */

  s_nISOInitFlag = 1;
  g_pFileSystem = &g_FakeISOCDFileSystem;

  /* Lets not bother error checking the Create* calls */
  mbx.attr = 0;
  mbx.option = 0;
  g_nISOMbx = CreateMbx(&mbx);

  mbx.attr = 0;
  mbx.option = 0;
  s_nDGOMbx = CreateMbx(&mbx);

  mbx.attr = 0;
  mbx.option = 0;
  s_nSyncMbx = CreateMbx(&mbx);

  thp.attr = TH_C;
  thp.entry = ISOThread;
  thp.initPriority = 0x37;
  thp.option = 0;
  thp.stackSize = 0x1100;
  g_nISOThreadID = CreateThread(&thp);

  thp.attr = TH_C;
  thp.entry = DGOThread;
  thp.initPriority = 0x38;
  thp.option = 0;
  thp.stackSize = 0x900;
  g_nDGOThreadID = CreateThread(&thp);

  thp.attr = TH_C;
  thp.entry = STRThread;
  thp.initPriority = 0x39;
  thp.option = 0;
  thp.stackSize = 0x900;
  g_nSTRThreadID = CreateThread(&thp);

  thp.attr = TH_C;
  thp.entry = PLAYThread;
  thp.initPriority = 0x35;
  thp.option = 0;
  thp.stackSize = 0x900;
  g_nPlayThreadID = CreateThread(&thp);

  StartThread(g_nISOThreadID, 0);
  StartThread(g_nDGOThreadID, 0);
  StartThread(g_nSTRThreadID, 0);
  StartThread(g_nPlayThreadID, 0);

  WaitMbx(s_nSyncMbx);

  /* Originally VAGDIR is only loaded for the CD filesystem
   * Presumably there was different mechanism for fakeiso,
   * but we need it regardless */

  fd = FindIsoFile("VAGDIR.AYB");
  LoadISOFileToIOP(fd, &g_VagDir, sizeof(g_VagDir));

  /* Skip getting the loading screen thing? */

  return s_nISOInitFlag;
}

const ISOFileDef* FindIsoFile(const char* name) {
  return g_pFileSystem->Find(name);
}

u32 ISOThread() {
  InitDriver();

  while (true) {
    DelayThread(4000);
  }

  return 0;
}

static void ISO_LoadDGO(RPC_Dgo_Cmd* msg);
static void ISO_LoadNextDGO(RPC_Dgo_Cmd* msg);
static void ISO_CancelDGO(RPC_Dgo_Cmd* msg);

static void* RPC_DGO(u32 fno, void* data, int size) {
  auto* msg = static_cast<RPC_Dgo_Cmd*>(data);

  switch (fno) {
    case 0:
      ISO_LoadDGO(msg);
      break;
    case 1:
      ISO_LoadNextDGO(msg);
      break;
    case 2:
      ISO_CancelDGO(msg);
      break;
    default:
      msg->result = 1;
      break;
  }
  return data;
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

static void ISO_LoadDGO(RPC_Dgo_Cmd* msg) {
  auto* def = g_pFileSystem->Find(msg->name);
  if (def == nullptr) {
    msg->result = 1;
    return;
  }

  msg->buffer1 = msg->buffer_heap_top;
  msg->result = 0;
}

static void ISO_LoadNextDGO(RPC_Dgo_Cmd* msg) {}

static void ISO_CancelDGO(RPC_Dgo_Cmd* msg) {}

/* COMPLETE */
const ISOFileDef* FindISOFile(char* name) {
  return g_pFileSystem->Find(name);
}

/* COMPLETE */
s32 GetISOFileLength(const ISOFileDef* fd) {
  return g_pFileSystem->GetLength(fd);
}

}  // namespace jak3
