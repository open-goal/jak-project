#include "iso.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/jak3/basefilesystem.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_fake.h"
#include "game/overlord/jak3/iso_queue.h"
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
char g_szCurrentMusicName[16];
char g_szTargetMusicName[16];

static int s_nISOInitFlag;
static ISO_LoadDGO s_LoadDGO;
static int s_nSyncMbx;
static MsgPacket s_MsgPacket_NotOnStackSync[2];
static RPC_Dgo_Cmd s_aISO_RPCBuf[1];
static int s_nDGOMbx;

static u32 DGOThread();
u32 ISOThread();
int NullCallback(ISO_Hdr* msg);
int CopyDataToEE(ISO_Hdr*);
int CopyDataToIOP(ISO_Hdr*);
int CopyDataTo989snd(ISO_Hdr*);
int CopyData(ISO_LoadSingle*, int);

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
  ISO_Msg* msg;

  g_szCurrentMusicName[0] = '\0';
  g_szTargetMusicName[0] = '\0';
  InitBuffers();
  // InitVagCmds();

  InitDriver();

  while (true) {
    int res = PollMbx((MsgPacket**)&msg, g_nISOMbx);
    if (res == KE_OK) {
      msg->SetActive(false);
      msg->SetUnk1(false);
      msg->SetUnk2(false);
      msg->file = nullptr;
      msg->callback = NullCallback;

      /* LoadSingle messages */
      if (msg->msg_kind >= LOAD_TO_EE_CMD_ID && msg->msg_kind <= LOAD_TO_989SND) {
        auto* ls = static_cast<ISO_LoadSingle*>(msg);
        int pri = 0;
        if (ls->unk40 == 2) {
          pri = 2;
        }
        if (ls->unk40 == 10) {
          pri = 4;
        }
        if (!QueueMessage(msg, pri)) {
          break;
        }

        msg->file = nullptr;
        if (msg->msg_kind == LOAD_TO_EE_OFFSET_CMD_ID) {
          msg->file = g_pFileSystem->Open(ls->fd, ls->offset, EFileComp::MODE1);
        } else if (msg->msg_kind == LOAD_TO_989SND) {
          char name[16] = {0};
          if (ls->filename) {
            strncpy(name, ls->filename, 12);
            name[8] = 0;
            strcat(name, ".sbk");
            auto fd = g_pFileSystem->Find(name);
            if (fd) {
              msg->file = g_pFileSystem->Open(ls->fd, -1, EFileComp::MODE1);
            }
          }
        } else {
          msg->file = g_pFileSystem->Open(ls->fd, -1, EFileComp::MODE1);
        }

        if (!msg->file) {
          msg->m_nStatus = 8;
          UnqueueMessage(msg);
          ReturnMessage(msg);
          break;
        }

        ls->unk44 = ls->address;
        ls->unk48 = 0;
        ls->length_to_copy = g_pFileSystem->GetLength(ls->fd);
        if (msg->msg_kind == LOAD_TO_989SND) {
          ls->length = ls->length_to_copy;
        } else {
          if (!ls->length_to_copy || ls->length < ls->length_to_copy) {
            ls->length_to_copy = ls->length;
          }
        }

        switch (msg->msg_kind) {
          case LOAD_TO_EE_CMD_ID:
            msg->callback = CopyDataToEE;
            break;
          case LOAD_TO_IOP_CMD_ID:
            msg->callback = CopyDataToIOP;
            break;
          case LOAD_TO_EE_OFFSET_CMD_ID:
            msg->callback = CopyDataToEE;
            break;
          case LOAD_TO_989SND:
            msg->callback = CopyDataTo989snd;
            break;
          default:
            ASSERT_NOT_REACHED();
        }

        msg->m_nStatus = 2;
        msg->SetActive(true);
      } else if (msg->msg_kind == 0xADEADBEE) {
        ReturnMessage(msg);
        ExitThread();
      }
    }

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

  ASSERT_NOT_REACHED();

  msg->buffer1 = msg->buffer_heap_top;
  msg->result = 0;
}

static void ISO_LoadNextDGO(RPC_Dgo_Cmd* msg) {
  ASSERT_NOT_REACHED();
}

static void ISO_CancelDGO(RPC_Dgo_Cmd* msg) {
  ASSERT_NOT_REACHED();
}

/* COMPLETE */
const ISOFileDef* FindISOFile(char* name) {
  return g_pFileSystem->Find(name);
}

/* COMPLETE */
s32 GetISOFileLength(const ISOFileDef* fd) {
  return g_pFileSystem->GetLength(fd);
}

int CopyDataToEE(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, 0);
}

int CopyDataToIOP(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, 1);
}

int CopyDataTo989snd(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, 2);
}

int CopyData(ISO_LoadSingle*, int) {
  ASSERT_NOT_REACHED();
  return 0;
}

int NullCallback(ISO_Hdr* msg) {
  return 7;
}

void ISO_Hdr::SetActive(bool b) {
  m_bActive = b;
}

void ISO_Hdr::SetUnk1(bool b) {
  unk1 = b;
}

void ISO_Hdr::SetUnk2(bool b) {
  unk2 = b;
}

void ISOBuffer::AdjustDataLength(int size) {
  int state;

  CpuSuspendIntr(&state);
  decompressed_size -= size;
  CpuResumeIntr(state);
}

void ISOBuffer::AdvanceCurrentData(int size) {
  int state;

  CpuSuspendIntr(&state);
  decomp_buffer += size;
  decompressed_size -= size;
  CpuResumeIntr(state);
}

}  // namespace jak3
