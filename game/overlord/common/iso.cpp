#include "iso.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/common/fake_iso.h"
#include "game/overlord/common/iso_api.h"
#include "game/overlord/jak1/dma.h"
#include "game/overlord/jak1/iso.h"
#include "game/overlord/jak1/ssound.h"
#include "game/overlord/jak1/stream.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/stream.h"
#include "game/common/dgo_rpc_types.h"
#include "game/sce/iop.h"
#include "game/runtime.h"

using namespace iop;

u32 DGOThread();

IsoFs* isofs;
u32 iso_init_flag;
s32 sync_mbx;
s32 iso_mbx;
s32 dgo_mbx;
s32 iso_thread;
s32 dgo_thread;
s32 str_thread;
s32 play_thread;
VagDir gVagDir;
static RPC_Dgo_Cmd sRPCBuff[1];
DgoCommand sLoadDGO;  // renamed from scmd to sLoadDGO in Jak 2

void iso_init_globals() {
  isofs = nullptr;
  iso_init_flag = 0;
  sync_mbx = 0;
  iso_mbx = 0;
  dgo_mbx = 0;
  iso_thread = 0;
  dgo_thread = 0;
  str_thread = 0;
  play_thread = 0;
  memset(&gVagDir, 0, sizeof(gVagDir));
  memset(sRPCBuff, 0, sizeof(sRPCBuff));
  memset(&sLoadDGO, 0, sizeof(DgoCommand));
}

static constexpr s32 LOOP_END = 1;
static constexpr s32 LOOP_REPEAT = 2;
static constexpr s32 LOOP_START = 4;

// Empty ADPCM block with loop flags
// clang-format off
static u8 VAG_SilentLoop[0x60] = {
    0x0, LOOP_START | LOOP_REPEAT, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_REPEAT,              0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_END | LOOP_REPEAT,   0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
};
// clang-format on

/*!
 * Does the messagebox have a message in it?
 */
u32 LookMbx(s32 mbx) {
  MsgPacket* msg_packet;
  return PollMbx((&msg_packet), mbx) != KE_MBOX_NOMSG;
}

/*!
 * Wait for a messagebox to have a message. This is inefficient and polls with a 100 us wait.
 * This is stupid because the IOP does have much better syncronization primitives so you don't have
 * to do this.
 */
void WaitMbx(s32 mbx) {
  while (!LookMbx(mbx)) {
    DelayThread(100);
  }
}

/*!
 * Find a file by name.  Return nullptr if it fails.
 */
FileRecord* FindISOFile(const char* name) {
  return isofs->find(name);
}

/*!
 * Get the length of an ISO File by FileRecord
 */
u32 GetISOFileLength(FileRecord* f) {
  return isofs->get_length(f);
}

/*!
 * Initialize the ISO FileSystem system.
 * Returns 0 on success.
 */
u32 InitISOFS(const char* fs_mode, const char* loading_screen, GameVersion version) {
  // in retail:
  // isofs = &iso_cd;

  // ADDED
  isofs = &fake_iso;
  (void)fs_mode;  // ignore user's request.
  // always pick fake_iso because the others are not useful.
  // END ADDED

  // mark us as NOT initialized.
  iso_init_flag = 1;

  if (version == GameVersion::Jak1) {
    while (!jak1::DMA_SendToSPUAndSync(&VAG_SilentLoop, 0x30, jak1::gTrapSRAM)) {
      DelayThread(1000);
    }
  }

  // INITIALIZE MESSAGE BOXES
  MbxParam mbx_param;
  mbx_param.attr = 0;
  mbx_param.option = 0;
  iso_mbx = CreateMbx(&mbx_param);
  if (iso_mbx <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create ISO mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  mbx_param.attr = 0;
  mbx_param.option = 0;
  dgo_mbx = CreateMbx(&mbx_param);
  if (dgo_mbx <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create DGO mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  mbx_param.attr = 0;
  mbx_param.option = 0;
  sync_mbx = CreateMbx(&mbx_param);
  if (sync_mbx <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create sync mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  // INITIALIZE THREADS
  ThreadParam thread_param;
  thread_param.attr = TH_C;
  thread_param.initPriority = 100;
  thread_param.stackSize = 0x1000;
  thread_param.option = 0;
  switch (version) {
    case GameVersion::Jak1:
      thread_param.entry = jak1::ISOThread;
      break;
    case GameVersion::Jak2:
      thread_param.entry = jak2::ISOThread;
      break;
    default:
      ASSERT_NOT_REACHED();
  }
  strcpy(thread_param.name, "ISOThread");
  iso_thread = CreateThread(&thread_param);
  if (iso_thread <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create ISO thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 98;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  thread_param.entry = DGOThread;
  strcpy(thread_param.name, "DGOThread");
  dgo_thread = CreateThread(&thread_param);
  if (dgo_thread <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create DGO thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 97;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  switch (version) {
    case GameVersion::Jak1:
      thread_param.entry = jak1::STRThread;
      break;
    case GameVersion::Jak2:
      thread_param.entry = jak2::STRThread;
      break;
    default:
      ASSERT_NOT_REACHED();
  }
  strcpy(thread_param.name, "STRThread");
  str_thread = CreateThread(&thread_param);
  if (str_thread <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create STR thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 97;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  switch (version) {
    case GameVersion::Jak1:
      thread_param.entry = jak1::PLAYThread;
      break;
    case GameVersion::Jak2:
      thread_param.entry = jak2::PLAYThread;
      break;
    default:
      ASSERT_NOT_REACHED();
  }
  strcpy(thread_param.name, "PLAYThread");
  play_thread = CreateThread(&thread_param);
  if (play_thread <= 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : Cannot create PLAY thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  // Start the threads!
  StartThread(iso_thread, 0);
  StartThread(dgo_thread, 0);
  StartThread(str_thread, 0);
  StartThread(play_thread, 0);

  // wait for ISO Thread to initialize
  WaitMbx(sync_mbx);

  // LOAD VAGDIR file
  FileRecord* vagdir_file = FindISOFile("VAGDIR.AYB");
  if (vagdir_file) {
    LoadISOFileToIOP(vagdir_file, &gVagDir, VAG_DIR_FILE_SIZE[version]);
  } else {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : cannot load VAG directory\n");
    printf("IOP: ======================================================================\n");
  }

  constexpr int LOADING_SCREEN_SIZE = 0x800000;
  constexpr u32 LOADING_SCREEN_DEST_ADDR = 0x1000000;
  FileRecord* loading_screen_file = FindISOFile(loading_screen);
  if (loading_screen_file) {
    LoadISOFileToEE(loading_screen_file, LOADING_SCREEN_DEST_ADDR, LOADING_SCREEN_SIZE);
  }

  // should be set by ISOThread to 0 before the WaitMbx(sync_mbx);
  return iso_init_flag;
}

/*!
 * Initialize the ISO Driver.
 * Requires a buffer large enough to hold 3 sector (or 4 if you have DUP files)
 */
static MsgPacket not_on_stack_sync;
void InitDriver(u8* buffer) {
  if (!isofs->init(buffer)) {
    // succesful init!
    iso_init_flag = 0;
  }

  // you idiots, you're giving the kernel a pointer to a stack variable!
  // (this is fixed in Jak 1 Japan and NTSC Greatest Hits)
  // SendMbx(sync_mbx, &msg_packet);

  // whoever fixed that bug felt similarly about it
  SendMbx(sync_mbx, &not_on_stack_sync);
}

/*!
 * Find VAG file by "name", where name is 8 bytes (chars with spaces at the end, treated as two
 * s32's). Returns pointer to name in the VAGDIR file data.
 */
VagDirEntry* FindVAGFile(const char* name) {
  VagDirEntry* entry = gVagDir.vag;
  for (u32 idx = 0; idx < gVagDir.count; idx++) {
    // check if matching name
    if (memcmp(entry->name, name, 8) == 0) {
      return entry;
    }
    entry++;
  }
  return nullptr;
}



void* RPC_DGO(unsigned int fno, void* _cmd, int y);
void LoadDGO(RPC_Dgo_Cmd* cmd);
void LoadNextDGO(RPC_Dgo_Cmd* cmd);
void CancelDGO(RPC_Dgo_Cmd* cmd);

/*!
 * DGO RPC Thread.
 */
u32 DGOThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // setup RPC.
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, DGO_RPC_ID[g_game_version], RPC_DGO, sRPCBuff, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

/*!
 * DGO RPC Handler.
 */
void* RPC_DGO(unsigned int fno, void* _cmd, int y) {
  (void)y;
  auto* cmd = (RPC_Dgo_Cmd*)_cmd;
  // call appropriate handler.
  switch (fno) {
    case DGO_RPC_LOAD_FNO:
      LoadDGO(cmd);
      break;
    case DGO_RPC_LOAD_NEXT_FNO:
      LoadNextDGO(cmd);
      break;
    case DGO_RPC_CANCEL_FNO:
      CancelDGO(cmd);
      break;
    default:
      cmd->result = DGO_RPC_RESULT_ERROR;
  }
  return cmd;
}

/*!
 * Begin loading a DGO.  Returns when the first obj is loaded.
 * Then will load the next obj into the second buffer.
 * Then the DGO loader will block until LoadNextDGO is called.
 * This approach keeps two loads in flight at a time to increase loading throughput.
 * One load will be read from DVD / DMA'd to EE
 * Another will be linked on the EE.
 * The final load is done directly onto the heap, and isn't double buffered
 * (otherwise the linking object could allocate on the heap where the final loading object is
 * being copied).  This avoids having to relocate the data from the temporary load buffer to the
 * heap, and is the only way to make sure that the entire heap can be filled.
 */
void LoadDGO(RPC_Dgo_Cmd* cmd) {
  // Find the file
  FileRecord* fr = isofs->find(cmd->name);
  if (!fr) {
    cmd->result = DGO_RPC_RESULT_ERROR;
    return;
  }

  // cancel an in progress command and wait for it to end.
  // note - this doesn't handle a nullptr correctly, so if this actually ends up cancelling
  // it will crash.
  CancelDGO(nullptr);

  // set up the ISO Command
  sLoadDGO.cmd_id = LOAD_DGO_CMD_ID;
  sLoadDGO.messagebox_to_reply = dgo_mbx;
  sLoadDGO.thread_id = 0;
  sLoadDGO.buffer1 = (u8*)(u64)(cmd->buffer1);
  sLoadDGO.buffer2 = (u8*)(u64)(cmd->buffer2);
  sLoadDGO.buffer_heaptop = (u8*)(u64)(cmd->buffer_heap_top);
  sLoadDGO.fr = fr;

  // printf("LOAD DGO -- 0x%x\n", cmd->buffer1);

  // send the command to ISO Thread
  SendMbx(iso_mbx, &sLoadDGO);

  // wait for the ReturnMessage in the DGO callback state machine.
  // this happens when the first file is loaded
  WaitMbx(dgo_mbx);

  if (sLoadDGO.status == CMD_STATUS_IN_PROGRESS) {
    // we got one, but there's more to load.
    // we don't set cmd->buffer1 as it's already the correct buffer in this case -
    // when there are >1 objs, we load into buffer1 first.
    cmd->result = DGO_RPC_RESULT_MORE;
  } else if (sLoadDGO.status == CMD_STATUS_DONE) {
    // all done! make sure our reply says we loaded to the top.
    cmd->result = DGO_RPC_RESULT_DONE;
    cmd->buffer1 = cmd->buffer_heap_top;
    sLoadDGO.cmd_id = 0;
  } else {
    // error.
    cmd->result = DGO_RPC_RESULT_ERROR;
    sLoadDGO.cmd_id = 0;
  }
}

/*!
 * Signal to the IOP it can keep loading and overwrite the oldest obj buffer.
 * This will return when there's another loaded obj.
 */
void LoadNextDGO(RPC_Dgo_Cmd* cmd) {
  // printf("LOAD NEXT DGO -- 0x%x\n", cmd->buffer1);

  if (sLoadDGO.cmd_id == 0) {
    // something went wrong.
    cmd->result = DGO_RPC_RESULT_ERROR;
  } else {
    // update heap location
    sLoadDGO.buffer_heaptop = (u8*)(u64)cmd->buffer_heap_top;
    if (g_game_version != GameVersion::Jak1) {
      sLoadDGO.buffer1 = (u8*)(u64)cmd->buffer1;
      sLoadDGO.buffer2 = (u8*)(u64)cmd->buffer2;
    }
    // allow DGO state machine to advance
    SendMbx(sync_mbx, nullptr);
    // wait for another load to finish.
    WaitMbx(dgo_mbx);
    // another load finished, respond with the result.
    if (sLoadDGO.status == CMD_STATUS_IN_PROGRESS) {
      // more, use the selected buffer.
      cmd->result = DGO_RPC_RESULT_MORE;
      cmd->buffer1 = (u32)(u64)sLoadDGO.selectedBuffer;
    } else if (sLoadDGO.status == CMD_STATUS_DONE) {
      // last obj, always loaded to top.
      cmd->result = DGO_RPC_RESULT_DONE;
      cmd->buffer1 = cmd->buffer_heap_top;
      sLoadDGO.cmd_id = 0;
    } else {
      cmd->result = DGO_RPC_RESULT_ERROR;
      sLoadDGO.cmd_id = 0;
    }
  }
}

/*!
 * Abort an in progress load.
 */
void CancelDGO(RPC_Dgo_Cmd* cmd) {
  if (sLoadDGO.cmd_id) {
    sLoadDGO.want_abort = 1;
    // wake up DGO state machine with abort
    SendMbx(sync_mbx, nullptr);
    // wait for it to abort.
    WaitMbx(dgo_mbx);
    // this will cause a crash if we cancel because we try to load 2 dgos at the same time.
    // this should succeed if it's an actual cancel because we changed which level we're trying to
    // load.
    // This is weird in the original game, the IOP doesn't crash on writing to 0
    // or, we have some other bug.
    // NOTE : actually got fixed in Jak 2 so who cares
    if (cmd) {
      cmd->result = DGO_RPC_RESULT_ABORTED;
    }

    sLoadDGO.cmd_id = 0;
  }
}