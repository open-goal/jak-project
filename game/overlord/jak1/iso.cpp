/*!
 * @file iso.cpp
 * CD/DVD Reading.
 * This is a huge mess
 */

#include "iso.h"

#include <cstdio>
#include <cstring>

#include "iso_api.h"
#include "iso_queue.h"
#include "stream.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/common/dma.h"
#include "game/overlord/common/fake_iso.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/jak1/dma.h"
#include "game/overlord/jak1/fake_iso.h"
#include "game/overlord/jak1/srpc.h"
#include "game/runtime.h"
#include "game/sce/iop.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak1 {
u32 ISOThread();
u32 DGOThread();
u32 RunDGOStateMachine(IsoMessage* _cmd, IsoBufferHeader* buffer_header);
u32 CopyDataToEE(IsoMessage* _cmd, IsoBufferHeader* buffer_header);
u32 CopyDataToIOP(IsoMessage* _cmd, IsoBufferHeader* buffer_header);
u32 NullCallback(IsoMessage* _cmd, IsoBufferHeader* buffer_header);

static void InitVAGCmd(VagCommand* cmd, u32 x);
static u32 ProcessVAGData(IsoMessage* _cmd, IsoBufferHeader* buffer_header);
static s32 CheckVAGStreamProgress(VagCommand* vag);
static void StopVAG(VagCommand* vag);
static void PauseVAG(VagCommand* vag);
static void UnpauseVAG(VagCommand* vag);
static s32 GetPlayPos();
static void UpdatePlayPos();
static void VAG_MarkLoopEnd(void* data, u32 size);

u32 gPlayPos;

static VagCommand vag_cmd;
VagCommand* gVAGCMD = nullptr;
VagDir gVagDir;
s32 gDialogVolume = 0;
s32 gFakeVAGClockPaused = 0;
s32 gFakeVAGClockRunning = 0;
s32 gFakeVAGClock = 0;
s32 gRealVAGClockRunning = 0;
s32 gRealVAGClock = 0;
s32 gRealVAGClockS = 0;
s32 gPlaying = 0;
s32 gSampleRate = 0;
bool gLastVagHalf = false;
s32 gVoice;
IsoFs* isofs;
u32 iso_init_flag;
s32 sync_mbx;
s32 iso_mbx;
s32 dgo_mbx;
s32 iso_thread;
s32 dgo_thread;
s32 str_thread;
s32 play_thread;
static RPC_Dgo_Cmd sRPCBuff[1];
DgoCommand sLoadDGO;  // renamed from scmd to sLoadDGO in Jak 2

void iso_init_globals() {
  gPlayPos = 0;
  isofs = nullptr;
  iso_init_flag = 0;
  sync_mbx = 0;
  iso_mbx = 0;
  dgo_mbx = 0;
  iso_thread = 0;
  dgo_thread = 0;
  str_thread = 0;
  play_thread = 0;
  memset(sRPCBuff, 0, sizeof(sRPCBuff));
  memset(&sLoadDGO, 0, sizeof(DgoCommand));
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
u32 InitISOFS(const char* fs_mode, const char* loading_screen) {
  // in retail:
  // isofs = &iso_cd;

  // ADDED
  isofs = &fake_iso;
  (void)fs_mode;  // ignore user's request.
  // always pick fake_iso because the others are not useful.
  // END ADDED

  // mark us as NOT initialized.
  iso_init_flag = 1;

  while (!jak1::DMA_SendToSPUAndSync(&VAG_SilentLoop, 0x30, jak1::gTrapSRAM)) {
    DelayThread(1000);
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
  thread_param.entry = jak1::ISOThread;
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
  thread_param.entry = jak1::STRThread;
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
  thread_param.entry = jak1::PLAYThread;
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
    LoadISOFileToIOP(vagdir_file, &gVagDir, sizeof(gVagDir));
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
void InitDriver() {
  if (!isofs->init()) {
    // succesful init!
    iso_init_flag = 0;
  }

  // you idiots, you're giving the kernel a pointer to a stack variable!
  // (this is fixed in Jak 1 Japan and NTSC Greatest Hits)
  // SendMbx(sync_mbx, &msg_packet);

  // whoever fixed that bug felt similarly about it
  SendMbx(sync_mbx, &not_on_stack_sync);
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

/*!
 * The CD/DVD Reading Thread. This is a mess.
 */
u32 ISOThread() {
  // Initialize!
  InitBuffers();
  auto temp_buffer = AllocateBuffer(BUFFER_PAGE_SIZE);
  InitDriver();  // unblocks InitISOFS's WaitMbx
  FreeBuffer(temp_buffer);

  VagCommand* in_progress_vag_command = nullptr;
  s32 vag_paused = 0;
  s32 unk = 0;

  // main CD/DVD read loop
  for (;;) {
    /////////////////////////////////////
    // Receive Messages and Add to Queue
    /////////////////////////////////////

    // receive a message
    IsoMessage* msg_from_mbx;
    s32 mbx_status = PollMbx((MsgPacket**)(&msg_from_mbx), iso_mbx);

    if (mbx_status == KE_OK) {
      // we got a new message!
      // initialize fields of the message
      msg_from_mbx->callback_buffer = nullptr;
      msg_from_mbx->ready_for_data = 1;
      msg_from_mbx->callback_function = NullCallback;
      msg_from_mbx->fd = nullptr;

      switch (msg_from_mbx->cmd_id) {
        case LOAD_TO_EE_CMD_ID:
        case LOAD_TO_IOP_CMD_ID:
        case LOAD_TO_EE_OFFSET_CMD_ID: {
          // A Simple File Load, add it to the queue
          if (!QueueMessage(msg_from_mbx, 2, "LoadSingle")) {
            break;
          }

          auto* load_single_cmd = (IsoCommandLoadSingle*)msg_from_mbx;

          // if queued successfully, start by opening the file:
          if (load_single_cmd->cmd_id == LOAD_TO_EE_OFFSET_CMD_ID) {
            load_single_cmd->fd =
                isofs->open(load_single_cmd->file_record, load_single_cmd->offset);
          } else {
            // open takes -1 as "no offset", same as 0.
            load_single_cmd->fd = isofs->open(load_single_cmd->file_record, -1);
          }

          // Check to see if it opened correctly:
          if (!load_single_cmd->fd) {
            // nope, set the status to indicate we failed
            load_single_cmd->status = CMD_STATUS_FAILED_TO_OPEN;
            // remove us from the queue...
            UnqueueMessage(load_single_cmd);
            // and wake up whoever requested this.
            ReturnMessage(load_single_cmd);
            break;
          }

          // yep, opened correctly. Set up the pointers/sizes
          load_single_cmd->dst_ptr = load_single_cmd->dest_addr;
          load_single_cmd->bytes_done = 0;
          // by default, copy size is the full file.
          load_single_cmd->length_to_copy = isofs->get_length(load_single_cmd->file_record);

          if (load_single_cmd->length_to_copy == 0) {
            // if we get zero for some reason, use the commanded length.
            ASSERT(false);
            load_single_cmd->length_to_copy = load_single_cmd->length;
          } else if (load_single_cmd->length < load_single_cmd->length_to_copy) {
            // if we ask for less than the full length, use the smaller value.
            load_single_cmd->length_to_copy = load_single_cmd->length;
          }

          // set status and callback function.
          load_single_cmd->status = CMD_STATUS_IN_PROGRESS;
          u32 cmd_id = msg_from_mbx->cmd_id;
          if (cmd_id == LOAD_TO_EE_CMD_ID || cmd_id == LOAD_TO_EE_OFFSET_CMD_ID) {
            msg_from_mbx->callback_function = CopyDataToEE;
          } else if (cmd_id == LOAD_TO_IOP_CMD_ID) {
            msg_from_mbx->callback_function = CopyDataToIOP;
          }

        } break;
        case LOAD_DGO_CMD_ID: {
          if (!QueueMessage(msg_from_mbx, 0, "LoadDGO")) {
            break;
          }
          // Got a DGO command. There is one LoadDGO command for the entire DGO.
          // queued successfully, open the file.
          auto* load_dgo_cmd = (DgoCommand*)msg_from_mbx;
          load_dgo_cmd->fd = isofs->open(load_dgo_cmd->fr, -1);
          if (!load_dgo_cmd->fd) {
            // failed to open, return error
            load_dgo_cmd->status = CMD_STATUS_FAILED_TO_OPEN;
            UnqueueMessage(load_dgo_cmd);
            if (g_game_version == GameVersion::Jak1) {
              ReturnMessage(load_dgo_cmd);
            } else {
              SendMbx(iso_mbx, &sLoadDGO);
            }
          } else {
            // init DGO state machine and register as the callback.
            load_dgo_cmd->status = CMD_STATUS_IN_PROGRESS;
            load_dgo_cmd->dgo_state = DgoState::Init;
            load_dgo_cmd->callback_function = RunDGOStateMachine;
          }

        } break;
        case LOAD_SOUND_BANK: {
          // NOTE: this check has been removed. there doesn't seem to be any issues with this, and
          // it fixes some other issues. there doesn't appear to be any extra safety from it either

          // if there's an in progress vag command, try again.
          // if (in_progress_vag_command && !in_progress_vag_command->paused) {
          //   SendMbx(iso_mbx, msg_from_mbx);
          //   break;
          // }

          auto buff = TryAllocateBuffer(BUFFER_PAGE_SIZE);
          if (!buff) {
            // no buffers, try again.
            SendMbx(iso_mbx, msg_from_mbx);
            break;
          }

          auto* cmd = (SoundBankLoadCommand*)msg_from_mbx;
          isofs->load_sound_bank(cmd->bank_name, cmd->bank);
          FreeBuffer(buff);
          ReturnMessage(msg_from_mbx);
        } break;
        case LOAD_MUSIC: {
          // NOTE: this check has been removed. there doesn't seem to be any issues with this, and
          // it fixes some other issues. there doesn't appear to be any extra safety from it either

          // if there's an in progress vag command, try again.
          // if (in_progress_vag_command && !in_progress_vag_command->paused) {
          //   SendMbx(iso_mbx, msg_from_mbx);
          //   break;
          // }

          auto buff = TryAllocateBuffer(BUFFER_PAGE_SIZE);
          if (!buff) {
            // no buffers, try again.
            SendMbx(iso_mbx, msg_from_mbx);
            break;
          }

          auto* cmd = (MusicLoadCommand*)msg_from_mbx;
          isofs->load_music(cmd->music_name, cmd->music_handle);
          FreeBuffer(buff);
          ReturnMessage(msg_from_mbx);

        } break;
        case QUEUE_VAG_STREAM: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          if (cmd->vag &&
              (!in_progress_vag_command || in_progress_vag_command->vag != cmd->vag ||
               in_progress_vag_command->file != cmd->file) &&
              (!in_progress_vag_command || (!vag_paused && in_progress_vag_command->paused))) {
            if (in_progress_vag_command) {
              gVAGCMD = nullptr;
              StopVAG(in_progress_vag_command);
              ReleaseMessage(in_progress_vag_command);
            }
            in_progress_vag_command = &vag_cmd;
            memcpy(&vag_cmd, cmd, sizeof(vag_cmd));
            InitVAGCmd(&vag_cmd, 1);
            LoadStackEntry* file = nullptr;
            if (QueueMessage(&vag_cmd, 3, "QueueVAG")) {
              if (vag_cmd.vag) {
                file = isofs->open_wad(vag_cmd.file, vag_cmd.vag->offset);
              }
              vag_cmd.fd = file;
              vag_cmd.status = -1;
              vag_cmd.callback_function = ProcessVAGData;
              gVAGCMD = &vag_cmd;
            } else {
              in_progress_vag_command = nullptr;
            }
          }
          ReturnMessage(cmd);
        } break;
        case PLAY_VAG_STREAM: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          bool thing = true;
          if (in_progress_vag_command && in_progress_vag_command->vag == cmd->vag &&
              in_progress_vag_command->file == cmd->file) {
            in_progress_vag_command->volume = cmd->volume;
            in_progress_vag_command->sound_id = cmd->sound_id;
            if (in_progress_vag_command->paused) {
              if (vag_paused) {
                unk = 1;
              } else {
                UnpauseVAG(in_progress_vag_command);
              }
            }
          } else {
            if (in_progress_vag_command && !in_progress_vag_command->paused &&
                cmd->priority < in_progress_vag_command->priority) {
              thing = false;
            }
            if (thing) {
              if (in_progress_vag_command) {
                gVAGCMD = nullptr;
                StopVAG(in_progress_vag_command);
                ReleaseMessage(in_progress_vag_command);
              }
              in_progress_vag_command = &vag_cmd;
              memcpy(&vag_cmd, cmd, sizeof(vag_cmd));
              if (vag_paused) {
                InitVAGCmd(&vag_cmd, 1);
                unk = 1;
              } else {
                InitVAGCmd(&vag_cmd, 0);
              }
              vag_cmd.messagebox_to_reply = 0;
              vag_cmd.thread_id = 0;
              if (QueueMessage(&vag_cmd, 3, "PlayVag")) {
                if (vag_cmd.vag) {
                  vag_cmd.fd = isofs->open_wad(vag_cmd.file, vag_cmd.vag->offset);
                } else {
                  vag_cmd.fd = nullptr;
                }
                vag_cmd.status = -1;
                vag_cmd.callback_function = ProcessVAGData;
                gVAGCMD = &vag_cmd;
              } else {
                in_progress_vag_command = nullptr;
              }
            }
          }
          if (thing) {
            if (!in_progress_vag_command || in_progress_vag_command->fd) {
              gRealVAGClock = 0;
              gRealVAGClockS = 0;
              gRealVAGClockRunning = true;
            } else {
              gFakeVAGClock = 0;
              gFakeVAGClockRunning = true;
              gFakeVAGClockPaused = 0;
            }
            gVAG_Id = in_progress_vag_command ? in_progress_vag_command->sound_id : 1;
          }
          ReturnMessage(cmd);
        } break;
        case STOP_VAG_STREAM: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          if (in_progress_vag_command && (!cmd->vag || in_progress_vag_command->vag == cmd->vag) &&
              (cmd->priority >= in_progress_vag_command->priority)) {
            gVAGCMD = nullptr;
            StopVAG(in_progress_vag_command);
            ReleaseMessage(in_progress_vag_command);
            in_progress_vag_command = nullptr;
          }
          vag_paused = 0;
          unk = 0;
          ReturnMessage(cmd);
        } break;
        case PAUSE_VAG_STREAM: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          gFakeVAGClockPaused = 1;
          if (!vag_paused) {
            if (!in_progress_vag_command || in_progress_vag_command->paused) {
              unk = 0;
            } else {
              PauseVAG(in_progress_vag_command);
              unk = 1;
            }
            vag_paused = 1;
          }
          ReturnMessage(cmd);
        } break;
        case CONTINUE_VAG_STREAM: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          gFakeVAGClockPaused = 0;
          if (vag_paused) {
            if (unk) {
              UnpauseVAG(in_progress_vag_command);
            }
            vag_paused = 0;
            unk = 0;
          }
          ReturnMessage(cmd);
        } break;
        case SET_VAG_VOLUME: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          if (in_progress_vag_command) {
            in_progress_vag_command->volume = cmd->volume;
            SetVAGVol();
          }
          ReturnMessage(cmd);
        } break;
        case SET_DIALOG_VOLUME: {
          auto* cmd = (VagCommand*)msg_from_mbx;
          gDialogVolume = cmd->volume;
          if (in_progress_vag_command)
            SetVAGVol();
          ReturnMessage(cmd);
        } break;
        default:
          printf("[OVERLORD] Unknown ISOThread message id 0x%x\n", msg_from_mbx->cmd_id);
      }
    } else if (mbx_status == KE_WAIT_DELETE) {
      return 0;
    }

    ////////////////////////////
    // Handle Sound
    ////////////////////////////

    if (g_game_version == GameVersion::Jak1) {
      if (in_progress_vag_command && !CheckVAGStreamProgress(in_progress_vag_command)) {
        gVAGCMD = nullptr;
        StopVAG(in_progress_vag_command);
        ReleaseMessage(in_progress_vag_command);
        in_progress_vag_command = nullptr;
        // added. this variable seems to determine whether a vag stream is actually playing, and it
        // is possible to get into a scenario where (for example) you want to unpause a vag stream
        // but a different sound command hasn't run yet to correct this value, which makes the game
        // either play the wrong sound or crash right away if no actual sound is to be played with
        // the vag stream
        unk = 0;
      }
    }

    ////////////////////////////
    // Begin a read
    ////////////////////////////

    IsoBufferHeader* read_buffer = nullptr;
    IsoMessage* cmd_to_process = GetMessage();
    if (cmd_to_process) {  // okay, there's a command queued that we should process
      // prep for a read !! DANGER !! - this read _may_ complete after the command is done.
      // At this point we don't know if the command actually needs another read or not!
      if (cmd_to_process->callback_function == ProcessVAGData) {
        read_buffer = AllocateBuffer(STR_BUFFER_DATA_SIZE);
      } else {
        read_buffer = AllocateBuffer(BUFFER_PAGE_SIZE);
      }

      if (!read_buffer) {
        // there aren't enough buffers.  give up on this command for now.
        cmd_to_process = nullptr;
      } else {
        // kick off read
        if (cmd_to_process->callback_function == ProcessVAGData) {
          cmd_to_process->status =
              isofs->begin_read(cmd_to_process->fd, read_buffer->get_data(), STR_BUFFER_DATA_SIZE);
        } else {
          cmd_to_process->status =
              isofs->begin_read(cmd_to_process->fd, read_buffer->get_data(), BUFFER_PAGE_SIZE);
        }

        // if we have bad status, kill read buffer
        if (cmd_to_process->status != CMD_STATUS_IN_PROGRESS) {
          FreeBuffer(read_buffer);
          read_buffer = nullptr;
          cmd_to_process = nullptr;
        }
      }
    }

    if (!cmd_to_process) {
      // drive is doing nothing, make sure the DVD is still in there.
      isofs->poll_drive();
    }

    // Deal with completed reads.  NOTE - this can close files and terminate return commands!
    ProcessMessageData();

    if (!read_buffer) {
      // didn't actually start a read, just delay for a bit I guess.
      DelayThread(100);
    } else {
      // attempt to sync read.  If we closed the file mid-read in ProcessMessageData, this returns
      // an error code.
      u32 read_status = isofs->sync_read();
      if (read_status == CMD_STATUS_READ_ERR) {
        // closed file mid-read, or the read failed.  Either way we can't give this read buffer to
        // anybody, so we should just free it.
        FreeBuffer(read_buffer);
      } else {
        // read is good!
        cmd_to_process->status = read_status;
        // setup the buffer for the callback.
        if (cmd_to_process->callback_function == ProcessVAGData) {
          read_buffer->data = read_buffer->get_data();
          read_buffer->data_size = STR_BUFFER_DATA_SIZE;
        } else {
          read_buffer->data = read_buffer->get_data();
          read_buffer->data_size = BUFFER_PAGE_SIZE;
        }

        // add buffer to linked list of buffers.
        if (!cmd_to_process->callback_buffer) {
          cmd_to_process->callback_buffer = read_buffer;
        } else {
          auto* bh = cmd_to_process->callback_buffer;
          while (bh->next) {
            bh = (IsoBufferHeader*)bh->next;
          }
          bh->next = read_buffer;
        }
      }
    }
  }  // for
  return 0;
}

/*!
 * Handler for DGO data buffers.
 */
u32 RunDGOStateMachine(IsoMessage* _cmd, IsoBufferHeader* buffer) {
  auto* cmd = (DgoCommand*)_cmd;
  u32 return_value = CMD_STATUS_IN_PROGRESS;
  u8* unprocessed_data = (u8*)buffer->data;
  u32 bytes_left = buffer->data_size;

  // loop until we've read all the data
  while (bytes_left) {
    // printf("run DGO in state %d (%s) with %d unprocessed buffered bytes\n", cmd->dgoState,
    // names[cmd->dgoState], buffer->data_size);
    switch (cmd->dgo_state) {
      case DgoState::Init:  // init
        cmd->bytes_processed = 0;
        // start by reading header.
        cmd->dgo_state = DgoState::Read_Header;
        cmd->finished_first_obj = 0;
        cmd->want_abort = 0;
        break;

      case DgoState::Read_Header:  // read dgo header.  If we are unlucky this crosses a boundary
                                   // and we have to do this in two chunks
      {
        u32 bytes_to_read = sizeof(DgoHeader) - cmd->bytes_processed;
        if (bytes_to_read > bytes_left) {
          bytes_to_read = bytes_left;
        }

        // copy to our local storage
        memcpy((u8*)&cmd->dgo_header + cmd->bytes_processed, unprocessed_data, bytes_to_read);
        unprocessed_data += bytes_to_read;
        bytes_left -= bytes_to_read;
        cmd->bytes_processed += bytes_to_read;

        // if we are done with header
        if (cmd->bytes_processed == sizeof(DgoHeader)) {
          lg::info("[Overlord DGO] Got DGO file header for {} with {} objects",
                   cmd->dgo_header.name, cmd->dgo_header.object_count);
          cmd->bytes_processed = 0;
          cmd->objects_loaded = 0;
          if (cmd->dgo_header.object_count == 1) {
            // if there's only one object, load to top immediately
            cmd->buffer_toggle = 0;
            cmd->ee_destination_buffer = cmd->buffer_heaptop;
            cmd->dgo_state = DgoState::Read_Obj_Header;
          } else {
            // otherwise load to buffer1 first.
            cmd->buffer_toggle = 1;
            cmd->ee_destination_buffer = cmd->buffer1;
            cmd->dgo_state = DgoState::Read_Obj_Header;
          }
        }
      } break;

      case DgoState::Finish_Obj:  // we have reached the end of an object file!
      {
        // EE synchronization occurs here.
        // we "Return" the command to tell the EE that we've loaded stuff
        // EE sends us data that we read with LookMbx so we keep loading.
        // the order is that we wait for the EE to tell us the next location before we return
        // the most recently loaded object.

        // we skip this if we're loading the first object so we can double buffer the
        // linking/loading process and have two in flight at a time (one loading, other linking)
        // this fills the pipeline.

        // for jak 2, double buffered is disabled for the "borrow" style loads.
        // this is detected by seeing that buffer1 and buffer2 are the same address.
        // this function decompiles poorly in ghidra, so this is a best guess.
        // in this mode, the order is swapped - the overlord returns the message once loading
        // is done, then waits for the next loaddgo.

        if (cmd->finished_first_obj) {
          // in all cases, need sync after the first object.
          s32 isSync = LookMbx(sync_mbx);  // did we get a "sync" message?
          if (isSync) {
            // if so, this means we got a CancelDGO or NextDGO
            if (cmd->want_abort) {
              // we got a CancelDGO.
              cmd->dgo_state = DgoState::Finish_Dgo;
              break;
            }
          } else {
            // nope, ee isn't ready. bail and wait for next run.
            goto cleanup_and_return;
          }
        }

        if (cmd->buffer1 != cmd->buffer2) {
          // normal double buffered case.
          cmd->finished_first_obj = 1;
          cmd->status = CMD_STATUS_IN_PROGRESS;

          // select a buffer for next time.
          if (cmd->buffer_toggle == 1) {
            cmd->selectedBuffer = cmd->buffer1;
          } else {
            cmd->selectedBuffer = cmd->buffer2;
          }

          // we've processed the command, go wake up the DGO RPC thread.
          // doesn't terminate the command (ReleaseMessage does this, ReturnMessage just
          // wakes up the caller while keeping the command alive).
          ReturnMessage(cmd);
        } else {
          // single buffer mode. before we can move on, we need to wait for the EE to send us
          // an update load location. We've already informed the EE where we loaded the most
          // recent object, and it is busy linking...
          if (cmd->finished_first_obj == 0) {
            s32 isSync = LookMbx(sync_mbx);  // did we get a "sync" message?
            if (!isSync) {
              goto cleanup_and_return;
            }
            cmd->finished_first_obj = 1;
          }
        }

        // toggle buffer
        if (cmd->buffer_toggle == 1) {
          cmd->ee_destination_buffer = cmd->buffer2;
          cmd->buffer_toggle = 2;
        } else {
          cmd->ee_destination_buffer = cmd->buffer1;
          cmd->buffer_toggle = 1;
        }

        // setup for next run
        if (cmd->objects_loaded + 1 == cmd->dgo_header.object_count &&
            cmd->buffer1 != cmd->buffer2) {
          cmd->dgo_state = DgoState::Read_Last_Obj;
        } else {
          cmd->dgo_state = DgoState::Read_Obj_Header;
        }
        break;
      }

      case DgoState::Read_Last_Obj:  // setup load last
      {
        // extra sync here
        s32 sync = LookMbx(sync_mbx);
        if (sync) {
          if (cmd->want_abort) {
            cmd->dgo_state = DgoState::Finish_Dgo;
          } else {
            // EE ready, no abort. Nothing in flight, so we are safe to do a top load!
            cmd->ee_destination_buffer = cmd->buffer_heaptop;
            cmd->buffer_toggle = 0;
            cmd->dgo_state = DgoState::Read_Obj_Header;
          }
        } else {
          goto cleanup_and_return;
        }
      } break;

      case DgoState::Read_Obj_Header:  // read object file header
      {
        u32 bytesToRead = sizeof(ObjectHeader) - cmd->bytes_processed;
        if (bytes_left < bytesToRead) {
          bytesToRead = bytes_left;
        }

        // for now, buffer locally
        memcpy((u8*)&cmd->objHeader + cmd->bytes_processed, unprocessed_data, bytesToRead);
        unprocessed_data += bytesToRead;
        bytes_left -= bytesToRead;
        cmd->bytes_processed += bytesToRead;

        // once we're done, send the header to the EE, and start reading object data
        if (cmd->bytes_processed == sizeof(ObjectHeader)) {
          //          printf("[Overlord DGO] Got object header for %s, object size 0x%x bytes (sent
          //          to 0x%p)\n",
          //                 cmd->objHeader.name, cmd->objHeader.size, cmd->ee_destination_buffer);
          DMA_SendToEE(&cmd->objHeader, sizeof(ObjectHeader), cmd->ee_destination_buffer);
          DMA_Sync();
          cmd->ee_destination_buffer += sizeof(ObjectHeader);
          cmd->objHeader.size = (cmd->objHeader.size + 0xf) & 0xfffffff0;
          cmd->dgo_state = DgoState::Read_Obj_data;
          cmd->bytes_processed = 0;
        }
      } break;

      case DgoState::Read_Obj_data:  // read object file data
      {
        u32 bytesToRead = cmd->objHeader.size - cmd->bytes_processed;
        if (bytes_left < bytesToRead) {
          bytesToRead = bytes_left;
        }

        // send contents directly to EE
        DMA_SendToEE(unprocessed_data, bytesToRead, cmd->ee_destination_buffer);
        DMA_Sync();
        unprocessed_data += bytesToRead;
        bytes_left -= bytesToRead;
        cmd->ee_destination_buffer += bytesToRead;
        cmd->bytes_processed += bytesToRead;

        if (cmd->bytes_processed == cmd->objHeader.size) {
          cmd->objects_loaded++;
          if (cmd->objects_loaded == cmd->dgo_header.object_count) {
            cmd->dgo_state = DgoState::Finish_Dgo;
          } else {
            // this logic makes jak 2 single buffer loads go to NoDoubleBuffer to return the
            // command as soon as loading is done.
            cmd->dgo_state = (cmd->buffer1 == cmd->buffer2) ? DgoState::Finish_Obj_NoDoubleBuffer
                                                            : DgoState::Finish_Obj;
            cmd->bytes_processed = 0;
          }
        }
      } break;

      case DgoState::Finish_Dgo: {
        // done with buffer, complete. Kill the ISO thread read.
        return_value = CMD_STATUS_DONE;
        goto cleanup_and_return;
      }

      case DgoState::Finish_Obj_NoDoubleBuffer: {
        // new, added for jak2 - here we return the message once loading finishes.
        cmd->status = CMD_STATUS_IN_PROGRESS;
        if (cmd->buffer_toggle == 1) {
          cmd->selectedBuffer = cmd->buffer1;
        } else {
          cmd->selectedBuffer = cmd->buffer2;
        }
        ReturnMessage(cmd);
        cmd->dgo_state = DgoState::Finish_Obj;
      } break;

      default:
        printf("unknown dgoState!\n");
    }
  }

  //  printf("[DGO State Machine Complete] Out of things to read!\n");

cleanup_and_return:
  if (return_value == 0) {
    buffer->data = nullptr;
    buffer->data_size = 0;
  } else {
    if (!bytes_left) {
      buffer->data = nullptr;
      buffer->data_size = 0;
    } else {
      buffer->data = unprocessed_data;
      buffer->data_size = bytes_left;
    }
  }
  return return_value;
}

/*!
 * Callback for sending to EE.
 */
u32 CopyDataToEE(IsoMessage* _cmd, IsoBufferHeader* buffer_header) {
  auto* cmd = (IsoCommandLoadSingle*)_cmd;

  s32 bytes_to_send = cmd->length_to_copy - cmd->bytes_done;

  // make sure we don't copy too much (if the buffer does not have enough data)
  if (buffer_header->data_size < (u32)bytes_to_send) {
    bytes_to_send = (s32)buffer_header->data_size;
  }

  DMA_SendToEE(buffer_header->get_data(), bytes_to_send, cmd->dest_addr);
  DMA_Sync();

  cmd->dest_addr += bytes_to_send;
  cmd->bytes_done += bytes_to_send;
  buffer_header->data = nullptr;
  buffer_header->data_size = 0;
  if (cmd->bytes_done == cmd->length_to_copy) {
    return CMD_STATUS_DONE;
  } else {
    return CMD_STATUS_IN_PROGRESS;
  }
}

/*!
 * Callback for loading to IOP buffer.
 */
u32 CopyDataToIOP(IsoMessage* _cmd, IsoBufferHeader* buffer_header) {
  auto* cmd = (IsoCommandLoadSingle*)_cmd;

  s32 bytes_to_send = cmd->length_to_copy - cmd->bytes_done;

  // make sure we don't copy too much (if the buffer does not have enough data)
  if (buffer_header->data_size < (u32)bytes_to_send) {
    bytes_to_send = (s32)buffer_header->data_size;
  }

  memcpy(cmd->dst_ptr, buffer_header->get_data(), bytes_to_send);

  cmd->dst_ptr += bytes_to_send;
  cmd->bytes_done += bytes_to_send;
  buffer_header->data = nullptr;
  buffer_header->data_size = 0;
  if (cmd->bytes_done == cmd->length_to_copy) {
    return CMD_STATUS_DONE;
  } else {
    return CMD_STATUS_IN_PROGRESS;
  }
}

/*!
 * Callback which does nothing.
 */
u32 NullCallback(IsoMessage* _cmd, IsoBufferHeader* buffer_header) {
  (void)_cmd;
  buffer_header->data_size = 0;
  return CMD_STATUS_NULL_CB;
}

/*!
 * Initialize a VagCommand.
 */
static void InitVAGCmd(VagCommand* cmd, u32 x) {
  cmd->buffer_number = 0;
  cmd->data_left = 0;
  cmd->started = 0;
  cmd->paused = x;
  cmd->sample_rate = 0;
  cmd->stop = 0;
  cmd->end_point = -1;
  cmd->unk2 = 0;
  gPlayPos = 48;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
}

/*!
 * Byte-swap.
 */
u32 bswap(u32 in) {
  return ((in >> 0x18) & 0xff) | ((in >> 8) & 0xff00) | ((in & 0xff00) << 8) | (in << 0x18);
}

static u32 ProcessVAGData(IsoMessage* _cmd, IsoBufferHeader* buffer_header) {
  auto* vag = (VagCommand*)_cmd;
  if (vag->stop) {
    buffer_header->data_size = 0;
    return CMD_STATUS_IN_PROGRESS;
  }

  if (vag->buffer_number == 0) {
    // first buffer, set stuff up
    u32* data = (u32*)buffer_header->data;
    if (data[0] != 0x70474156 /* 'pGAV' */ && data[0] != 0x56414770 /* 'VAGp' */) {
      vag->stop = true;
      buffer_header->data_size = 0;
      return CMD_STATUS_IN_PROGRESS;
    }

    vag->sample_rate = data[4];
    vag->data_left = data[3];
    if (data[0] == 0x70474156 /* 'pGAV' */) {
      vag->sample_rate = bswap(vag->sample_rate);
      vag->data_left = bswap(vag->data_left);
    }

    gSampleRate = vag->sample_rate;
    gLastVagHalf = false;
    vag->data_left += 48;
    if (buffer_header->data_size >= vag->data_left) {
      vag->end_point = vag->data_left - 16;
    }

    if (!DMA_SendToSPUAndSync(buffer_header->data, buffer_header->data_size, gStreamSRAM)) {
      return CMD_STATUS_IN_PROGRESS;
    }

    sceSdSetParam(gVoice | SD_VP_VOLL, 0);
    sceSdSetParam(gVoice | SD_VP_VOLR, 0);
    u32 vmix = sceSdGetSwitch((gVoice & 1) | SD_S_VMIXL);
    sceSdSetSwitch((gVoice & 1) | SD_S_VMIXL, vmix | (1 << (gVoice >> 1)));
    vmix = sceSdGetSwitch((gVoice & 1) | SD_S_VMIXR);
    sceSdSetSwitch((gVoice & 1) | SD_S_VMIXR, vmix | (1 << (gVoice >> 1)));
    sceSdSetParam(gVoice | SD_VP_PITCH, 0);
    sceSdSetAddr(gVoice | SD_VA_SSA, gStreamSRAM + 0x30);
    sceSdSetParam(gVoice | SD_VP_ADSR1, 0xf);
    sceSdSetParam(gVoice | SD_VP_ADSR2, 0x1fc0);
    if (vag->end_point == -1) {
      sceSdSetAddr(gVoice | SD_VA_LSAX, gTrapSRAM);
    }
    snd_keyOnVoiceRaw(gVoice & 1, gVoice >> 1);
    vag->started = 1;
    vag->data_left -= buffer_header->data_size;
    buffer_header->data_size = 0;
  }

  if (vag->buffer_number == 1) {
    if (buffer_header->data_size < vag->data_left) {
      VAG_MarkLoopEnd(buffer_header->data, buffer_header->data_size);
      FlushDcache();
    } else {
      vag->end_point = vag->data_left + 0x5FF0;
    }

    if (!DMA_SendToSPUAndSync(buffer_header->data, buffer_header->data_size,
                              gStreamSRAM + 0x6000)) {
      return CMD_STATUS_IN_PROGRESS;
    }

    if (!vag->paused) {
      vag->paused = 1;
      UnpauseVAG(vag);
    }

    vag->ready_for_data = 0;
    vag->data_left -= buffer_header->data_size;
    buffer_header->data_size = 0;
    gPlayPos = 48;
    gPlaying = true;
  }

  if (vag->buffer_number > 1) {
    if ((vag->buffer_number & 1) != 0) {
      if (buffer_header->data_size < vag->data_left) {
        VAG_MarkLoopEnd(buffer_header->data, buffer_header->data_size);
        FlushDcache();
      } else {
        vag->end_point = vag->data_left + 0x5FF0;
      }

      if (!DMA_SendToSPUAndSync(buffer_header->data, buffer_header->data_size,
                                gStreamSRAM + 0x6000)) {
        return CMD_STATUS_IN_PROGRESS;
      }

      sceSdSetAddr(gVoice | SD_VA_LSAX, gStreamSRAM + 0x6000);
    } else {
      if (buffer_header->data_size < vag->data_left) {
        VAG_MarkLoopEnd(buffer_header->data, buffer_header->data_size);
        FlushDcache();
      } else {
        vag->end_point = vag->data_left - 16;
      }

      if (!DMA_SendToSPUAndSync(buffer_header->data, buffer_header->data_size, gStreamSRAM)) {
        return CMD_STATUS_IN_PROGRESS;
      }

      sceSdSetAddr(gVoice | SD_VA_LSAX, gStreamSRAM);
    }

    vag->ready_for_data = 0;
    vag->data_left -= buffer_header->data_size;
    buffer_header->data_size = 0;
  }

  vag->buffer_number++;
  return CMD_STATUS_IN_PROGRESS;
}

static s32 CheckVAGStreamProgress(VagCommand* vag) {
  if (vag->stop) {
    return 0;
  }

  if (!vag->started) {
    return 1;
  }

  if (vag->end_point != -1) {
    if ((s32)(gPlayPos & 0xFFFFFFF0) == vag->end_point) {
      return 0;
    }

    if (((gPlayPos < 0x6000) && (vag->end_point < 0x6000)) ||
        ((0x5fff < gPlayPos && (0x5fff < vag->end_point)))) {
      if ((vag->unk2 == 0) && (gPlayPos < (u32)vag->end_point)) {
        sceSdSetAddr(gVoice | SD_VA_LSAX, gStreamSRAM + vag->end_point);
        vag->unk2 = 1;
      }

      return 1;
    }
  }

  if (gPlayPos < 0x6000) {
    if ((vag->buffer_number & 1) != 0) {
      vag->ready_for_data = 1;
      sceSdSetAddr(gVoice | SD_VA_LSAX, gTrapSRAM);
    }

  } else {
    if ((vag->buffer_number & 1) == 0) {
      vag->ready_for_data = 1;
      sceSdSetAddr(gVoice | SD_VA_LSAX, gTrapSRAM);
    }
  }

  return 1;
}

static void StopVAG(VagCommand* vag) {
  gPlaying = false;
  PauseVAG(vag);
  snd_keyOffVoiceRaw(gVoice & 1, gVoice >> 1);
  gFakeVAGClockRunning = false;
  gRealVAGClockRunning = false;
  gVAG_Id = 1;
}

static void PauseVAG(VagCommand* vag) {
  gFakeVAGClockPaused = true;
  if (vag->paused) {
    return;
  }

  vag->paused = true;
  if (vag->started) {
    sceSdSetParam(gVoice | SD_VP_VOLL, 0);
    sceSdSetParam(gVoice | SD_VP_VOLR, 0);
    sceSdSetParam(gVoice | SD_VP_PITCH, 0);
  }
}

static void CalculateVAGVolumes(s32 volume, s32 positioned, Vec3w* trans, s32* left, s32* right) {
  if (positioned) {
    volume = CalculateFallofVolume(trans, (volume * gDialogVolume) >> 10, 1, 10, 50);
    auto* pan = &gPanTable[(630 - CalculateAngle(trans)) % 360];
    *left = (pan->left * volume) >> 10;
    *right = (pan->right * volume) >> 10;
    if (*left >= 0x4000) {
      *left = 0x3FFF;
    }
    if (*right >= 0x4000) {
      *right = 0x3FFF;
    }
  } else {
    volume = (volume * gDialogVolume) >> 6;
    if (volume >= 0x4000) {
      volume = 0x3FFF;
    }

    *left = volume;
    *right = volume;
  }
}

static void UnpauseVAG(VagCommand* vag) {
  gFakeVAGClockPaused = false;
  if (vag->paused) {
    if (vag->started) {
      s32 left = 0, right = 0;
      CalculateVAGVolumes(vag->volume, vag->positioned, &vag->trans, &left, &right);
      sceSdSetParam(gVoice | SD_VP_VOLL, left);
      sceSdSetParam(gVoice | SD_VP_VOLR, right);
      sceSdSetParam(gVoice | SD_VP_PITCH, (vag->sample_rate << 12) / 48000);
    }

    vag->paused = false;
  }
}

void SetVAGVol() {
  if (gVAGCMD && gVAGCMD->started && !gVAGCMD->paused) {
    s32 left = 0, right = 0;
    CalculateVAGVolumes(gVAGCMD->volume, gVAGCMD->positioned, &gVAGCMD->trans, &left, &right);
    sceSdSetParam(gVoice | SD_VP_VOLL, left);
    sceSdSetParam(gVoice | SD_VP_VOLR, right);
  }
}

static s32 GetPlayPos() {
  // This does the safe NAX read by reading NAX in a loop until it returns
  // the same value 3 times in a row. We can skip this on pc.
  u32 NAX = sceSdGetAddr(gVoice | SD_VA_NAX);
  // We can also say our sample buffer always starts at 0 to simplify things.
  if (NAX > 0xBFFF) {
    return -1;
  } else {
    return NAX;
  }
}

static void UpdatePlayPos() {
  if (!gPlaying) {
    return;
  }

  u32 pos = GetPlayPos();
  if (pos == 0xffffffff) {
    if (gLastVagHalf) {
      pos = 0xC000;
    } else {
      pos = 0x6000;
    }
  } else {
    gLastVagHalf = pos >= 0x6000;
  }

  if (pos >= gPlayPos) {
    gRealVAGClockS += pos - gPlayPos;
  } else {
    gRealVAGClockS += pos + 0xC000 - gPlayPos;
  }

  gRealVAGClock = 4 * (0x1C00 * (gRealVAGClockS / 16) / gSampleRate);
  gPlayPos = pos;
}

s32 GetVAGStreamPos() {
  UpdatePlayPos();
  if (gFakeVAGClockRunning) {
    return gFakeVAGClock;
  }
  if (gRealVAGClockRunning) {
    return gRealVAGClock;
  }
  return -1;
}

static void VAG_MarkLoopEnd(void* data, u32 size) {
  ((u8*)data)[size - 15] = 3;
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
}  // namespace jak1
