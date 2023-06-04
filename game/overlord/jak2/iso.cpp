#include "iso.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/common/dma.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/common/srpc.h"
#include "game/overlord/jak2/iso_api.h"
#include "game/overlord/jak2/iso_cd.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/overlord/jak2/srpc.h"
#include "game/overlord/jak2/ssound.h"
#include "game/overlord/jak2/stream.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {
void spu_dma_hack();
u32 DGOThread();
FileRecord* FindISOFile(const char* name);
int LoadISOFileToIOP(FileRecord* fr, uint8_t* dest, int length);
int RunDGOStateMachine(CmdHeader* param_1, Buffer* param_2);
void CancelDGO(RPC_Dgo_Cmd* cmd);
int CopyDataToIOP(CmdHeader* param_1, Buffer* param_2);
int CopyDataToEE(CmdHeader* param_1, Buffer* param_2);

IsoFs* isofs;

int iso_init_flag = 0;
int iso_mbx = 0;
int dgo_mbx = 0;
int sync_mbx = 0;
int iso_thread = 0;
int dgo_thread = 0;
int str_thread = 0;
int play_thread = 0;

int ext_pause = 0;
int ext_resume = 0;

CmdDgo sLoadDgo;  // renamed from scmd to sLoadDGO in Jak 2
static RPC_Dgo_Cmd sRPCBuff[1];
VagDir gVagDir;

/// The main buffer used for reading data and doing blzo decompression.
LargeBuffer* SpLargeBuffer = nullptr;

void iso_init_globals() {
  isofs = nullptr;
  iso_init_flag = 0;
  iso_mbx = 0;
  dgo_mbx = 0;
  sync_mbx = 0;
  iso_thread = 0;
  dgo_thread = 0;
  str_thread = 0;
  play_thread = 0;
  ext_pause = 0;
  ext_resume = 0;
  memset(&sLoadDgo, 0, sizeof(sLoadDgo));
  memset(&gVagDir, 0, sizeof(gVagDir));
  memset(sRPCBuff, 0, sizeof(RPC_Dgo_Cmd));
  SpLargeBuffer = nullptr;
}

// The "ISO Thread" is responsible managing "messages" - both starting reads for messages that need
// data, and calling callbacks on messages that have pending data.

/*!
 * Set up messageboxes/threads for the ISO thread.
 */
u32 InitISOFS() {
  isofs = &iso_cd;
  iso_init_flag = 1;

  SpLargeBuffer = (LargeBuffer*)ScratchPadMemory;
  ScratchPadMemory += sizeof(LargeBuffer);

  MbxParam mbx_param;
  mbx_param.attr = 0;
  mbx_param.option = 0;
  iso_mbx = CreateMbx(&mbx_param);
  if (iso_mbx < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create ISO mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  mbx_param.attr = 0;
  mbx_param.option = 0;
  dgo_mbx = CreateMbx(&mbx_param);
  if (dgo_mbx < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create DGO mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }
  mbx_param.attr = 0;
  mbx_param.option = 0;
  sync_mbx = CreateMbx(&mbx_param);
  if (sync_mbx < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create sync mbx\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }

  ThreadParam thread_param;
  thread_param.entry = ISOThread;
  thread_param.initPriority = 0x6e;
  thread_param.attr = TH_C;
  thread_param.stackSize = 0x1000;
  thread_param.option = 0;
  iso_thread = CreateThread(&thread_param);
  if (iso_thread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create ISO thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }
  thread_param.entry = DGOThread;
  thread_param.attr = TH_C;
  thread_param.initPriority = 0x6f;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  dgo_thread = CreateThread(&thread_param);
  if (dgo_thread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create DGO thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }
  thread_param.entry = STRThread;
  thread_param.attr = TH_C;
  thread_param.initPriority = 0x72;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  str_thread = CreateThread(&thread_param);
  if (str_thread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create STR thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }
  thread_param.entry = PLAYThread;
  thread_param.attr = 0x2000000;
  thread_param.initPriority = 0x3c;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  play_thread = CreateThread(&thread_param);
  if (play_thread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: iso InitISOFS: Cannot create PLAY thread\n");
    printf("IOP: ======================================================================\n");
    return 1;
  }
  StartThread(iso_thread, 0);
  StartThread(dgo_thread, 0);
  StartThread(str_thread, 0);
  StartThread(play_thread, 0);
  // wait for ISO Thread to initialize
  WaitMbx(sync_mbx);

  FileRecord* vagdir_file = FindISOFile("VAGDIR.AYB");
  if (vagdir_file) {
    LoadISOFileToIOP(vagdir_file, (u8*)&gVagDir, sizeof(gVagDir));
  } else {
    printf("IOP: ======================================================================\n");
    printf("IOP : iso InitISOFS : cannot load VAG directory\n");
    printf("IOP: ======================================================================\n");
  }

  // loading screen thing.
  /*
  iVar2 = FindISOFile(param_2);
  if (iVar2 == 0) {
    return iso_init_flag;
  }
  LoadISOFileToEE(iVar2, 0x1000000, 0x800000);
   */
  return iso_init_flag;
}

void IsoQueueVagStream(VagCmd* cmd, int param_2) {
  VagCmd* new_cmd;
  LoadStackEntry* pLVar5;
  VagCmd* pVVar7;
  VagCmd* new_stereo_cmd;

  // we added a few null checks on vag_dir_entry.
  // as far as I can tell, it's "okay" that some animations (like Jak's idle animation)
  // don't have a corresponding audio. There's checks later on in this function that skip
  // trying to open the VAG file when this happens, but the stereo/mono checks don't have this
  // check. So we added them.

  if (param_2 == 1) {
    // CpuSuspendIntr(local_20);
  }
  new_stereo_cmd = nullptr;

  // allocate/find a vag cmd to hold this stream command. We don't own the incoming command.
  if ((cmd->id == 0) ||
      (((cmd->vag_dir_entry && cmd->vag_dir_entry->flag & 1U) != 0 &&  // added null check
        (HowManyBelowThisPriority(cmd->priority, 0) < 2))))
    goto LAB_000049dc;
  new_cmd = FindThisVagStream(cmd->name, cmd->id);
  if (!new_cmd) {
    new_cmd = SmartAllocVagCmd(cmd);
    if (!new_cmd)
      goto LAB_000049dc;

    if ((*(u32*)&new_cmd->status_bytes[BYTE4] & 0xffff00) != 0) {
      IsoStopVagStream(new_cmd, 0);
    }

    // copy data from the other command to us.
    new_cmd->header = cmd->header;

    new_cmd->file_record = cmd->file_record;
    new_cmd->vag_dir_entry = cmd->vag_dir_entry;
    strncpy(new_cmd->name, cmd->name, 0x30);
    new_cmd->unk_196 = cmd->unk_196;
    new_cmd->num_processed_chunks = cmd->num_processed_chunks;
    new_cmd->xfer_size = cmd->xfer_size;
    new_cmd->sample_rate = cmd->sample_rate;
    new_cmd->unk_260 = cmd->unk_260;
    new_cmd->unk_264 = cmd->unk_264;
    new_cmd->unk_268 = cmd->unk_268;
    new_cmd->vol_multiplier = cmd->vol_multiplier;
    new_cmd->unk_256_pitch2 = cmd->unk_256_pitch2;
    new_cmd->id = cmd->id;

    new_cmd->plugin_id = cmd->plugin_id;
    new_cmd->unk_136 = cmd->unk_136;
    new_cmd->unk_176 = cmd->unk_176;
    new_cmd->unk_288 = cmd->unk_288;
    new_cmd->unk_292 = cmd->unk_292;
    new_cmd->unk_296 = cmd->unk_296;
    new_cmd->vec3.x = cmd->vec3.x;
    new_cmd->vec3.y = cmd->vec3.y;
    new_cmd->vec3.z = cmd->vec3.z;
    InitVAGCmd(new_cmd, 1);
    new_cmd->sb_scanned = true;

    // check for stereo
    if (cmd->vag_dir_entry && (new_cmd->vag_dir_entry->flag & 1U) != 0) {  // added null check
      // try allocating it
      new_stereo_cmd = SmartAllocVagCmd(cmd);
      if (!new_stereo_cmd) {
        // no room for stereo, give up
        new_cmd->sb_scanned = false;
        ReleaseMessage(&new_cmd->header, 0);
        RemoveVagCmd(new_cmd, 0);
        FreeVagCmd(new_cmd, 0);
        new_cmd = nullptr;
      } else {
        // set up stereo command.
        // if ((*(u32*)&new_stereo_cmd->status_bytes[BYTE4] & 0xffff00) != 0) {
        if (new_stereo_cmd->byte5 || new_stereo_cmd->byte6) {
          IsoStopVagStream(new_stereo_cmd, 0);
        }

        // ADDED this line: seems random if this is set or not and other code relies on it not
        // being set. I don't understand why this doesn't happen on the real game, but it could just
        // be dma timing differences.
        new_stereo_cmd->byte21 = 0;

        new_stereo_cmd->status_bytes[BYTE11] = true;
        new_cmd->stereo_sibling = new_stereo_cmd;
        new_stereo_cmd->stereo_sibling = new_cmd;
        strncpy(new_stereo_cmd->name, "Stereo", 0x30);
        new_stereo_cmd->id = ~new_cmd->id;
        new_stereo_cmd->sb_scanned = true;
        new_stereo_cmd->vag_dir_entry = new_cmd->vag_dir_entry;
      }
    }
    if (new_cmd == nullptr)
      goto LAB_000049dc;

    // queue the command.
    if (QueueMessage(&new_cmd->header, 3, "QueueVAGStream", 0) == 0) {
      // queue failed, give up.
      new_cmd->sb_scanned = false;
      RemoveVagCmd(new_cmd, 0);
      FreeVagCmd(new_cmd, 0);
      if (cmd->vag_dir_entry && (new_cmd->vag_dir_entry->flag & 1U) != 0) {  // added null check
        new_stereo_cmd->sb_scanned = false;
        RemoveVagCmd(new_stereo_cmd, 0);
        FreeVagCmd(new_stereo_cmd, 0);
      }
      ReleaseMessage(&new_cmd->header, 0);
    } else {
      // queue succeeded! open the file
      if (new_cmd->vag_dir_entry == nullptr) {
        (new_cmd->header).lse = nullptr;
      } else {
        pLVar5 = (isofs->open_wad)(new_cmd->file_record, new_cmd->vag_dir_entry->offset);
        (new_cmd->header).lse = pLVar5;
      }
      if (cmd->unk_288 != 0) {
        new_cmd->status_bytes[BYTE10] = true;
      }
      if (cmd->unk_292 != 0) {
        new_cmd->unk_232 = true;
      }
      new_cmd->sb_paused = true;

      // set up name/priority
      SetNewVagCmdPri(new_cmd, cmd->priority, 0);
      if (new_stereo_cmd != nullptr) {
        new_stereo_cmd->sb_scanned = true;
        new_stereo_cmd->sb_paused = true;
        SetNewVagCmdPri(new_stereo_cmd, 10, 0);
      }
      SetVagStreamName(new_cmd, 0x30, 0);
      if (new_stereo_cmd != nullptr) {
        SetVagStreamName(new_stereo_cmd, 0x30, 0);
      }

      // set up buffer
      (new_cmd->header).status = -1;
      (new_cmd->header).callback = ProcessVAGData;
    }
    if (new_cmd == nullptr)
      goto LAB_000049dc;
  }
  pLVar5 = (new_cmd->header).lse;
  pVVar7 = new_cmd->stereo_sibling;
  new_cmd->unk_188 = 0;
  new_cmd->unk_180 = 0;
  new_cmd->unk_184 = 0;
  new_cmd->unk_192 = 0;

  if (pLVar5 == nullptr) {
    new_cmd->status_bytes[BYTE6] = true;
  } else {
    new_cmd->status_bytes[BYTE5] = true;
    if (pVVar7 != nullptr) {
      pVVar7->status_bytes[BYTE5] = true;
    }
  }
LAB_000049dc:
  if (param_2 == 1) {
    // CpuResumeIntr(local_20[0]);
  }
}

void IsoPlayVagStream(VagCmd* param_1, int param_2) {
  VagCmd* iVar1;
  LoadStackEntry* pLVar1;
  VagCmd* pVVar1;
  VagCmd* pRVar2;

  if (param_2 == 1) {
    // CpuSuspendIntr(local_20);
  }
  pRVar2 = param_1->stereo_sibling;
  iVar1 = FindThisVagStream(param_1->name, param_1->id);
  if (iVar1 != nullptr) {
    if (iVar1->status_bytes[BYTE4] == false) {
      iVar1->vol_multiplier = param_1->vol_multiplier;
      if (iVar1->sb_paused != false) {
        if (ext_pause != 0) {
          ext_resume = 1;
        }
        if (iVar1->sb_playing == false) {
          iVar1->sb_paused = false;
          if (pRVar2 != nullptr) {
            pRVar2->sb_paused = false;
          }
        } else {
          UnPauseVAG(iVar1, 0);
        }
        if ((u32)param_1->priority < 3) {
          SetNewVagCmdPri(param_1, 7, 0);
        }
      }
      iVar1->status_bytes[BYTE4] = true;
      if (pRVar2 != nullptr) {
        pRVar2->status_bytes[BYTE4] = true;
      }
    } else {
      iVar1 = nullptr;
    }
    if (iVar1 != nullptr) {
      pLVar1 = (iVar1->header).lse;
      pVVar1 = iVar1->stereo_sibling;
      iVar1->unk_188 = 0;
      iVar1->unk_180 = 0;
      iVar1->unk_184 = 0;
      iVar1->unk_192 = 0;

      if (pLVar1 == nullptr) {
        iVar1->status_bytes[BYTE6] = true;
      } else {
        iVar1->status_bytes[BYTE5] = true;
        if (pVVar1 != nullptr) {
          pVVar1->status_bytes[BYTE5] = true;
        }
      }
    }
  }
  if (param_2 == 1) {
    // CpuResumeIntr(local_20[0]);
  }
}

u32 _not_on_stack_sync = 0;
u32 IsoThreadCounter = 0;

u32 ISOThread() {
  bool bVar1;
  CmdLoadSingleIop* inasdf;
  Buffer* pBVar3;
  int iVar4;
  LoadStackEntry* pLVar5;
  Page* pages;
  VagCmd* pRVar7;
  Buffer* pBVar8;
  Buffer* pBVar9;
  VagCmd* pRVar10;
  int iVar11;
  FileRecord* pFVar12;
  VagStrListNode* pLVar14;
  CmdLoadSingleIop* local_30;

  InitBuffers();
  InitVagCmds();
  InitSpuStreamsThread();

  pBVar3 = AllocateBuffer(1, nullptr, 1);
  iVar4 = (isofs->init)(/*pBVar3->unk_12*/);
  if (iVar4 == 0) {
    iso_init_flag = 0;
  }
  SendMbx(sync_mbx, &_not_on_stack_sync);
  FreeBuffer(pBVar3, 1);

  do {
    spu_dma_hack();

    IsoThreadCounter = IsoThreadCounter + 1;
    // iVar4 = PollMbx(&local_30, iso_mbx);
    iVar4 = PollMbx((MsgPacket**)(&local_30), iso_mbx);
    inasdf = local_30;
    auto* in_dgo = (CmdDgo*)local_30;
    auto* in_sbk = (CmdLoadSoundBank*)local_30;
    auto* in_music = (CmdLoadMusic*)local_30;
    auto* in_vag = (VagCmd*)local_30;

    if (iVar4 == 0) {
      iVar4 = (local_30->header).cmd_kind;
      (local_30->header).callback_buffer = (Buffer*)nullptr;
      (local_30->header).ready_for_data = 1;
      (local_30->header).callback = NullCallback;
      (local_30->header).lse = (LoadStackEntry*)nullptr;
      if (iVar4 - 0x100U < 3) {
        iVar4 = QueueMessage(&local_30->header, 2, "LoadSingle", 1);
        if (iVar4 != 0) {
          if ((inasdf->header).cmd_kind == 0x102) {
            pFVar12 = inasdf->file_record;
            iVar4 = inasdf->offset;
          } else {
            iVar4 = -1;
            pFVar12 = inasdf->file_record;
          }
          pLVar5 = (isofs->open)(pFVar12, iVar4, OpenMode::KNOWN_NOT_BLZO);
          (inasdf->header).lse = pLVar5;
          if ((inasdf->header).lse == (LoadStackEntry*)nullptr) {
            (inasdf->header).status = 6;
            UnqueueMessage(&inasdf->header, 1);
          LAB_00005120:
            ReturnMessage(&inasdf->header);
          } else {
            inasdf->unk_64 = 0;
            inasdf->ptr = inasdf->dest_addr;
            iVar4 = (isofs->get_length)(inasdf->file_record);
            inasdf->length_to_copy = iVar4;
            if (iVar4 == 0) {
              inasdf->length_to_copy = inasdf->length;
            } else if (inasdf->length < iVar4) {
              inasdf->length_to_copy = inasdf->length;
            }
            iVar4 = (inasdf->header).cmd_kind;
            if (iVar4 == 0x101) {
              (inasdf->header).callback = CopyDataToIOP;
            } else {
              if (iVar4 < 0x102) {
                if (iVar4 != 0x100) {
                  (inasdf->header).status = -1;
                  goto LAB_00005144;
                }
              } else if (iVar4 != 0x102)
                goto LAB_00004e50;
              (inasdf->header).callback = CopyDataToEE;
            }
          LAB_00004e50:
            (inasdf->header).status = -1;
          }
        }
      } else if (iVar4 == 0x200) {
        iVar4 = QueueMessage(&local_30->header, 0, "LoadDGO", 1);
        if (iVar4 != 0) {
          pLVar5 = (LoadStackEntry*)(isofs->open)(in_dgo->fr, -1, OpenMode::MODE1);
          (in_dgo->header).lse = pLVar5;
          if (pLVar5 != (LoadStackEntry*)nullptr) {
            in_dgo->dgo_state = DgoState::Init;
            (in_dgo->header).callback = RunDGOStateMachine;
            goto LAB_00004e50;
          }
          UnqueueMessage(&in_dgo->header, 1);
          SendMbx(iso_mbx, &sLoadDgo);
        }
      } else {
        if (iVar4 == 0x300) {
          gSoundInUse = gSoundInUse + 1;
          if (gSoundEnable != 0) {
            pages = (Page*)AllocPages(SpMemoryBuffers, 1);
            if (pages == (Page*)nullptr) {
            LAB_00004f28:
              SendMbx(iso_mbx, inasdf);
            } else {
              SetBufferMem(pages->buffer, SpMemoryBuffers->page_size);
              // iVar4 = inasdf->maybe_offset;
              // pcVar6 = (code*)isofs->load_sound_bank;
              iVar4 = isofs->load_sound_bank(in_sbk->bank_name, in_sbk->bank);
            LAB_00004f6c:
              // iVar4 = (*pcVar6)(&inasdf->file_record, iVar4);
              (inasdf->header).status = iVar4;
              ReleaseBufferMem();
              FreePagesList(SpMemoryBuffers, pages);
            }
          }
        } else {
          if (iVar4 != 0x380) {
            if (iVar4 == 0x403) {
              if (ext_pause == 0) {
                SetVagStreamsNoStart(1, 1);
                iVar4 = AnyVagRunning();
                if (iVar4 != 0) {
                  PauseVagStreams();
                }
                ext_resume = (u32)(iVar4 != 0);
                ext_pause = 1;
              }
            } else if (iVar4 == 0x404) {
              if (ext_pause != 0) {
                if (ext_resume != 0) {
                  UnPauseVagStreams();  // 0?
                }
                ext_pause = 0;
                ext_resume = 0;
              }
              SetVagStreamsNoStart(0, 1);
            } else if (iVar4 == 0x405) {
              pRVar7 = FindVagStreamId(in_vag->id);
              if (pRVar7 != nullptr) {
                pRVar7->vol_multiplier = in_vag->vol_multiplier;
                SetVAGVol(pRVar7, 1);
              }
            } else if (iVar4 == 0x406) {
              pRVar7 = FindVagStreamId(in_vag->id);
              if (pRVar7 != nullptr) {
                pRVar7->unk_256_pitch2 = in_vag->unk_256_pitch2;
                SetVAGVol(pRVar7, 1);
              }
            } else if (iVar4 == 0x407) {
              MasterVolume[2] = in_vag->vol_multiplier;
              SetAllVagsVol(-1);
            } else {
              if ((local_30->header).cmd_kind == 0x666) {
                ReturnMessage(&local_30->header);
                ASSERT_NOT_REACHED();
              }
              (local_30->header).status = 4;
            }
            goto LAB_00005120;
          }
          gSoundInUse = gSoundInUse + 1;
          if (gSoundEnable != 0) {
            pages = (Page*)AllocPages(SpMemoryBuffers, 1);
            if (pages == (Page*)nullptr)
              goto LAB_00004f28;
            SetBufferMem(pages->buffer, SpMemoryBuffers->page_size);
            iVar4 = inasdf->offset;
            // pcVar6 = (code*)isofs->load_music;
            iVar4 = isofs->load_music(in_music->name, in_music->handle);
            goto LAB_00004f6c;
          }
        }
        gSoundInUse = gSoundInUse + -1;
        ReturnMessage(&inasdf->header);
      }
    } else if (iVar4 == -0x1a9) {
      return 0;
    }

  LAB_00005144:
    pBVar3 = (Buffer*)nullptr;
    pRVar7 = (VagCmd*)GetMessage();

    if (pRVar7 == nullptr) {
    LAB_00005208:;
      // (isofs->poll_drive)();
    } else {
      u32 uVar13 = 1;
      if ((pRVar7->header).callback == ProcessVAGData) {
        if (pRVar7->xfer_size != 0) {
        LAB_000051ac:
          uVar13 = 2;
          goto LAB_000051b0;
        }
        // no idea what happens here.. i think this is a bug and the callback_buffer == 0 should
        // flip.
        if (((pRVar7->header).callback_buffer == (Buffer*)nullptr) &&
            (uVar13 = 2, /*DAT_00000008 == 0*/ true))
          goto LAB_000051b0;
        if (pRVar7->xfer_size != 0)
          goto LAB_000051ac;
      } else {
      LAB_000051b0:
        pBVar3 = AllocateBuffer(uVar13, pRVar7, 1);
      }
      if (pBVar3 == (Buffer*)nullptr) {
      LAB_000051fc:
        pRVar7 = nullptr;
      } else {
        iVar4 = (isofs->page_begin_read)((pRVar7->header).lse, pBVar3);
        (pRVar7->header).status = iVar4;
        if (iVar4 != -1) {
          FreeBuffer(pBVar3, 1);
          pBVar3 = (Buffer*)nullptr;
          goto LAB_000051fc;
        }
      }
      if (pRVar7 == nullptr)
        goto LAB_00005208;
    }
    ProcessMessageData();
    if (pBVar3 != (Buffer*)nullptr) {
      iVar4 = (isofs->sync_read)(/*pBVar3*/);
      if (iVar4 == 8) {
        FreeBuffer(pBVar3, 1);
        pBVar3 = (Buffer*)nullptr;
      } else {
        (pRVar7->header).status = iVar4;
        pBVar3->decomp_buffer = (uint8_t*)pBVar3->unk_12;
        pBVar8 = (pRVar7->header).callback_buffer;
        if (pBVar8 == (Buffer*)nullptr) {
          (pRVar7->header).callback_buffer = pBVar3;
        } else {
          pBVar9 = pBVar8->next;
          while (pBVar9 != (Buffer*)nullptr) {
            pBVar8 = pBVar8->next;
            pBVar9 = pBVar8->next;
          }
          pBVar8->next = pBVar3;
        }
        pBVar3 = (Buffer*)nullptr;
      }
    }
    WaitSema(RequestedStreamsList.sema);
    if (RequestedStreamsList.unk2_init0 == 1) {
      QueueNewStreamsFromList(&RequestedStreamsList);
      iVar4 = 0;
      pLVar14 = (VagStrListNode*)NewStreamsList.next;
      do {
        if (pLVar14->id != 0) {
          QueueVAGStream(pLVar14);
        }
        pLVar14 = (VagStrListNode*)pLVar14->list.next;
        iVar4 = iVar4 + 1;
      } while (iVar4 < 4);
    }
    pRVar7 = VagCmds;
    iVar4 = 0;
    // pcVar15 = VagCmds[0].name;
    auto* cmd_iter = VagCmds;
    do {
      if ((((cmd_iter->byte11 == false) && (cmd_iter->sb_scanned == false)) &&
           (cmd_iter->id != 0)) ||
          ((StopPluginStreams == 1 && (cmd_iter->unk_136) != 0))) {
        // CpuSuspendIntr(&local_2c);
        bVar1 = false;
        if (cmd_iter->id == 0) {
          if (cmd_iter->name[0] != false) {
            while (pRVar10 = FindVagStreamName(pRVar7->name), pRVar10 != nullptr) {
              TerminateVAG(pRVar10, 0);
              bVar1 = true;
            }
          }
        } else {
          pRVar10 = FindThisVagStream(cmd_iter->name, cmd_iter->id);
          if (pRVar10 != nullptr) {
            TerminateVAG(pRVar10, 0);
            bVar1 = true;
          }
        }
        if ((bVar1) && (iVar11 = AnyVagRunning(), iVar11 == 0)) {
          ext_pause = 0;
          ext_resume = 0;
        }
        // CpuResumeIntr(local_2c);
      }
      // pcVar15 = pcVar15 + 0x144;
      cmd_iter++;
      iVar4 = iVar4 + 1;
      pRVar7 = pRVar7 + 1;
    } while (iVar4 < 4);
    SignalSema(RequestedStreamsList.sema);
    RequestedStreamsList.unk2_init0 = 0;
    if (pBVar3 == (Buffer*)nullptr) {
      bool should_sleep = true;
      if (PeekMbx(iso_mbx)) {
        should_sleep = false;
      }
      auto* msg = GetMessage();
      if (msg && msg->callback != ProcessVAGData) {
        should_sleep = false;
      }

      if (should_sleep) {
        DelayThread(1000);
      }
    }
  } while (true);
}

int RunDGOStateMachine(CmdHeader* param_1_in, Buffer* param_2) {
  auto* param_1 = (CmdDgo*)param_1_in;
  uint8_t* puVar1;
  size_t bytes_to_read;
  size_t sVar2;
  int iVar3;
  int iVar4;
  size_t sVar5;
  size_t bytes_left;
  uint8_t* unprocessed_data;
  int return_value;

  return_value = -1;
  bytes_left = param_2->decompressed_size;
  unprocessed_data = param_2->decomp_buffer;
  do {
    if (bytes_left == 0)
      goto LAB_000059b4;
    switch (param_1->dgo_state) {
      case DgoState::Init:
        param_1->bytes_processed = 0;
        param_1->dgo_state = DgoState::Read_Header;
        param_1->finished_first_object = 0;
        param_1->want_abort = 0;
        break;
      case DgoState::Read_Header:
        bytes_to_read = 0x40 - param_1->bytes_processed;
        if ((int)bytes_left < (int)bytes_to_read) {
          bytes_to_read = bytes_left;
        }
        for (; bytes_to_read != 0; bytes_to_read = bytes_to_read - sVar5) {
          sVar2 = (param_2->page->ptr - unprocessed_data) + 1;
          sVar5 = bytes_to_read;
          if ((int)sVar2 < (int)bytes_to_read) {
            sVar5 = sVar2;
          }
          bytes_left = bytes_left - sVar5;
          memcpy((param_1->dgo_header).name + param_1->bytes_processed + -4, unprocessed_data,
                 sVar5);
          param_2->decomp_buffer = param_2->decomp_buffer + sVar5;
          param_2->decompressed_size = param_2->decompressed_size - sVar5;
          unprocessed_data = (uint8_t*)CheckForIsoPageBoundaryCrossing(param_2);
          param_1->bytes_processed = param_1->bytes_processed + sVar5;
        }
        if (param_1->bytes_processed == 0x40) {
          iVar4 = (param_1->dgo_header).object_count;
          param_1->bytes_processed = 0;
          param_1->objects_loaded = 0;
          if (iVar4 == 1) {
          LAB_00005990:
            puVar1 = param_1->buffer_heaptop;
            param_1->buffer_toggle = 0;
          } else {
            puVar1 = param_1->buffer1;
            param_1->buffer_toggle = 1;
          }
          param_1->dgo_state = DgoState::Read_Obj_Header;
          param_1->ee_dest_buffer = puVar1;
        }
        break;
      case DgoState::Finish_Obj:
        if (param_1->finished_first_object == 0) {
        LAB_00005870:
          if (param_1->buffer1 == param_1->buffer2)
            goto LAB_000058c4;
          iVar4 = param_1->buffer_toggle;
          (param_1->header).status = -1;
          if (iVar4 == 1) {
            puVar1 = param_1->buffer1;
          } else {
            puVar1 = param_1->buffer2;
          }
          param_1->selected_buffer = puVar1;
          ReturnMessage(&param_1->header);
          if (param_1->buffer1 == param_1->buffer2)
            goto LAB_000058c4;
        } else {
          if (param_1->buffer1 != param_1->buffer2) {
            if (LookMbx(sync_mbx)) {
              if (param_1->want_abort != 0)
                goto LAB_00005988;
              goto LAB_00005870;
            }
            goto LAB_000059b4;
          }
        LAB_000058c4:
          if (param_1->objects_loaded + 1 < (param_1->dgo_header).object_count) {
            if (!LookMbx(sync_mbx))
              goto LAB_000059b4;
            if (param_1->want_abort != 0)
              goto LAB_00005988;
          }
        }
        param_1->finished_first_object = 1;
        if (param_1->buffer_toggle == 1) {
          param_1->buffer_toggle = 2;
          param_1->ee_dest_buffer = param_1->buffer2;
        } else {
          param_1->buffer_toggle = 1;
          param_1->ee_dest_buffer = param_1->buffer1;
        }
        if (param_1->objects_loaded + 1 == (param_1->dgo_header).object_count) {
          param_1->dgo_state = DgoState::Read_Last_Obj;
        } else {
          param_1->dgo_state = DgoState::Read_Obj_Header;
        }
        break;
      case DgoState::Read_Last_Obj:
        if (!LookMbx(sync_mbx))
          goto LAB_000059b4;
        if (param_1->want_abort == 0)
          goto LAB_00005990;
      LAB_00005988:
        param_1->dgo_state = DgoState::Finish_Dgo;
        break;
      case DgoState::Read_Obj_Header:
        bytes_to_read = 0x40 - param_1->bytes_processed;
        if ((int)bytes_left < (int)bytes_to_read) {
          bytes_to_read = bytes_left;
        }
        for (; bytes_to_read != 0; bytes_to_read = bytes_to_read - sVar5) {
          sVar2 = (param_2->page->ptr - unprocessed_data) + 1;
          sVar5 = bytes_to_read;
          if ((int)sVar2 < (int)bytes_to_read) {
            sVar5 = sVar2;
          }
          bytes_left = bytes_left - sVar5;
          memcpy((param_1->obj_header).name + param_1->bytes_processed + -4, unprocessed_data,
                 sVar5);
          param_2->decomp_buffer = param_2->decomp_buffer + sVar5;
          param_2->decompressed_size = param_2->decompressed_size - sVar5;
          unprocessed_data = (uint8_t*)CheckForIsoPageBoundaryCrossing(param_2);
          param_1->bytes_processed = param_1->bytes_processed + sVar5;
        }
        if (param_1->bytes_processed == 0x40) {
          DMA_SendToEE(&param_1->obj_header, 0x40, param_1->ee_dest_buffer);
          param_1->dgo_state = DgoState::Read_Obj_data;
          iVar4 = (param_1->obj_header).size;
          param_1->bytes_processed = 0;
          param_1->ee_dest_buffer = param_1->ee_dest_buffer + 0x40;
          (param_1->obj_header).size = iVar4 + 0xfU & 0xfffffff0;
        }
        break;
      case DgoState::Read_Obj_data:
        bytes_to_read = (param_1->obj_header).size - param_1->bytes_processed;
        if ((int)bytes_left < (int)bytes_to_read) {
          bytes_to_read = bytes_left;
        }
        for (; bytes_to_read != 0; bytes_to_read = bytes_to_read - sVar5) {
          sVar2 = (param_2->page->ptr - unprocessed_data) + 1;
          sVar5 = bytes_to_read;
          if ((int)sVar2 < (int)bytes_to_read) {
            sVar5 = sVar2;
          }
          bytes_left = bytes_left - sVar5;
          DMA_SendToEE(unprocessed_data, sVar5, param_1->ee_dest_buffer);
          param_2->decomp_buffer = param_2->decomp_buffer + sVar5;
          param_2->decompressed_size = param_2->decompressed_size - sVar5;
          unprocessed_data = (uint8_t*)CheckForIsoPageBoundaryCrossing(param_2);
          param_1->ee_dest_buffer = param_1->ee_dest_buffer + sVar5;
          param_1->bytes_processed = param_1->bytes_processed + sVar5;
        }
        if (param_1->bytes_processed == (param_1->obj_header).size) {
          iVar3 = (param_1->dgo_header).object_count;
          iVar4 = param_1->objects_loaded + 1;
          param_1->objects_loaded = iVar4;
          if (iVar4 < iVar3) {
            DgoState nstate = DgoState::Finish_Obj_NoDoubleBuffer;
            if (param_1->buffer1 != param_1->buffer2) {
              nstate = DgoState::Finish_Obj;
            }
            param_1->dgo_state = nstate;
            param_1->bytes_processed = 0;
          } else {
            param_1->dgo_state = DgoState::Finish_Dgo;
            return_value = 0;
          }
        }
        break;
      case DgoState::Finish_Dgo:
        return_value = 0;
      LAB_000059b4:
        if ((return_value == 0) || (bytes_left == 0)) {
          param_2->decomp_buffer = (uint8_t*)nullptr;
          param_2->decompressed_size = 0;
        } else {
          param_2->decomp_buffer = unprocessed_data;
          param_2->decompressed_size = bytes_left;
        }
        return return_value;
      case DgoState::Finish_Obj_NoDoubleBuffer:
        iVar4 = param_1->buffer_toggle;
        (param_1->header).status = -1;
        if (iVar4 == 1) {
          puVar1 = param_1->buffer1;
        } else {
          puVar1 = param_1->buffer2;
        }
        param_1->selected_buffer = puVar1;
        ReturnMessage(&param_1->header);
        param_1->dgo_state = DgoState::Finish_Obj;
    }
  } while (true);
}

void LoadDGO(RPC_Dgo_Cmd* param_1) {
  FileRecord* iVar1;
  iVar1 = (isofs->find)(param_1->name);
  if (iVar1 == 0) {
    printf("overlord couldn't find dgo: %s\n", param_1->name);
    param_1->result = 1;
  } else {
    CancelDGO(0);
    sLoadDgo.header.cmd_kind = 0x200;
    sLoadDgo.header.thread_id = 0;
    sLoadDgo.header.mbx_to_reply = dgo_mbx;
    sLoadDgo.buffer1 = (uint8_t*)(u64)param_1->buffer1;
    sLoadDgo.buffer2 = (uint8_t*)(u64)param_1->buffer2;
    sLoadDgo.buffer_heaptop = (uint8_t*)(u64)param_1->buffer_heap_top;
    sLoadDgo.fr = iVar1;
    SendMbx(iso_mbx, &sLoadDgo);

    // wait for the ReturnMessage in the DGO callback state machine.
    // this happens when the first file is loaded
    WaitMbx(dgo_mbx);

    if (sLoadDgo.header.status == -1) {
      param_1->result = 2;
    } else {
      if (sLoadDgo.header.status == 0) {
        param_1->result = 0;
        param_1->buffer1 = param_1->buffer_heap_top;
      } else {
        param_1->result = 1;
      }
      sLoadDgo.header.cmd_kind = 0;
    }
  }
}

int CopyData(CmdLoadSingleIop* param_1, Buffer* param_2, int param_3) {
  size_t sVar1;
  Page* pPVar2;
  uint8_t* src;
  size_t n;

  pPVar2 = param_2->page;
  if (param_2->decompressed_size != 0) {
    n = param_1->length_to_copy - param_1->unk_64;
    if (pPVar2 != (Page*)nullptr) {
      while (0 < (int)n) {
        n = param_1->length_to_copy - param_1->unk_64;
        if (param_2->decompressed_size < (int)n) {
          n = param_2->decompressed_size;
        }
        src = param_2->decomp_buffer;
        sVar1 = (pPVar2->ptr - src) + 1;
        if ((int)sVar1 < (int)n) {
          n = sVar1;
        }
        if (param_3 == 0) {
          DMA_SendToEE(src, n, param_1->ptr);
        } else if (param_3 == 1) {
          memcpy(param_1->ptr, src, n);
        }
        param_1->ptr = param_1->ptr + n;
        param_1->unk_64 = param_1->unk_64 + n;
        param_2->decomp_buffer = param_2->decomp_buffer + n;
        param_2->decompressed_size = param_2->decompressed_size - n;
        CheckForIsoPageBoundaryCrossing(param_2);
        pPVar2 = param_2->page;
        if ((u32)param_1->length_to_copy <= (u32)param_1->unk_64) {
          if (pPVar2 != (Page*)nullptr) {
            pPVar2->state = PageState::SIX;
            pPVar2 = (Page*)StepTopPage(param_2->plist, pPVar2);
            param_2->page = pPVar2;
            param_2->free_pages = param_2->free_pages + -1;
          }
          break;
        }
        if (pPVar2 == (Page*)nullptr)
          break;
      }
    }
    if ((u32)param_1->length_to_copy <= (u32)param_1->unk_64) {
      return 0;
    }
  }
  return -1;
}

/*!
 * Find a file by name.  Return nullptr if it fails.
 */
FileRecord* FindISOFile(const char* name) {
  return isofs->find(name);
}

// FindVAGFile in common

u32 GetISOFileLength(FileRecord* fr) {
  return isofs->get_length(fr);
}

int NullCallback(CmdHeader* cmd, Buffer* buff) {
  (void)cmd;
  buff->decompressed_size = 0;
  return CMD_STATUS_NULL_CB;
}

void IsoStopVagStream(VagCmd* param_1, int param_2) {
  bool bVar1;
  VagCmd* pRVar2;
  int iVar3;

  if (param_2 == 1) {
    // CpuSuspendIntr(local_18);
  }
  bVar1 = false;
  if (param_1->id == 0) {
    if (param_1->name[0] != false) {
      while (pRVar2 = FindVagStreamName(param_1->name), pRVar2 != nullptr) {
        TerminateVAG(pRVar2, 0);
        bVar1 = true;
      }
    }
  } else {
    pRVar2 = FindThisVagStream(param_1->name, param_1->id);
    if (pRVar2 != nullptr) {
      TerminateVAG(pRVar2, 0);
      bVar1 = true;
    }
  }
  if ((bVar1) && (iVar3 = AnyVagRunning(), iVar3 == 0)) {
    ext_pause = 0;
    ext_resume = 0;
  }
  if (param_2 == 1) {
    // CpuResumeIntr(local_18[0]);
  }
}

int CopyDataToIOP(CmdHeader* param_1, Buffer* param_2) {
  return CopyData((CmdLoadSingleIop*)param_1, param_2, 1);
}

int CopyDataToEE(CmdHeader* param_1, Buffer* param_2) {
  return CopyData((CmdLoadSingleIop*)param_1, param_2, 0);
}

void* RPC_DGO(unsigned int fno, void* _cmd, int y);
void LoadNextDGO(RPC_Dgo_Cmd* cmd);

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

void* RPC_DGO(unsigned int param_1, void* param_2, int) {
  if (param_1 == 0) {
    LoadDGO((RPC_Dgo_Cmd*)param_2);
  } else if (param_1 == 1) {
    LoadNextDGO((RPC_Dgo_Cmd*)param_2);
  } else if (param_1 == 2) {
    CancelDGO((RPC_Dgo_Cmd*)param_2);
  } else {
    ((RPC_Dgo_Cmd*)param_2)->result = 1;
  }
  return param_2;
}

void LoadNextDGO(RPC_Dgo_Cmd* param_1) {
  if (sLoadDgo.header.cmd_kind == 0) {
    param_1->result = 1;
  } else {
    sLoadDgo.buffer_heaptop = (uint8_t*)(u64)param_1->buffer_heap_top;
    sLoadDgo.buffer1 = (uint8_t*)(u64)param_1->buffer1;
    sLoadDgo.buffer2 = (uint8_t*)(u64)param_1->buffer2;
    SendMbx(sync_mbx, nullptr /*&a*/);  // no idea why they put an address here..
    WaitMbx(dgo_mbx);

    if (sLoadDgo.header.status == -1) {
      param_1->result = 2;
      param_1->buffer1 = (u32)(u64)sLoadDgo.selected_buffer;
    } else {
      if (sLoadDgo.header.status == 0) {
        param_1->result = 0;
        param_1->buffer1 = param_1->buffer_heap_top;
      } else {
        param_1->result = 1;
      }
      sLoadDgo.header.cmd_kind = 0;
    }
  }
}

void CancelDGO(RPC_Dgo_Cmd* param_1) {
  if (sLoadDgo.header.cmd_kind != 0) {
    sLoadDgo.want_abort = 1;
    SendMbx(sync_mbx, nullptr);  // was some stack addr...
    WaitMbx(dgo_mbx);

    if (param_1 != (RPC_Dgo_Cmd*)nullptr) {
      param_1->result = 3;
    }
    sLoadDgo.header.cmd_kind = 0;
  }
}

void InitDriver() {
  int iVar1;

  iVar1 = (isofs->init)();
  if (iVar1 == 0) {
    iso_init_flag = 0;
  }
  SendMbx(sync_mbx, &_not_on_stack_sync);
}

void SetVagClock(VagCmd* param_1, int param_2) {
  LoadStackEntry* pLVar1;

  if (param_2 == 1) {
    // CpuSuspendIntr(local_18);
  }
  pLVar1 = (param_1->header).lse;
  param_1->unk_188 = 0;
  param_1->unk_180 = 0;
  param_1->unk_184 = 0;
  param_1->unk_192 = 0;
  if (pLVar1 == (LoadStackEntry*)nullptr) {
    param_1->byte6 = true;
  } else {
    param_1->byte5 = true;
    if (param_1->stereo_sibling != (VagCmd*)nullptr) {
      param_1->stereo_sibling->byte5 = true;
    }
  }
  if (param_2 == 1) {
    // CpuResumeIntr(local_18[0]);
  }
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

}  // namespace jak2
