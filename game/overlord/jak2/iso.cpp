#include "iso.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/common/iso.h"
#include "game/overlord/jak2/iso_cd.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/stream.h"
#include "game/overlord/jak2/vag.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {

u32 DGOThread();
FileRecord* FindISOFile(const char* name);
int LoadISOFileToIOP(FileRecord* fr, uint8_t* dest, int length);

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

void iso_init_globals() {
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
}

/// The main buffer used for reading data and doing blzo decompression.
LargeBuffer* SpLargeBuffer = nullptr;

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
    LoadISOFileToIOP(vagdir_file, (u8*)&gVagDir, VAG_DIR_FILE_SIZE[g_game_version]);
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
  int iVar1;
  VagCmd* puVar3;
  VagCmd* pRVar2;
  // VagCmd* pRVar3;
  LoadStackEntry* pLVar5;
  VagCmd* pVVar8;
  // VagCmd* pRVar6;
  VagCmd* pVVar7;
  int iVar7;
  int iVar8;
  VagCmd* pVVar11;

  if (param_2 == 1) {
    // CpuSuspendIntr(local_20);
  }
  pVVar11 = nullptr;
  if ((cmd->id == 0) || (((cmd->unk_44_ptr[3] & 1U) != 0 &&
                          (iVar1 = HowManyBelowThisPriority(cmd->priority, 0), iVar1 < 2))))
    goto LAB_000049dc;
  puVar3 = FindThisVagStream(cmd->name, cmd->id);
  if (!puVar3) {
    puVar3 = SmartAllocVagCmd(cmd);
    if (!puVar3)
      goto LAB_000049dc;
    if ((*(uint*)&puVar3->status_bytes[BYTE4] & 0xffff00) != 0) {
      IsoStopVagStream(puVar3, 0);
    }
    //    pRVar3 = cmd;
    //    pRVar6 = puVar3;
    //    do {
    //      pVVar8 = pRVar6;
    //      pRVar2 = pRVar3;
    //      iVar1 = (pRVar2->header).unk_4;
    //      iVar7 = (pRVar2->header).cmd_kind;
    //      iVar8 = (pRVar2->header).status;
    //      (pVVar8->header).unk_0 = (pRVar2->header).unk_0;
    //      (pVVar8->header).unk_4 = iVar1;
    //      (pVVar8->header).cmd_kind = iVar7;
    //      (pVVar8->header).status = iVar8;
    //      pRVar3 = (RealVagCmd*)&(pRVar2->header).mbx_to_reply;
    //      pRVar6 = (RealVagCmd*)&(pVVar8->header).mbx_to_reply;
    //    } while (pRVar3 != (RealVagCmd*)&(cmd->header).callback);
    //    *(int*)pRVar6 = *(int*)pRVar3;
    // iVar1 = (pRVar2->header).thread_id;
    //(pVVar8->header).thread_id = iVar1;
    puVar3->header.unk_0 = cmd->header.unk_0;
    puVar3->header.unk_4 = cmd->header.unk_4;
    puVar3->header.cmd_kind = cmd->header.cmd_kind;
    puVar3->header.status = cmd->header.status;

    puVar3->header.mbx_to_reply = cmd->header.mbx_to_reply;
    puVar3->header.thread_id = cmd->header.thread_id;
    puVar3->header.unk_24 = cmd->header.unk_24;
    puVar3->header.callback_buffer = cmd->header.callback_buffer;

    puVar3->header.callback = cmd->header.callback;
    puVar3->header.lse = cmd->header.lse;

    puVar3->unk_40 = cmd->unk_40;
    puVar3->unk_44_ptr = cmd->unk_44_ptr;
    strncpy(puVar3->name, cmd->name, 0x30);
    puVar3->unk_196 = cmd->unk_196;
    puVar3->unk_240_flag0 = cmd->unk_240_flag0;
    puVar3->xfer_size = cmd->xfer_size;
    puVar3->unk_248 = cmd->unk_248;
    puVar3->unk_260 = cmd->unk_260;
    puVar3->unk_264 = cmd->unk_264;
    puVar3->unk_268 = cmd->unk_268;
    puVar3->vol_multiplier = cmd->vol_multiplier;
    puVar3->unk_256_pitch2 = cmd->unk_256_pitch2;
    puVar3->id = cmd->id;
    puVar3->plugin_id = cmd->plugin_id;
    puVar3->unk_136 = cmd->unk_136;
    puVar3->unk_176 = cmd->unk_176;
    puVar3->unk_288 = cmd->unk_288;
    puVar3->unk_292 = cmd->unk_292;
    puVar3->unk_296 = cmd->unk_296;
    puVar3->vec3.x = cmd->vec3.x;
    puVar3->vec3.y = cmd->vec3.y;
    puVar3->vec3.z = cmd->vec3.z;
    InitVAGCmd(puVar3, 1);
    puVar3->status_bytes[BYTE7_SCANNED] = '\x01';
    if ((puVar3->unk_44_ptr[3] & 1U) != 0) {
      pVVar11 = SmartAllocVagCmd(cmd);
      if (!pVVar11) {
        puVar3->status_bytes[BYTE7_SCANNED] = '\0';
        ReleaseMessage(&puVar3->header, 0);
        RemoveVagCmd(puVar3, 0);
        FreeVagCmd(puVar3, 0);
        puVar3 = 0x0;
      } else {
        if ((*(uint*)&pVVar11->status_bytes[BYTE4] & 0xffff00) != 0) {
          IsoStopVagStream(pVVar11, 0);
        }
        pVVar11->status_bytes[BYTE11] = '\x01';
        puVar3->stereo_sibling = pVVar11;
        pVVar11->stereo_sibling = puVar3;
        strncpy(pVVar11->name, "Stereo", 0x30);
        pVVar11->id = ~puVar3->id;
        pVVar11->status_bytes[BYTE7_SCANNED] = '\x01';
        pVVar11->unk_44_ptr = puVar3->unk_44_ptr;
      }
    }
    if (puVar3 == 0x0)
      goto LAB_000049dc;
    iVar1 = QueueMessage(&puVar3->header, 3, "QueueVAGStream", 0);
    if (iVar1 == 0) {
      puVar3->status_bytes[BYTE7_SCANNED] = '\0';
      RemoveVagCmd(puVar3, 0);
      FreeVagCmd(puVar3, 0);
      if ((puVar3->unk_44_ptr[3] & 1U) != 0) {
        pVVar11->status_bytes[BYTE7_SCANNED] = '\0';
        RemoveVagCmd(pVVar11, 0);
        FreeVagCmd(pVVar11, 0);
      }
      ReleaseMessage(&puVar3->header, 0);
    } else {
      if (puVar3->unk_44_ptr == 0x0) {
        (puVar3->header).lse = (LoadStackEntry*)0x0;
      } else {
        pLVar5 = (isofs->open_wad)(puVar3->unk_40, puVar3->unk_44_ptr[2]);
        (puVar3->header).lse = pLVar5;
      }
      if (cmd->unk_288 != 0) {
        puVar3->status_bytes[BYTE10] = '\x01';
      }
      if (cmd->unk_292 != 0) {
        puVar3->unk_232 = '\x01';
      }
      puVar3->status_bytes[PAUSED] = '\x01';
      SetNewVagCmdPri(puVar3, cmd->priority, 0);
      if (pVVar11 != 0x0) {
        pVVar11->status_bytes[BYTE7_SCANNED] = '\x01';
        pVVar11->status_bytes[PAUSED] = '\x01';
        SetNewVagCmdPri(pVVar11, 10, 0);
      }
      SetVagStreamName(puVar3, 0x30, 0);
      if (pVVar11 != 0x0) {
        SetVagStreamName(pVVar11, 0x30, 0);
      }
      (puVar3->header).status = -1;
      (puVar3->header).callback = ProcessVAGData;
    }
    if (puVar3 == 0x0)
      goto LAB_000049dc;
  }
  pLVar5 = (puVar3->header).lse;
  pVVar7 = puVar3->stereo_sibling;
  puVar3->unk_188 = 0;
  puVar3->unk_180 = 0;
  puVar3->unk_184 = 0;
  puVar3->unk_192 = 0;
  if (pLVar5 == (LoadStackEntry*)0x0) {
    puVar3->status_bytes[BYTE6] = '\x01';
  } else {
    puVar3->status_bytes[BYTE5] = '\x01';
    if (pVVar7 != 0x0) {
      pVVar7->status_bytes[BYTE5] = '\x01';
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
  if (iVar1 != 0x0) {
    if (iVar1->status_bytes[BYTE4] == '\0') {
      iVar1->vol_multiplier = param_1->vol_multiplier;
      if (iVar1->status_bytes[PAUSED] != '\0') {
        if (ext_pause != 0) {
          ext_resume = 1;
        }
        if (iVar1->status_bytes[BYTE1] == '\0') {
          iVar1->status_bytes[PAUSED] = '\0';
          if (pRVar2 != 0x0) {
            pRVar2->status_bytes[PAUSED] = '\0';
          }
        } else {
          UnPauseVAG(iVar1, 0);
        }
        if ((uint)param_1->priority < 3) {
          SetNewVagCmdPri(param_1, 7, 0);
        }
      }
      iVar1->status_bytes[BYTE4] = '\x01';
      if (pRVar2 != 0x0) {
        pRVar2->status_bytes[BYTE4] = '\x01';
      }
    } else {
      iVar1 = 0x0;
    }
    if (iVar1 != 0x0) {
      pLVar1 = (iVar1->header).lse;
      pVVar1 = iVar1->stereo_sibling;
      iVar1->unk_188 = 0;
      iVar1->unk_180 = 0;
      iVar1->unk_184 = 0;
      iVar1->unk_192 = 0;
      if (pLVar1 == (LoadStackEntry*)0x0) {
        iVar1->status_bytes[BYTE6] = '\x01';
      } else {
        iVar1->status_bytes[BYTE5] = '\x01';
        if (pVVar1 != 0x0) {
          pVVar1->status_bytes[BYTE5] = '\x01';
        }
      }
    }
  }
  if (param_2 == 1) {
    // CpuResumeIntr(local_20[0]);
  }
}

// ISOThread
// Run DGOStateMAchine
// LoadDGO
// CopyData

/*!
 * Find a file by name.  Return nullptr if it fails.
 */
FileRecord* FindISOFile(const char* name) {
  return isofs->find(name);
}

// FindVAGFile
// GetISOFileLength

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
    if (param_1->name[0] != '\0') {
      while (pRVar2 = FindVagStreamName(param_1->name), pRVar2 != 0x0) {
        TerminateVAG(pRVar2, 0);
        bVar1 = true;
      }
    }
  } else {
    pRVar2 = FindThisVagStream(param_1->name, param_1->id);
    if (pRVar2 != 0x0) {
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

int LoadISOFileToIOP(FileRecord* fr, uint8_t* dest, int length) {
  CmdLoadSingleIop cmd;
  cmd.header.cmd_kind = LOAD_TO_IOP_CMD_ID;
  cmd.header.mbx_to_reply = 0;
  cmd.header.thread_id = GetThreadId();
  cmd.file_record = fr;
  cmd.dest_addr = dest;
  cmd.length = length;
  SendMbx(iso_mbx, &cmd);
  SleepThread();
  int result = 0;
  if (cmd.header.status == 0) {
    result = cmd.length_to_copy;
  }
  return result;
}

u32 DGOThread() {
  ASSERT_NOT_REACHED();
}
// TODO: IsoQueueVagStream
}  // namespace jak2
