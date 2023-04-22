#include "iso.h"

#include <cstdio>

#include "common/util/Assert.h"

#include "game/overlord/common/iso.h"
#include "game/overlord/jak2/iso_cd.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/stream.h"
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

void iso_init_globals() {
  iso_init_flag = 0;
  iso_mbx = 0;
  dgo_mbx = 0;
  sync_mbx = 0;
  iso_thread = 0;
  dgo_thread = 0;
  str_thread = 0;
  play_thread = 0;
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

// IsoQueueVagStream
// IsoPlayVagStream
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
