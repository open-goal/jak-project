/*!
 * @file kmemcard.cpp
 * Memory card interface. Very messy code.
 */

//#include "ps2/SCE_MC.h"
//#include "ps2/SCE_FS.h"
//#include "ps2/common_types.h"
//#include "kernel/kmachine.h"
#include "game/sce/sif_ee.h"
#include "kmemcard.h"
#include "game/kernel/kdgo.h"
#include "game/common/ramdisk_rpc_types.h"
#include "game/kernel/fileio.h"
#include <cstdio>
#include <cstring>

using McCallbackFunc = void (*)(s32);

McCallbackFunc callback;

static s32 next;
static s32 language;
static MemoryCardOperation op;
static MemoryCard mc[2];
static RPC_Ramdisk_LoadCmd ramdisk_cmd;

// these are the return value for sceMcGetInfo.
static s32 p1, p2, p3, p4;
using namespace ee;

void cb_reprobe_format(s32);
void cb_format_complete(s32);
void cb_unformat(s32);
void cb_reprobe_createfile(s32);
void cb_wait_for_ramdisk(s32);
void cb_wait_for_ramdisk_load(s32);
void cb_createfile_erasing(s32);
void cb_createdir(s32);
void cb_createdfile(s32);
void cb_writtenfile(s32);
void cb_closedfile(s32);
void cb_reprobe_save(s32);

const char* filename[12] = {
    "/BASCUS-97124AYBABTU!",           "/BASCUS-97124AYBABTU!/icon.sys",
    "/BASCUS-97124AYBABTU!/icon.ico",  "/BASCUS-97124AYBABTU!/BASCUS-97124AYBABTU!",
    "/BASCUS-97124AYBABTU!/bank0.bin", "/BASCUS-97124AYBABTU!/bank1.bin",
    "/BASCUS-97124AYBABTU!/bank2.bin", "/BASCUS-97124AYBABTU!/bank3.bin",
    "/BASCUS-97124AYBABTU!/bank4.bin", "/BASCUS-97124AYBABTU!/bank5.bin",
    "/BASCUS-97124AYBABTU!/bank6.bin", "/BASCUS-97124AYBABTU!/bank7.bin"};

void kmemcard_init_globals() {
  next = 0;
  language = 0;
  op = {};
  mc[0] = {};
  mc[1] = {};
  callback = nullptr;
  p1 = 0;
  p2 = 0;
  p3 = 0;
  p4 = 0;
}

/*!
 * Get a new memory card handle.
 * Will never return 0.
 */
s32 new_mc_handle() {
  s32 handle = next++;

  // if you wrap around, it avoid the zero handle.
  // it doesn't seem like you will need billions of memory card handles
  if (handle == 0) {
    handle = next++;
  }
  return handle;
}

/*!
 * A questionable checksum.
 */
u32 mc_checksum(Ptr<u8> data, s32 size) {
  if (size < 0) {
    size += 3;
  }

  u32 result = 0;
  u32* data_u32 = (u32*)data.c();
  for (s32 i = 0; i < size / 4; i++) {
    result = result << 1 ^ (s32)result >> 0x1f ^ data_u32[i] ^ 0x12345678;
  }

  return result ^ 0xedd1e666;
}

/*!
 * Get the slot for a handle. The card must be in the given state.
 * Return -1 if it fails.
 */
s32 handle_to_slot(u32 handle, MemoryCardState state) {
  if (mc[0].state == state && mc[0].handle == handle) {
    return 0;
  }
  if (mc[1].state == state && mc[0].handle == handle) {
    return 1;
  } else {
    return -1;
  }
}

/*!
 * Run the Memory Card state machine.
 * This waits for in-progress ops to finish.
 * If it is done, it starts a new op, if there is one pending.
 */
void MC_run() {
  // if we have an in-progress operation, wait for it to complete.
  if (callback) {
    s32 sony_cmd, sony_status;
    s32 status = sceMcSync(1, &sony_cmd, &sony_status);
    McCallbackFunc callback_for_sync = callback;
    if (status == sceMcExecRun) {
      // busy, return.
      return;
    }

    if (status == sceMcExecFinish) {
      // sony function is done. do the callback
      callback = nullptr;
      (*callback_for_sync)(sony_status);
    } else {
      // sony function is done, but failed.
      assert(false);
      callback = nullptr;
      (*callback_for_sync)(0);
    }

    if (callback) {
      // if we got another callback, it means there's another op started by the prev callback.
      // and this case, we're done.
      return;
    }
  }

  // if we got here, there is no in-progress operation. So start the next one.
  if (op.operation == MemoryCardOperationKind::FORMAT) {
    // grab the slot. should be open, but not formatted
    p1 = handle_to_slot(op.param, MemoryCardState::OPEN);
    if (p1 == -1) {
      // no slot in the right state.
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      // do a getInfo
      s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
      if (info_result == sceMcResSucceed) {
        callback = cb_reprobe_format;
      }
      // allow some number of errors.
      op.counter--;
      if (op.counter == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  } else if (op.operation == MemoryCardOperationKind::UNFORMAT) {
    p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    if (p1 == -1) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      s32 rv = sceMcUnformat(p1, 0);
      if (rv == sceMcResSucceed) {
        callback = cb_unformat;
      }
      op.counter--;
      if (op.counter == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  } else if (op.operation == MemoryCardOperationKind::CREATE_FILE) {
    p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    if (p1 == -1) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
      if (info_result == sceMcResSucceed) {
        callback = cb_reprobe_createfile;
      }
      // allow some number of errors.
      op.counter--;
      if (op.counter == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  } else if (op.operation == MemoryCardOperationKind::SAVE) {
    p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    if (p1 == -1) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
      if (info_result == sceMcResSucceed) {
        callback = cb_reprobe_save;
      }
      // allow some number of errors.
      op.counter--;
      if (op.counter == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  }
  // TODO: the rest.
}

/*!
 * Set the language or something.
 */
void MC_set_language(s32 l) {
  printf("Language set to %d\n", l);
  language = l;
}

/*!
 * Set the current memory card operation to FORMAT the given card.
 */
u64 MC_format(s32 card_idx) {
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    op.operation = MemoryCardOperationKind::FORMAT;
    op.result = McStatusCode::BUSY;
    op.counter = 100;
    op.param = card_idx;
  }
  return can_add;
}

/*!
 * Set the current memory card operation to UNFORMAT the given card.
 */
u64 MC_unformat(s32 card_idx) {
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    op.operation = MemoryCardOperationKind::UNFORMAT;
    op.result = McStatusCode::BUSY;
    op.counter = 100;
    op.param = card_idx;
  }
  return can_add;
}

/*!
 * Set the current memory card operation to create the save file.
 * The data I believe is just an empty buffer.
 */
u64 MC_createfile(s32 param, Ptr<u8> data) {
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    op.operation = MemoryCardOperationKind::CREATE_FILE;
    op.result = McStatusCode::BUSY;
    op.counter = 100;
    op.param = param;
    op.data_ptr = data;
  }
  return can_add;
}

/*!
 * Set the current operation to SAVE.
 */
u64 MC_save(s32 card_idx, s32 file_idx, Ptr<u8> save_data, Ptr<u8> save_summary_data) {
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    op.operation = MemoryCardOperationKind::SAVE;
    op.result = McStatusCode::BUSY;
    op.counter = 100;
    op.param = card_idx;
    op.param2 = file_idx;
    op.data_ptr = save_data;
    op.data_ptr2 = save_summary_data;
  }
  return can_add;
}

u64 MC_load(s32 card_idx, s32 file_idx, Ptr<u8> data) {
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    op.operation = MemoryCardOperationKind::LOAD;
    op.result = McStatusCode::BUSY;
    op.counter = 100;
    op.param = card_idx;
    op.param2 = file_idx;
    op.data_ptr = data;
  }
  return can_add;
}

/*!
 * Some sort of test function for memory card stuff.
 * This is exported as a GOAL function, but nothing calls it.
 */
void MC_makefile(s32 port, s32 size) {
  sceMcMkdir(port, 0, "/BASCUS-00000XXXXXXXX");
  // wait for operation to complete
  s32 cmd, result, fd;
  sceMcSync(0, &cmd, &result);

  if (result == sceMcResSucceed || result == sceMcResNoEntry) {
    // it worked, or the folder already exists...

    // open file
    sceMcOpen(port, 0, "/BASCUS-00000XXXXXXXX/BASCUS-00000XXXXXXXX", SCE_CREAT | SCE_WRONLY);
    sceMcSync(0, &cmd, &fd);

    if (result < 0) {
      printf("Can\'t open file on memcard [%d]\n", result);
    } else {
      // write some random crap into the memory card.
      sceMcWrite(fd, Ptr<u8>(0x1000000).c(), size);
      sceMcSync(0, &cmd, &result);
      if (result != size) {
        printf("Only written %d bytes\n", result);
      }
      sceMcClose(fd);
      sceMcSync(0, &cmd, &result);
    }
  } else {
    printf("Can\'t create garbage folder [%d]\n", result);
  }
}

u32 MC_check_result() {
  return (u32)op.result;
}

void MC_get_status(s32 slot, Ptr<mc_slot_info> info) {
  info->handle = 0;
  info->known = 0;
  info->formatted = 0;
  info->initted = 0;
  for (s32 i = 0; i < 4; i++) {
    info->files[i].present = 0;
  }
  info->last_file = 0xffffffff;
  info->mem_required = SAVE_SIZE;
  info->mem_actual = 0;

  switch (mc[slot].state) {
    case MemoryCardState::KNOWN:
      info->known = 1;
      break;
    case MemoryCardState::OPEN:
      info->known = 1;
      info->handle = mc[slot].handle;
      break;
    case MemoryCardState::FORMATTED:
      info->known = 1;
      info->handle = mc[slot].handle;
      info->formatted = 1;
      if (mc[slot].inited == 0) {
        info->mem_actual = mc[slot].mem_size;
      } else {
        info->initted = 1;
        for (s32 file = 0; file < 4; file++) {
          info->files[file].present = mc[slot].files[file].present;
          for (s32 i = 0; i < 64; i++) {  // actually a loop over u32's
            info->files[file].data[i] = mc[slot].files[file].data[i];
          }
        }
        info->last_file = mc[slot].last_file;
      }
    case MemoryCardState::UNKNOWN:
      break;
  }
}

/*!
 * Check for an error. Returns true if there is an error and sets op.result as needed
 */
u64 cb_check(s32 sony_error, McStatusCode goal_error) {
  if (sony_error < 0) {
    // sony thing failed.
    if (sony_error < -9) {
      // memory card gone. reset state
      mc[p1].state = MemoryCardState::UNKNOWN;
      // kill in progress op
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
      return 1;
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = goal_error;
      return 1;
    }
  }
  return 0;
}

// cb check open
// cb check read
// cb check close
// cb reprobe

void cb_reprobe_format(s32 sync_result) {
  if (sync_result == sceMcResSucceed) {
    // get info succeeded. we can format.
    s32 format_result = sceMcFormat(p1, 0);
    if (format_result == sceMcResSucceed) {
      callback = cb_format_complete;
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  } else {
    // get info failed.  Revert the state to unknown, this will restart everything.
    mc[p1].state = MemoryCardState::UNKNOWN;
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::BAD_HANDLE;
  }
}

void cb_format_complete(s32 sync_result) {
  if (sync_result == sceMcResSucceed) {
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::OK;
    mc[p1].state = MemoryCardState::FORMATTED;
    mc[p1].formatted = 100;
    mc[p1].inited = 0;
    for (int i = 0; i < 4; i++) {
      mc[p1].files[i].present = 0;
    }
    mc[p1].last_file = -1;
    mc[p1].mem_size = 8000;
  } else {
    mc[p1].state = MemoryCardState::UNKNOWN;
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::FORMAT_FAILED;
  }
}

void cb_unformat(s32 sync_result) {
  if (sync_result == sceMcResSucceed) {
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::OK;
    mc[p1].state = MemoryCardState::UNKNOWN;
  } else {
    mc[p1].state = MemoryCardState::UNKNOWN;
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::FORMAT_FAILED;
  }
}

void cb_reprobe_createfile(s32 sync_result) {
  if (sync_result == sceMcResSucceed) {
    // if the ramdisk is ready, just jump directly to its callback
    if (!RpcBusy(RAMDISK_RPC_CHANNEL)) {
      cb_wait_for_ramdisk(0);
    } else {
      // otherwise, don't.
      callback = cb_wait_for_ramdisk;
    }
  } else {
    mc[p1].state = MemoryCardState::UNKNOWN;
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::BAD_HANDLE;
  }
}

void cb_wait_for_ramdisk(s32) {
  RPC_Ramdisk_LoadCmd cmd;
  cmd.pad = 0;
  cmd.file_id_or_ee_addr = op.data_ptr.offset;
  cmd.offset_into_file = 0;
  cmd.size = 0x1e800;
  memcpy(cmd.name, "SAVEGAME.ICO", 13);  // was 16.
  RpcCall(RAMDISK_RPC_CHANNEL, RAMDISK_BYPASS_LOAD_FILE, 1, &ramdisk_cmd, 0x20, nullptr, 0);
  callback = cb_wait_for_ramdisk_load;
}

void cb_wait_for_ramdisk_load(s32) {
  if (RpcBusy(RAMDISK_RPC_CHANNEL) == 0) {
    p2 = 11;  // filenames left to delete
    if (sceMcDelete(p1, 0, filename[11]) == sceMcResSucceed) {
      callback = cb_createfile_erasing;
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  } else {
    callback = cb_wait_for_ramdisk_load;
  }
}

void cb_createfile_erasing(s32 sync_result) {
  if (sync_result == sceMcResSucceed || sync_result == sceMcResNoEntry ||
      sync_result == sceMcResNotEmpty) {
    mc[p1].inited = 0;
    // delete didn't fail.
    if (p2 < 1) {
      // on the last one. which is the directory to create.
      if (sceMcMkdir(p1, 0, filename[0]) == sceMcResSucceed) {
        callback = cb_createdir;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else {
      p2--;
      if (sceMcDelete(p1, 0, filename[p2]) == sceMcResSucceed) {
        callback = cb_createfile_erasing;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  } else {
    if (sync_result == sceMcResDeniedPermit) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    } else {
      mc[p1].state = MemoryCardState::UNKNOWN;
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    }
  }
}

void cb_createdir(s32 sync_result) {
  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
    // this sets up some stuff for the icon file that we will ignore.
    // memset(&iconsys,0,0x3c4);
    // kstrcpy(&iconsys,&DAT_00137000);
    // if (language == Language::Japanese) {
    //  kstrcpy(&DAT_00137560,titles[5]);
    // } else {
    //  // non japanese need to convert to shift-JIS format.
    //  ASCII2SJIS(&DAT_00137560,titles[language]);
    // }
    // DAT_001374a6 = 0x20;
    // DAT_001374ac = 0;
    // memcpy(&DAT_001374b0,bgcolor.610,0x40);
    // memcpy(&DAT_001374f0,lightdir.611,0x30);
    // memcpy(&DAT_00137520,lightcol.612,0x30);
    // memcpy(&DAT_00137550,ambient.613,0x10);
    // kstrcpy(&DAT_001375a4,"icon.ico");
    // kstrcpy(&DAT_001375e4,"icon.ico");
    // kstrcpy(&DAT_00137624,"icon.ico");

    p2 = 1;
    if (sceMcOpen(p1, 0, filename[1], 0x203) == 0) {
      callback = cb_createdfile;
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  }
}

void cb_createdfile(s32 sync_result) {
  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
    if (p2 == 1) {
      p3 = sync_result;  // the fd of the icon file.
      // actually would write the icon sys file.
      if (sceMcWrite(sync_result, nullptr, 0) == sceMcResSucceed) {
        callback = cb_writtenfile;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else if (p2 == 2) {
      p3 = sync_result;
      // would write the icon data (ramdisk loaded into the temp buffer)
      if (sceMcWrite(sync_result, nullptr, 0) == sceMcResSucceed) {
        callback = cb_writtenfile;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else if (p2 == 3) {
      p3 = sync_result;
      kstrcpy(op.data_ptr.cast<char>().c(), "Nope, the save game data isn\'t in this file!\n");
      if (sceMcWrite(p3, op.data_ptr.c(), strlen((const char*)op.data_ptr.c())) ==
          sceMcResSucceed) {
        callback = cb_writtenfile;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else {
      p3 = sync_result;
      memset(op.data_ptr.c(), 0, 0x11800);
      if (sceMcWrite(p3, op.data_ptr.c(), 0x11800)) {
        callback = cb_writtenfile;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
  }
}

void cb_writtenfile(s32 sync_result) {
  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
    if (sceMcClose(p3) == sceMcResSucceed) {
      callback = cb_closedfile;
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  }
}

void cb_closedfile(s32 sync_result) {
  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
    p2++;
    if (p2 < 0xc) {
      if (sceMcOpen(p1, 0, filename[p2], 0x203) == sceMcResSucceed) {
        callback = cb_createdfile;
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::OK;
      mc[p1].inited = 1;
      for (int i = 0; i < 4; i++) {
        mc[p1].files[i].present = 0;
      }
      mc[p1].last_file = -1;
    }
  }
}

void cb_reprobe_save(s32) {
  assert(false);
}