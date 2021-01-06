#include "iso_api.h"
#include "game/sce/iop.h"
#include "common/log/log.h"

using namespace iop;

/*!
 * Load a File to IOP memory (blocking)
 */
s32 LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length) {
  lg::debug("[OVERLORD] LoadISOFileToIOP {}, {}/{} bytes", file->name, length, file->size);
  IsoCommandLoadSingle cmd;
  cmd.cmd_id = LOAD_TO_IOP_CMD_ID;
  cmd.messagebox_to_reply = 0;
  cmd.thread_id = GetThreadId();
  cmd.file_record = file;
  cmd.dest_addr = (u8*)addr;
  cmd.length = length;
  SendMbx(iso_mbx, &cmd);
  SleepThread();

  if (cmd.status) {
    cmd.length_to_copy = 0;
  }

  return cmd.length_to_copy;
}

/*!
 * Load a File to IOP memory (blocking)
 */
s32 LoadISOFileToEE(FileRecord* file, uint32_t addr, uint32_t length) {
  lg::debug("[OVERLORD] LoadISOFileToEE {}, {}/{} bytes", file->name, length, file->size);
  IsoCommandLoadSingle cmd;
  cmd.cmd_id = LOAD_TO_EE_CMD_ID;
  cmd.messagebox_to_reply = 0;
  cmd.thread_id = GetThreadId();
  cmd.file_record = file;
  cmd.dest_addr = (u8*)(u64)addr;
  cmd.length = length;
  SendMbx(iso_mbx, &cmd);
  SleepThread();

  if (cmd.status) {
    cmd.length_to_copy = 0;
  }

  return cmd.length_to_copy;
}

s32 LoadISOFileChunkToEE(FileRecord* file, uint32_t dest_addr, uint32_t length, uint32_t offset) {
  lg::debug("[OVERLORD] LoadISOFileChunkToEE {} : {} offset {}\n", file->name, length, offset);
  IsoCommandLoadSingle cmd;
  cmd.cmd_id = LOAD_TO_EE_OFFSET_CMD_ID;
  cmd.messagebox_to_reply = 0;
  cmd.thread_id = GetThreadId();
  cmd.file_record = file;
  cmd.dest_addr = (u8*)(u64)dest_addr;
  cmd.length = length;
  cmd.offset = offset;
  SendMbx(iso_mbx, &cmd);
  SleepThread();
  if (cmd.status) {
    cmd.length_to_copy = 0;
  }
  return cmd.length_to_copy;
}
