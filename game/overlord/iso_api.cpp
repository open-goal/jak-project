#include "iso_api.h"
#include "game/sce/iop.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"

using namespace iop;

/*!
 * Load a File to IOP memory (blocking)
 */
void LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length) {
  // printf("[OVERLORD] LoadISOFileToIOP %s, %d/%d bytes\n", file->name, length, file->size);
  spdlog::debug("[OVERLORD] LoadISOFileToIOP {}, {}/{} bytes", file->name, length, file->size);
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
}

/*!
 * Load a File to IOP memory (blocking)
 */
void LoadISOFileToEE(FileRecord* file, uint32_t addr, uint32_t length) {
  // printf("[OVERLORD] LoadISOFileToEE %s, %d/%d bytes\n", file->name, length, file->size);
  spdlog::debug("[OVERLORD] LoadISOFileToEE {}, {}/{} bytes", file->name, length, file->size);
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
}
