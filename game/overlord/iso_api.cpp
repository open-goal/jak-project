#include "iso_api.h"
#include "game/overlord/srpc.h"
#include "game/sce/iop.h"
#include "common/log/log.h"
#include "sbank.h"
#include "common/util/Assert.h"

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

/*!
 * Send a command to the ISO thread to load a sound bank. This will sleep the calling thread
 * until the load has completed.
 */
void LoadSoundBank(const char* bank_name, SoundBank* bank) {
  (void)bank;
  ASSERT(strlen(bank_name) < 16);
  SoundBankLoadCommand cmd;
  cmd.cmd_id = LOAD_SOUND_BANK;
  cmd.messagebox_to_reply = 0;
  cmd.thread_id = GetThreadId();
  strcpy(cmd.bank_name, bank_name);
  cmd.bank = bank;
  SendMbx(iso_mbx, &cmd);
  SleepThread();  // wait for finish.
}

void LoadMusic(const char* music_name, s32* bank) {
  ASSERT(strlen(music_name) < 16);
  MusicLoadCommand cmd;
  cmd.cmd_id = LOAD_MUSIC;
  cmd.messagebox_to_reply = 0;
  cmd.thread_id = GetThreadId();
  strcpy(cmd.music_name, music_name);
  cmd.music_handle = bank;
  SendMbx(iso_mbx, &cmd);
  SleepThread();

  for (int i = 0; i < gMusicTweakInfo.TweakCount; i++) {
    if (!strcmp(gMusicTweakInfo.MusicTweak[i].MusicName, music_name)) {
      gMusicTweak = gMusicTweakInfo.MusicTweak[i].VolumeAdjust;
      return;
    }
  }

  gMusicTweak = 0x80;
}
