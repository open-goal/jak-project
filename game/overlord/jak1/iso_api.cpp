#include "iso_api.h"

#include "iso_queue.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/common/sbank.h"
#include "game/overlord/common/srpc.h"
#include "game/overlord/jak1/iso.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak1 {

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

  for (u32 i = 0; i < gMusicTweakInfo.TweakCount; i++) {
    if (!strcmp(gMusicTweakInfo.MusicTweak[i].MusicName, music_name)) {
      gMusicTweak = gMusicTweakInfo.MusicTweak[i].VolumeAdjust;
      return;
    }
  }

  gMusicTweak = 0x80;
}

void QueueVAGStream(FileRecord* file, VagDirEntry* vag, u32 sound_id, u32 priority) {
  if (vag == nullptr) {
    return;
  }

  auto* cmd = GetVAGCommand();
  cmd->cmd_id = QUEUE_VAG_STREAM;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  cmd->file = file;
  cmd->vag = vag;
  cmd->sound_id = sound_id;
  cmd->priority = priority;
  cmd->positioned = 0;

  SendMbx(iso_mbx, cmd);
}

void PlayVAGStream(FileRecord* file,
                   VagDirEntry* vag,
                   u32 sound_id,
                   s32 volume,
                   u32 priority,
                   Vec3w* trans) {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = PLAY_VAG_STREAM;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  cmd->file = file;
  cmd->vag = vag;
  cmd->sound_id = sound_id;
  cmd->volume = volume;
  cmd->priority = priority;

  if (trans) {
    cmd->trans = *trans;
    cmd->positioned = 1;
  } else {
    cmd->positioned = 0;
  }

  SendMbx(iso_mbx, cmd);
}

void SetVAGStreamVolume(s32 volume) {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = SET_VAG_VOLUME;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  cmd->volume = volume;
  SendMbx(iso_mbx, cmd);
}

void SetDialogVolume(s32 volume) {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = SET_DIALOG_VOLUME;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  cmd->volume = volume;
  SendMbx(iso_mbx, cmd);
}

void StopVAGStream(VagDirEntry* vag, u32 priority) {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = STOP_VAG_STREAM;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  cmd->vag = vag;
  cmd->priority = priority;

  SendMbx(iso_mbx, cmd);
}

void PauseVAGStream() {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = PAUSE_VAG_STREAM;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  SendMbx(iso_mbx, cmd);
}

void UnpauseVAGStream() {
  auto cmd = GetVAGCommand();
  cmd->cmd_id = CONTINUE_VAG_STREAM;
  cmd->messagebox_to_reply = 0;
  cmd->thread_id = 0;
  SendMbx(iso_mbx, cmd);
}

/*!
 * Load a File to IOP memory (blocking)
 */
s32 LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length) {
  lg::debug("[OVERLORD] LoadISOFileToIOP {}, {}/{} bytes", file->name, length, (s32)file->size);
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
  lg::debug("[OVERLORD] LoadISOFileToEE {}, {}/{} bytes", file->name, length, (s32)file->size);
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
  lg::debug("[OVERLORD] LoadISOFileChunkToEE {} : {} offset {}", file->name, length, offset);
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

}  // namespace jak1