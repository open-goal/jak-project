#include "iso_api.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_cd.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;
void jak3_overlord_init_globals_iso_api() {}

/*!
 * This file is the "API" that implementations of RPCs or other code can use to submit things to the
 * ISO thread. Note that not all RPCs use this API - for example the DGO RPC just manually submits
 * messages.  Generally, these functions will not return until the actual action is complete, like a
 * file is loaded.
 * - except for messages to pause/play audio, those functions will return immediately, but there may
 * be a delay until they are actually processed.
 */

// PluginVagAndVagWad not ported.

u32 EEVagAndVagWad(ISO_VAGCommand* cmd, char* name) {
  ISOName name_buff;
  if (*name == '$' || strlen(name) < 9) {
    if (*name == '$') {
      name++;
    }
    MakeISOName(&name_buff, name);
  } else {
    file_util::ISONameFromAnimationName(name_buff.data, name);
  }

  cmd->vag_dir_entry = get_file_system()->FindVAGFile(name_buff.data);
  if (cmd->vag_dir_entry) {
    memcpy(name_buff.data, "VAGWAD  ", 8);
    strncpy(name_buff.data + 8, g_pszLanguage, 4);
    auto* file_def = get_file_system()->FindIN(&name_buff);
    // piVar1 = g_pFileSystem;
    cmd->file_def = file_def;

    strncpy(name_buff.data + 8, "INT", 4);

    cmd->vag_file_def = get_file_system()->FindIN(&name_buff);
    if (cmd->vag_dir_entry && cmd->file_def && cmd->vag_file_def) {
      return 1;
    }
  }

  cmd->vag_dir_entry = nullptr;
  cmd->file_def = nullptr;
  cmd->vag_file_def = nullptr;
  return 0;
}

int LoadISOFileToIOP(const ISOFileDef* file_def, void* addr, int length) {
  ISO_LoadSingle cmd;
  cmd.msg_type = ISO_Hdr::MsgType::LOAD_IOP;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = GetThreadId();
  cmd.file_def = file_def;
  cmd.addr = (u8*)addr;
  cmd.maxlen = length;
  lg::warn("--------------- LoadISOFileToIOP START");
  SendMbx(g_nISOMbx, &cmd);
  SleepThread();
  lg::warn("--------------- LoadISOFileToIOP END");
  if (cmd.status == EIsoStatus::NONE_0) {
    return cmd.length_to_copy;
  } else {
    return 0;
  }
}

int LoadISOFileToEE(const ISOFileDef* file_def, u32 addr, int length) {
  ISO_LoadSingle cmd;
  cmd.msg_type = ISO_Hdr::MsgType::LOAD_EE;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = GetThreadId();
  cmd.file_def = file_def;
  cmd.addr = (u8*)(u64)addr;
  cmd.maxlen = length;
  lg::warn("--------------- LoadISOFileToEE START");
  SendMbx(g_nISOMbx, &cmd);
  SleepThread();
  lg::warn("--------------- LoadISOFileToEE  END");
  if (cmd.status == EIsoStatus::NONE_0) {
    return cmd.length_to_copy;
  }
  return 0;
}

int LoadISOFileChunkToEE(const ISOFileDef* file_def, u32 addr, int max_len, int sector_offset) {
  ISO_LoadSingle cmd;
  cmd.msg_type = ISO_Hdr::MsgType::LOAD_EE_CHUNK;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = GetThreadId();
  cmd.file_def = file_def;
  cmd.addr = (u8*)(u64)addr;
  cmd.maxlen = max_len;
  cmd.sector_offset = sector_offset;
  lg::warn("--------------- LoadISOFileChunkToEE START");
  SendMbx(g_nISOMbx, &cmd);
  SleepThread();
  lg::warn("--------------- LoadISOFileChunkToEE END");
  if (cmd.status == EIsoStatus::NONE_0) {
    return cmd.length_to_copy;
  }
  return 0;
}

u32 LoadSoundBankToIOP(const char* name, SoundBankInfo* bank, u32 mode) {
  ISO_LoadSoundbank cmd;
  cmd.msg_type = ISO_Hdr::MsgType::LOAD_SOUNDBANK;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = GetThreadId();
  cmd.bank_info = bank;
  cmd.name = name;
  cmd.priority = mode;
  lg::warn("--------------- LoadSoundBankToIOP START");
  SendMbx(g_nISOMbx, &cmd);
  SleepThread();
  lg::warn("--------------- LoadSoundBankToIOP END");

  return (u32)cmd.status;
}

void PlayMusicStream(VagStreamData* stream) {
  int iVar1;
  ISO_VAGCommand cmd;

  cmd.msg_type = ISO_Hdr::MsgType::PLAY_MUSIC_STREAM;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = 0;
  iVar1 = EEVagAndVagWad(&cmd, stream->name);
  if (iVar1 == 0) {
    //    if (bWarn == 0) {
    //      bWarn = 1;
    //    }
  } else {
    cmd.play_volume = 0x400;
    // bWarn = 0;
    strncpy(cmd.name, stream->name, 0x30);
    cmd.id = stream->id;
    cmd.priority_pq = 9;
    cmd.music_flag = 1;
    cmd.maybe_sound_handler = 0;
    cmd.plugin_id = 0;
    cmd.art_flag = 0;
    cmd.movie_flag = 0;
    cmd.updated_trans = 0;
    IsoPlayMusicStream(&cmd);
  }
}

void QueueVAGStream(VagStreamData* stream) {
  bool bVar1;
  bool bVar2;
  ISO_VAGCommand cmd;

  cmd.msg_type = ISO_Hdr::MsgType::VAG_QUEUE;
  cmd.mbox_reply = 0;
  cmd.thread_to_wake = 0;
  if (stream->sound_handler == 0) {
    EEVagAndVagWad(&cmd, stream->name);
    cmd.play_volume = 0x400;
    cmd.play_group = 2;
  } else {
    ASSERT_NOT_REACHED();
    //    PluginVagAndVagWad(&cmd,stream);
    //    cmd.play_volume = stream->maybe_volume2;
    //    cmd.oog = stream->maybe_volume_3;
    //    cmd.play_group = stream->group;
  }
  strncpy(cmd.name, stream->name, 0x30);
  cmd.id = stream->id;
  cmd.plugin_id = stream->plugin_id;
  cmd.priority_pq = stream->priority;
  cmd.maybe_sound_handler = stream->sound_handler;
  bVar1 = stream->movie_art_load != 0;
  cmd.movie_flag = bVar1;
  bVar2 = stream->art_load != 0;
  cmd.art_flag = bVar2;
  cmd.music_flag = 0;
  if (bVar2) {
    cmd.flags.art = 1;
  }
  if (bVar1) {
    cmd.flags.movie = 1;
  }
  cmd.updated_trans = 0;
  IsoQueueVagStream(&cmd);
}

void PauseVAGStreams() {
  auto* cmd = GetVAGCommand();
  cmd->msg_type = ISO_Hdr::MsgType::VAG_PAUSE;
  cmd->mbox_reply = 0;
  cmd->thread_to_wake = 0;
  SendMbx(g_nISOMbx, cmd);
}

void UnpauseVAGStreams() {
  auto* cmd = GetVAGCommand();
  cmd->msg_type = ISO_Hdr::MsgType::VAG_UNPAUSE;
  cmd->mbox_reply = 0;
  cmd->thread_to_wake = 0;
  SendMbx(g_nISOMbx, cmd);
}

void SetVAGStreamPitch(int id, int pitch) {
  auto* cmd = GetVAGCommand();
  cmd->msg_type = ISO_Hdr::MsgType::VAG_SET_PITCH_VOL;
  cmd->id = id;
  cmd->pitch_cmd = pitch;
  cmd->mbox_reply = 0;
  cmd->thread_to_wake = 0;
  SendMbx(g_nISOMbx, cmd);
}

}  // namespace jak3