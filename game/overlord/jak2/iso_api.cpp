#include "iso_api.h"

#include <cstring>

#include "common/util/FileUtil.h"

#include "game/overlord/common/srpc.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak2 {
void EEVagAndVagwad(char* name, VagCmd* cmd) {
  char name_buff[16];

  int name_len;
  if (*name != '$') {
    name_len = strlen(name);
    if (8 < name_len) {
      file_util::ISONameFromAnimationName(name_buff, name);
      goto LAB_0000cc60;
    }
  }

  name_len = strlen(name);
  if (*name == '$') {
    name = name + 1;
  }
  if ((int)name_len < 9) {
    memset(name_buff, 0x20, 8);
    memcpy(name_buff, name, name_len);
  } else {
    memcpy(name_buff, name, 8);
  }
  {
    int iVar3 = 0;
    char* pbVar4 = name_buff;
    do {
      *pbVar4 = ::toupper(*pbVar4);
      iVar3 = iVar3 + 1;
      pbVar4 = (name_buff + iVar3);
    } while (iVar3 < 8);
  }

LAB_0000cc60:
  cmd->vag_dir_entry = FindVAGFile(name_buff);
  memcpy(name_buff, "VAGWAD  ", 8);
  strncpy(name_buff + 8, gLanguage, 4);
  cmd->file_record = (isofs->find_in)(name_buff);
}

void QueueVAGStream(VagStrListNode* param_1) {
  char bVar1;
  int iVar2;
  char* pcVar3;
  char* pbVar4;
  VagCmd cmd;
  char local_20[12];

  cmd.header.cmd_kind = 0x400;
  cmd.header.mbx_to_reply = 0;
  cmd.header.thread_id = 0;
  if (param_1->sound_handler == 0) {
    EEVagAndVagwad(param_1->name, &cmd);
    cmd.vol_multiplier = 0x400;
  } else {
    pcVar3 = param_1->name;
    strcpy(local_20, "           ");
    pbVar4 = local_20;
    do {
      bVar1 = *pcVar3;
      pcVar3 = pcVar3 + 1;
      *pbVar4 = bVar1;
      pbVar4 = pbVar4 + 1;
      if (*pcVar3 == 0x2e)
        break;
    } while (*pcVar3 != 0);
    iVar2 = 0;
    pbVar4 = local_20;
    do {
      if (*pbVar4 - 0x61 < 0x1a) {
        *pbVar4 = *pbVar4 - 0x20;
      }
      iVar2 = iVar2 + 1;
      pbVar4 = (local_20 + iVar2);
    } while (iVar2 < 0xc);
    cmd.vag_dir_entry = FindVAGFile(local_20);
    strcpy(local_20, "VAGWAD     ");
    strncpy(local_20 + 8, gLanguage, 3);
    cmd.file_record = (isofs->find_in)(local_20);
    cmd.vol_multiplier = param_1->vol_multiplier;
    cmd.unk_176 = param_1->unk_100;
  }
  strncpy(cmd.name, param_1->name, 0x30);
  cmd.sound_handler = param_1->sound_handler;
  cmd.id = param_1->id;
  cmd.plugin_id = param_1->unk_68;
  cmd.priority = param_1->prio;
  cmd.unk_288 = param_1->unk_76;
  cmd.unk_292 = param_1->unk_80;
  if (cmd.unk_288 != 0) {
    cmd.byte10 = '\x01';
  }
  if (cmd.unk_292 != 0) {
    cmd.unk_232 = '\x01';
  }
  cmd.unk_296 = 0;
  IsoQueueVagStream(&cmd, 1);
}

int LoadISOFileToIOP(FileRecord* fr, uint8_t* addr, int len) {
  int iVar1;
  CmdLoadSingleIop cmd;

  cmd.header.cmd_kind = 0x101;
  cmd.header.mbx_to_reply = 0;
  cmd.header.thread_id = GetThreadId();
  cmd.file_record = fr;
  cmd.dest_addr = addr;
  cmd.length = len;
  SendMbx(iso_mbx, &cmd);
  SleepThread();
  iVar1 = 0;
  if (cmd.header.status == 0) {
    iVar1 = cmd.length_to_copy;
  }
  return iVar1;
}

int LoadISOFileToEE(FileRecord* param_1, uint32_t param_2, int param_3) {
  int iVar1;
  CmdLoadSingleIop auStack88;

  auStack88.header.cmd_kind = 0x100;
  auStack88.header.mbx_to_reply = 0;
  auStack88.header.thread_id = GetThreadId();
  auStack88.file_record = param_1;
  auStack88.dest_addr = (u8*)(u64)param_2;
  auStack88.length = param_3;
  SendMbx(iso_mbx, &auStack88);
  SleepThread();
  iVar1 = 0;
  if (auStack88.header.status == 0) {
    iVar1 = auStack88.length_to_copy;
  }
  return iVar1;
}

int LoadISOFileChunkToEE(FileRecord* param_1, uint32_t param_2, int param_3, int param_4) {
  int iVar1;
  CmdLoadSingleIop auStack96;

  auStack96.header.cmd_kind = 0x102;
  auStack96.header.mbx_to_reply = 0;
  auStack96.header.thread_id = GetThreadId();
  auStack96.file_record = param_1;
  auStack96.dest_addr = (u8*)(u64)param_2;
  auStack96.length = param_3;
  auStack96.offset = param_4;
  SendMbx(iso_mbx, &auStack96);
  SleepThread();
  iVar1 = 0;
  if (auStack96.header.status == 0) {
    iVar1 = auStack96.length_to_copy;
  }
  return iVar1;
}

void PauseVAGStreams() {
  VagCmd* inasdf;

  inasdf = GetVAGCommand();
  (inasdf->header).cmd_kind = 0x403;
  (inasdf->header).mbx_to_reply = 0;
  (inasdf->header).thread_id = 0;
  SendMbx(iso_mbx, inasdf);
}

void UnpauseVAGStreams() {
  auto* inasdf = GetVAGCommand();
  (inasdf->header).cmd_kind = 0x404;
  (inasdf->header).mbx_to_reply = 0;
  (inasdf->header).thread_id = 0;
  SendMbx(iso_mbx, inasdf);
}

void SetVAGStreamPitch(int param_1, int param_2) {
  auto* inasdf = GetVAGCommand();
  (inasdf->header).cmd_kind = 0x406;
  (inasdf->header).mbx_to_reply = 0;
  (inasdf->header).thread_id = 0;
  inasdf->id = param_1;
  inasdf->unk_256_pitch2 = param_2;
  SendMbx(iso_mbx, inasdf);
}

void SetDialogVolume(int param_1) {
  auto* inasdf = GetVAGCommand();
  (inasdf->header).cmd_kind = 0x407;
  (inasdf->header).mbx_to_reply = 0;
  (inasdf->header).thread_id = 0;
  inasdf->vol_multiplier = param_1;
  SendMbx(iso_mbx, inasdf);
}

void LoadSoundBank(char* param_1, SoundBank* param_2) {
  CmdLoadSoundBank auStack80;
  auStack80.header.cmd_kind = 0x300;
  auStack80.header.mbx_to_reply = 0;
  auStack80.header.thread_id = GetThreadId();
  strncpy(auStack80.bank_name, param_1, 0x10);
  auStack80.bank = param_2;
  SendMbx(iso_mbx, &auStack80);
  SleepThread();
}

void LoadMusic(char* param_1, snd::BankHandle* param_2) {
  CmdLoadMusic auStack88;

  auStack88.header.cmd_kind = 0x380;
  auStack88.header.mbx_to_reply = 0;
  auStack88.header.thread_id = GetThreadId();
  strncpy(auStack88.name, param_1, 0x10);
  auStack88.handle = param_2;
  SendMbx(iso_mbx, &auStack88);
  SleepThread();

  for (u32 i = 0; i < gMusicTweakInfo.TweakCount; i++) {
    if (!strcmp(gMusicTweakInfo.MusicTweak[i].MusicName, param_1)) {
      gMusicTweak = gMusicTweakInfo.MusicTweak[i].VolumeAdjust;
      return;
    }
  }

  gMusicTweak = 0x80;
}

void UnLoadMusic(snd::BankHandle* param_1) {
  gMusicFadeDir = -1;
  if (gMusicFade != 0) {
    do {
      DelayThread(1000);
    } while (gMusicFade != 0);
  }
  snd_UnloadBank(*param_1);
  snd_ResolveBankXREFS();
  *param_1 = 0;
}

}  // namespace jak2
