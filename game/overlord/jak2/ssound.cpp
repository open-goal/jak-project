#include "ssound.h"

#include "common/util/Assert.h"

#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak2 {
VolumePair gPanTable[361];

s32 StreamThread = 0;
void ssound_init_globals() {
  StreamThread = 0;
}

void SetBufferMem(void*, int) {}
void ReleaseBufferMem() {}

void InitSound_overlord() {
  for (auto& s : gSounds) {
    s.id = 0;
  }

  SetCurve(2, 0, 0);
  SetCurve(9, 0, 0);
  SetCurve(11, 0, 0);
  SetCurve(10, 0, 0);
  SetCurve(3, 4096, 0);
  SetCurve(4, 0, 4096);
  SetCurve(5, 2048, 0);
  SetCurve(6, 2048, 2048);
  SetCurve(7, -4096, 0);
  SetCurve(8, -2048, 0);

  // need this one for PC sound to start up
  snd_StartSoundSystem();

  // there's a bunch of stuff that we don't use.
  StreamVoice[0] = 0;
  StreamVoice[1] = 1;
  StreamVoice[2] = 2;
  StreamVoice[3] = 3;  // TODO idk what im doing.

  for (int i = 0; i < 91; i++) {
    s16 opposing_front = static_cast<s16>(((i * 0x33ff) / 0x5a) + 0xc00);

    s16 rear_right = static_cast<s16>(((i * -0x2800) / 0x5a) + 0x3400);
    s16 rear_left = static_cast<s16>(((i * -0xbff) / 0x5a) + 0x3fff);

    gPanTable[90 - i].left = 0x3FFF;
    gPanTable[180 - i].left = opposing_front;
    gPanTable[270 - i].left = rear_right;
    gPanTable[360 - i].left = rear_left;

    gPanTable[i].right = opposing_front;
    gPanTable[90 + i].right = 0x3FFF;
    gPanTable[180 + i].right = rear_left;
    gPanTable[270 + i].right = rear_right;
  }

  snd_SetPanTable((s16*)gPanTable);
  snd_SetPlayBackMode(2);

  SemaParam local_58;
  local_58.attr = 1;
  local_58.init_count = 1;
  local_58.max_count = 1;
  local_58.option = 0;
  gSema = CreateSema(&local_58);
  if (gSema < 0) {
    printf("IOP: ======================================================================\n");
    printf("IOP: ssound InitSound: can\'t create semaphore\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  // Init989Plugins(); TODO
  // InitStreamLfoHandler(); TODO
  InitVagStreamList(&PluginStreamsList, 4, "plugin");
  InitVagStreamList(&EEStreamsList, 4, "ee");
  InitVagStreamList(&EEPlayList, 8, "play");
  InitVagStreamList(&RequestedStreamsList, 8, "streams");
  InitVagStreamList(&NewStreamsList, 4, "new");

  ThreadParam local_30;
  local_30.attr = 0x2000000;
  local_30.entry = StreamListThread;
  local_30.initPriority = 0x78;
  local_30.stackSize = 0x1000;
  local_30.option = 0;
  StreamThread = CreateThread(&local_30);
  if (StreamThread < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: ssound InitSound: can\'t create streamlist thread\n");
    printf("IOP: ======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  StartThread(StreamThread, 0);
}

void SetMusicVol() {
  int vol = (MasterVolume[1] * gMusicFade >> 0x10) * gMusicTweak >> 7;
  snd_SetMasterVolume(1, vol);
  snd_SetMasterVolume(2, vol);
}

void SetEarTrans(Vec3w* ear_trans0, Vec3w* ear_trans1, Vec3w* cam_trans, s32 cam_angle) {
  // some assuming that this is the same in jak2...
  s32 tick = snd_GetTick();
  u32 delta = tick - sLastTick;
  sLastTick = tick;

  gEarTrans[0] = *ear_trans0;
  gEarTrans[1] = *ear_trans1;
  gCamTrans = *cam_trans;
  gCamAngle = cam_angle;

  for (auto& s : gSounds) {
    if (s.id != 0 && s.is_music == 0) {
      if (s.auto_time != 0) {
        UpdateAutoVol(&s, delta);
      }
      UpdateLocation(&s);
    }
  }

  // SetVAGVol();

  int iVar2 = 0;
  auto* cmd = VagCmds;
  // piVar6 = &VagCmds[0].vol_multiplier;
  do {
    if (cmd->unk_136 == 0x0) {
    LAB_0000c388:
      SetVAGVol(cmd, 1);
    } else if ((cmd->sb_scanned == '\0') || (cmd->byte8 != '\0')) {
      if (cmd->byte20 == '\0') {
        if ((u32)cmd->vol_multiplier < 0x11) {
          cmd->vol_multiplier = 0;
        } else {
          cmd->vol_multiplier = cmd->vol_multiplier - 0x10;
        }
        SetVAGVol(cmd, 1);
        if (cmd->vol_multiplier != 0)
          goto LAB_0000c398;
      }
    LAB_0000c378:
      StopVagStream(cmd, 1);
    } else {
      if (snd_SoundIsStillPlaying(cmd->id) != 0)
        goto LAB_0000c388;
      if (cmd->byte20 != '\0')
        goto LAB_0000c378;
      // CpuSuspendIntr(local_30);
      cmd->byte8 = '\x01';

      // CpuResumeIntr(local_30[0]);
    }
  LAB_0000c398:
    cmd = cmd + 1;
    // piVar6 = piVar6 + 0x51;
    iVar2 = iVar2 + 1;
    if (3 < iVar2) {
      return;
    }
  } while (true);
}

}  // namespace jak2