#include "ssound.h"

#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak2/vag.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/sound/sndshim.h"

namespace jak2 {
void ssound_init_globals() {}

void SetBufferMem(void*, int) {}
void ReleaseBufferMem() {}

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
    } else if ((cmd->byte7 == '\0') || (cmd->byte8 != '\0')) {
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