#include "ssound.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak1/iso.h"
#include "game/runtime.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak1 {
VolumePair gPanTable[361];

s32 gMusicVol = 0x400;

u32 gStreamSRAM = 0;
u32 gTrapSRAM = 0;

void CatalogSRAM() {}

static void* SndMemAlloc();
static void SndMemFree(void* ptr);
void InitSound_Overlord() {
  for (auto& s : gSounds) {
    s.id = 0;
  }

  if (g_game_version == GameVersion::Jak1) {
    SetCurve(1, 0, 0);
    SetCurve(2, 4096, 0);
    SetCurve(3, 0, 4096);
    SetCurve(4, 2048, 0);
    SetCurve(5, 2048, 2048);
    SetCurve(6, -4096, 0);
    SetCurve(7, -2048, 0);
  } else {
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
  }

  snd_StartSoundSystem();
  snd_RegisterIOPMemAllocator(SndMemAlloc, SndMemFree);
  snd_LockVoiceAllocator(1);
  u32 voice = snd_ExternVoiceAlloc(2, 0x7f);
  snd_UnlockVoiceAllocator();

  // The voice allocator returns a number in the range 0-47 where voices
  // 0-23 are on SPU Core 0 and 24-47 are on core 2.
  // For some reason we convert it to this format where 0-47 alternate core every step.
  voice = voice / 24 + ((voice % 24) * 2);

  // Allocate SPU RAM for our streams.
  // (Which we don't need on PC)
  gStreamSRAM = snd_SRAMMalloc(0xc030);
  gTrapSRAM = gStreamSRAM + 0xC000;

  snd_SetMixerMode(0, 0);

  for (int i = 0; i < 8; i++) {
    snd_SetGroupVoiceRange(i, 0x10, 0x2f);
  }

  snd_SetGroupVoiceRange(1, 0, 0xf);
  snd_SetGroupVoiceRange(2, 0, 0xf);

  snd_SetReverbDepth(SND_CORE_0 | SND_CORE_1, 0, 0);
  snd_SetReverbType(SND_CORE_0, SD_REV_MODE_OFF);
  snd_SetReverbType(SND_CORE_1, SD_REV_MODE_OFF);

  CatalogSRAM();

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

  SemaParam sema;
  sema.attr = SA_THPRI;
  sema.init_count = 1;
  sema.max_count = 1;
  sema.option = 0;

  gSema = CreateSema(&sema);
  if (gSema < 0) {
    while (true)
      ;
  }
}

void SetEarTrans(Vec3w* ear_trans0, Vec3w* ear_trans1, Vec3w* cam_trans, s32 cam_angle) {
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

  SetVAGVol();
}

void SetMusicVol() {
  s32 volume = (gMusicVol * gMusicFade >> 0x10) * gMusicTweak >> 7;
  snd_SetMasterVolume(1, volume);
  snd_SetMasterVolume(2, volume);
}

// Do we even need/want these
// TODO void SetBufferMem() {}
// TODO void ReleaseBufferMem() {}
static void* SndMemAlloc() {
  return nullptr;
}
static void SndMemFree(void* /*ptr*/) {}
}  // namespace jak1