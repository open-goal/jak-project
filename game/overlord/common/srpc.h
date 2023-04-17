#pragma once
#include "common/common_types.h"

// added for PC port
extern u32 gMusicFadeHack;

constexpr int MUSIC_TWEAK_COUNT = 32;

struct MusicTweaks {
  u32 TweakCount;

  struct {
    char MusicName[12];
    s32 VolumeAdjust;
  } MusicTweak[MUSIC_TWEAK_COUNT];
};

extern s32 gMusicTweak;
extern MusicTweaks gMusicTweakInfo;

void srpc_init_globals();