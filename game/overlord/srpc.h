#pragma once

#ifndef JAK_V2_SRPC_H
#define JAK_V2_SRPC_H

#include "common/common_types.h"

void srpc_init_globals();

constexpr int MUSIC_TWEAK_COUNT = 32;

struct MusicTweaks {
  u32 TweakCount;

  struct {
    char MusicName[12];
    u32 VolumeAdjust;
  } MusicTweak[MUSIC_TWEAK_COUNT];
};

extern MusicTweaks gMusicTweakInfo;

#endif  // JAK_V2_SRPC_H
