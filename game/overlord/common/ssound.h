#pragma once
#include "common/common_types.h"

extern s32 gMusicFade;
extern s32 gSema;

struct Vec3w {
  s32 x;
  s32 y;
  s32 z;
};

void ssound_init_globals();