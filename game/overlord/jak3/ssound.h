#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/overlord.h"

namespace jak3 {

struct Curve {
  s32 p1;
  s32 p2;
  s32 p3;
  s32 p4;
};

struct SoundPlayParams {
  u16 mask;
  s16 pitch_mod;
  s16 bend;
  s16 fo_min;
  s16 fo_max;
  s8 fo_curve;
  s8 priority;
  s32 volume;
  Vec3 trans;
  u8 group;
  u8 reg[3];
};

struct SoundInfo {
  char name[16];
  s32 id;
  u32 sound_handle;
  s32 auto_volume;
  s32 auto_time;
  SoundPlayParams params;
};

}  // namespace jak3
