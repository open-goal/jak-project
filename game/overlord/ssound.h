#pragma once
#ifndef JAK_V2_SSOUND_H
#define JAK_V2_SSOUND_H

#include "game/sce/iop.h"
#include "sbank.h"

extern s32 gSema;
extern s32 gMusicFade;
extern s32 gMusicFadeDir;
extern s32 gMusicVol;

// FIXME where to put this
struct Vec3w {
  s32 x;
  s32 y;
  s32 z;
};

struct SoundParams {
  u16 mask;
  s16 pitch_mod;
  s16 bend;
  s16 fo_min;
  s16 fo_max;
  s8 fo_curve;
  s8 priority;
  s32 volume;
  Vec3w trans;
  u8 group;
};

struct Sound {
  s32 id;
  s32 sound_handle;
  s32 is_music;
  s32 new_volume;
  s32 auto_time;
  SoundParams params;
  SoundRecord* bank_entry;
};

struct Curve {
  s32 unk1;
  s32 unk2;
  s32 unk3;
  s32 unk4;
};

struct VolumePair {
  s16 left;
  s16 right;
};

void InitSound_Overlord();
void SetCurve(s32 curve, s32 fallof, s32 ease);
void SetEarTrans(Vec3w* ear_trans, Vec3w* cam_trans, s32 cam_angle);
void KillSoundsInGroup(u8 group);
void PrintActiveSounds();
void SetMusicVol();
Sound* LookupSound(s32 id);
Sound* AllocateSound();
void UpdateVolume(Sound* sound);
s32 GetVolume(Sound* sound);
s32 GetPan(Sound* sound);

#endif  // JAK_V2_SSOUND_H
