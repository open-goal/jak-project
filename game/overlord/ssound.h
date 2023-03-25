#pragma once
#ifndef JAK_V2_SSOUND_H
#define JAK_V2_SSOUND_H

#include "sbank.h"

#include "game/sce/iop.h"

struct VolumePair {
  s16 left;
  s16 right;
};

extern s32 gSema;
extern s32 gMusicFade;
extern s32 gMusicFadeDir;
extern s32 gMusicVol;
extern VolumePair gPanTable[361];
extern u32 gStreamSRAM;
extern u32 gTrapSRAM;
extern u8 gMirrorMode;

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
  u8 reg[3];
};

struct Sound {
  char name[16];
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

void InitSound_Overlord();
void SetCurve(s32 curve, s32 fallof, s32 ease);
void SetEarTrans(Vec3w* ear_trans1, Vec3w* ear_trans2, Vec3w* cam_trans, s32 cam_angle);
void KillSoundsInGroup(u8 group);
void PrintActiveSounds();
void SetMusicVol();
Sound* LookupSound(s32 id);
Sound* AllocateSound();
void UpdateVolume(Sound* sound);
s32 GetVolume(Sound* sound);
s32 GetPan(Sound* sound);
s32 CalculateFallofVolume(Vec3w* pos, s32 volume, s32 fo_curve, s32 fo_min, s32 fo_max);
s32 CalculateAngle(Vec3w* trans);

#endif  // JAK_V2_SSOUND_H
