#pragma once
#include "common/common_types.h"

#include "game/overlord/common/sbank.h"

extern s32 gMusicFade;
extern s32 gSema;
extern s32 gMusicFadeDir;

struct VolumePair {
  s16 left;
  s16 right;
};

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

extern Sound gSounds[64];
extern Vec3w gEarTrans[2];
extern Curve gCurve[16];
extern Vec3w gCamTrans;
extern u8 gMirrorMode;
extern s32 gCamAngle;
extern u32 sLastTick;

void ssound_init_globals();

Sound* LookupSound(s32 id);
Sound* AllocateSound();
s32 GetVolume(Sound* sound);
void UpdateVolume(Sound* sound);
s32 CalculateFallofVolume(Vec3w* pos, s32 volume, s32 fo_curve, s32 fo_min, s32 fo_max);
s32 GetPan(Sound* sound);
s32 CalculateAngle(Vec3w* trans);
void KillSoundsInGroup(u8 group);
void UpdateLocation(Sound* sound);
void UpdateAutoVol(Sound* sound, s32 ticks);
void PrintActiveSounds();
void SetCurve(s32 curve, s32 fallof, s32 ease);
