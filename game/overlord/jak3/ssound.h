#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/rpc_interface.h"

namespace jak3 {
void jak3_overlord_init_globals_ssound();
void InitSound();

extern s32 g_n989Semaphore;
extern bool g_bSoundEnable;

struct SoundInfo {
  SoundName name;
  s32 id;
  s32 sound_handle;
  s32 new_volume;
  s32 auto_time;
  SoundPlayParams params;
};

struct VolumePair {
  s16 left;
  s16 right;
};

SoundInfo* LookupSound(s32 id);
SoundInfo* AllocateSound();
int GetFalloffCurve(int fo_curve);
s32 GetVolume(SoundInfo* sound);
s32 GetPan(SoundInfo* sound);
void UpdateVolume(SoundInfo* sound);
void KillSoundsInGroup(u32 group);
void SetEarTrans(const s32* ear_trans0,
                 const s32* ear_trans1,
                 const s32* cam_trans,
                 const s32* cam_forward,
                 const s32* cam_left,
                 s32 cam_scale,
                 bool cam_inverted);
void SetPlaybackMode(s32 mode);
void SetCurve(int curve_idx, u32, u32, uint8_t, uint8_t, uint8_t, uint8_t, uint8_t);
u32 CalculateFalloffVolume(s32* trans, u32 vol, u32 fo_curve, u32 fo_min, u32 fo_max, u32*, u32*);
s32 CalculateAngle(s32* trans, u32 fo_curve, u32);
void PrintSounds();

extern u32 g_anStreamVoice[6];
extern VolumePair g_aPanTable[361];
extern bool g_CameraInvert;
}  // namespace jak3