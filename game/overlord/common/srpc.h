#pragma once
#include "common/common_types.h"

#include "game/overlord/common/ssound.h"

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

struct SoundRpcGetIrxVersion {
  u32 major;
  u32 minor;
  u32 ee_addr;
};

struct SoundRpcBankCommand {
  u8 pad[12];
  char bank_name[16];
};

struct SoundRpcSetLanguageCommand {
  u32 langauge_id;  // game_common_types.h, Language
};

struct SoundRpcPlayCommand {
  u32 sound_id;
  u32 pad[2];
  char name[16];
  SoundParams parms;
};

struct SoundRpcSetParamCommand {
  u32 sound_id;
  SoundParams parms;
  s32 auto_time;
  s32 auto_from;
};

struct SoundRpcSoundIdCommand {
  u32 sound_id;
};

struct SoundRpcSetFlavaCommand {
  u8 flava;
};

struct SoundRpcSetReverb {
  u8 core;
  s32 reverb;
  u32 left;
  u32 right;
};

struct SoundRpcSetEarTrans {
  Vec3w ear_trans;
  Vec3w cam_trans;
  s32 cam_angle;
};

struct SoundRpc2SetEarTrans {
  Vec3w ear_trans1;
  Vec3w ear_trans0;
  Vec3w cam_trans;
  s32 cam_angle;
};

struct SoundRpcSetFPSCommand {
  u8 fps;
};

struct SoundRpcSetFallof {
  u8 pad[12];
  char name[16];
  s32 curve;
  s32 min;
  s32 max;
};

struct SoundRpcSetFallofCurve {
  s32 curve;
  s32 falloff;
  s32 ease;
};

struct SoundRpcGroupCommand {
  u8 group;
};

struct SoundRpcMasterVolCommand {
  SoundRpcGroupCommand group;
  s32 volume;
};

struct SoundRpcStereoMode {
  s32 stereo_mode;
};

struct SoundRpcSetMidiReg {
  s32 reg;
  s32 value;
};

struct SoundRpcSetMirrror {
  u8 value;
};

extern s32 gMusicTweak;
extern MusicTweaks gMusicTweakInfo;
extern int32_t gSoundEnable;
extern snd::BankHandle gMusic;
extern s32 gMusicPause;
extern s32 gSoundInUse;
extern u8 gFPS;
extern const char* gLanguage;
extern u32 gFrameNum;
void srpc_init_globals();
