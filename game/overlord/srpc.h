#pragma once

#include "ssound.h"
#include "common/common_types.h"

void srpc_init_globals();

constexpr int MUSIC_TWEAK_COUNT = 32;

struct MusicTweaks {
  u32 TweakCount;

  struct {
    char MusicName[12];
    s32 VolumeAdjust;
  } MusicTweak[MUSIC_TWEAK_COUNT];
};

enum class SoundCommand : u16 {
  LOAD_BANK = 0,
  LOAD_MUSIC = 1,
  UNLOAD_BANK = 2,
  PLAY = 3,
  PAUSE_SOUND = 4,
  STOP_SOUND = 5,
  CONTINUE_SOUND = 6,
  SET_PARAM = 7,
  SET_MASTER_VOLUME = 8,
  PAUSE_GROUP = 9,
  STOP_GROUP = 10,
  CONTINUE_GROUP = 11,
  GET_IRX_VERSION = 12,
  SET_FALLOFF_CURVE = 13,
  SET_SOUND_FALLOFF = 14,
  RELOAD_INFO = 15,
  SET_LANGUAGE = 16,
  SET_FLAVA = 17,
  SET_REVERB = 18,
  SET_EAR_TRANS = 19,
  SHUTDOWN = 20,
  LIST_SOUNDS = 21,
  UNLOAD_MUSIC = 22
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

struct SoundRpcCommand {
  u16 rsvd1;
  SoundCommand command;
  union {
    SoundRpcGetIrxVersion irx_version;
    SoundRpcBankCommand load_bank;
    SoundRpcSetLanguageCommand set_language;
    SoundRpcPlayCommand play;
    SoundRpcSoundIdCommand sound_id;
    SoundRpcSetFPSCommand fps;
    SoundRpcSetEarTrans ear_trans;
    SoundRpcSetReverb reverb;
    SoundRpcSetFallof fallof;
    SoundRpcSetFallofCurve fallof_curve;
    SoundRpcGroupCommand group;
    SoundRpcSetFlavaCommand flava;
    SoundRpcMasterVolCommand master_volume;
    SoundRpcSetParamCommand param;
    u8 max_size[0x4C];  // Temporary
  };
};

static_assert(sizeof(SoundRpcCommand) == 0x50);

extern MusicTweaks gMusicTweakInfo;
extern s32 gMusicTweak;

u32 Thread_Loader();
u32 Thread_Player();

s32 VBlank_Handler();
