#pragma once

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

struct SoundRpcCommand {
  u16 rsvd1;
  SoundCommand command;
  union {
    SoundRpcGetIrxVersion irx_version;
    SoundRpcBankCommand load_bank;
    SoundRpcSetLanguageCommand set_language;
  };
};

extern MusicTweaks gMusicTweakInfo;

u32 Thread_Loader();