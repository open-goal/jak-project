#pragma once

#include "ssound.h"

#include "common/common_types.h"

void srpc_init_globals();

extern const char* gLanguage;
extern s32 gVAG_Id;

constexpr int MUSIC_TWEAK_COUNT = 32;

struct MusicTweaks {
  u32 TweakCount;

  struct {
    char MusicName[12];
    s32 VolumeAdjust;
  } MusicTweak[MUSIC_TWEAK_COUNT];
};

enum class Jak1SoundCommand : u16 {
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

enum class Jak2SoundCommand : u16 {
  iop_store = 0,
  iop_free = 1,
  load_bank = 2,
  load_bank_from_iop = 3,
  load_bank_from_ee = 4,
  load_music = 5,
  unload_bank = 6,
  play = 7,
  pause_sound = 8,
  stop_sound = 9,
  continue_sound = 10,
  set_param = 11,
  set_master_volume = 12,
  pause_group = 13,
  stop_group = 14,
  continue_group = 15,
  get_irx_version = 16,
  set_falloff_curve = 17,
  set_sound_falloff = 18,
  reload_info = 19,
  set_language = 20,
  set_flava = 21,
  set_midi_reg = 22,
  set_reverb = 23,
  set_ear_trans = 24,
  shutdown = 25,
  list_sounds = 26,
  unload_music = 27,
  set_fps = 28,
  boot_load = 29,
  game_load = 30,
  num_tests = 31,
  num_testruns = 32,
  num_sectors = 33,
  num_streamsectors = 34,
  num_streambanks = 35,
  track_pitch = 36,
  linvel_nom = 37,
  linvel_stm = 38,
  seek_nom = 39,
  seek_stm = 40,
  read_seq_nom = 41,
  read_seq_stm = 42,
  read_spr_nom = 43,
  read_spr_stm = 44,
  read_spr_strn_nom = 45,
  rand_stm_abort = 46,
  rand_nom_abort = 47,
  iop_mem = 48,
  cancel_dgo = 49,
  set_stereo_mode = 50,
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
  Vec3w ear_trans2;
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

struct SoundRpcCommand {
  u16 rsvd1;
  union {
    Jak1SoundCommand j1command;
    Jak2SoundCommand j2command;
  };
  union {
    SoundRpcGetIrxVersion irx_version;
    SoundRpcBankCommand load_bank;
    SoundRpcSetLanguageCommand set_language;
    SoundRpcPlayCommand play;
    SoundRpcSoundIdCommand sound_id;
    SoundRpcSetFPSCommand fps;
    SoundRpcSetEarTrans ear_trans;
    SoundRpc2SetEarTrans ear_trans_j2;
    SoundRpcSetReverb reverb;
    SoundRpcSetFallof fallof;
    SoundRpcSetFallofCurve fallof_curve;
    SoundRpcGroupCommand group;
    SoundRpcSetFlavaCommand flava;
    SoundRpcMasterVolCommand master_volume;
    SoundRpcSetParamCommand param;
    SoundRpcStereoMode stereo_mode;
    SoundRpcSetMidiReg midi_reg;
    u8 max_size[0x4C];  // Temporary
  };
};

static_assert(sizeof(SoundRpcCommand) == 0x50);

struct SoundIopInfo {
  u32 frame;
  s32 strpos;
  u32 std_id;
  u32 freemem;
  u8 chinfo[48];
  u32 freemem2;
  u32 nocd;
  u32 dirtycd;
  u32 diskspeed[2];
  u32 lastspeed;
  s32 dupseg;
  u32 times[41];
  u32 times_seq;
  u8 pad[10];  // pad up to transfer size
};

extern MusicTweaks gMusicTweakInfo;
extern s32 gMusicTweak;

u32 Thread_Loader();
u32 Thread_Player();

s32 VBlank_Handler(void*);

// added for PC port
extern u32 gMusicFadeHack;
