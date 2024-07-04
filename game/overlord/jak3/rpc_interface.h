#pragma once

#include "common/common_types.h"

namespace jak3 {

enum RpcId {
  Player = 0xfab0,    // sound effects playback
  Loader = 0xfab1,    // sound effects loading.
  LoadToEE = 0xfab2,  // was ramdisk, now just a simple way to load a file to EE memory.
  DGO = 0xfab3,       // level/engine .DGO loading
  STR = 0xfab4,       // loading .str files of animations or other streamed data
  PLAY = 0xfab5,      // playing and queueing vag streams
};

// RAMDISK RPC (renamed to LoadToEE for jak 3, kinda)
struct RpcLoadToEEMsg {
  u32 unk;
  u32 addr;
  u32 unk2;
  u32 length;
  char name[16];
};
static_assert(sizeof(RpcLoadToEEMsg) == 32);

enum LoadToEEFno {
  LOAD_FILE = 4,
};

struct RPC_Dgo_Cmd {
  uint16_t rsvd;
  uint16_t status;
  uint32_t buffer1;
  uint32_t buffer2;
  uint32_t buffer_heap_top;
  char name[16];
  uint16_t cgo_id;
  uint8_t pad[30];
};
static_assert(sizeof(RPC_Dgo_Cmd) == 0x40);

enum DgoFno {
  LOAD = 0,
  LOAD_NEXT = 1,
  CANCEL = 2,
};

struct RPC_Str_Cmd {
  u16 rsvd;
  u16 result;  // 2
  u32 address;
  s32 section;  // 8
  u32 maxlen;
  u32 dummy[4];
  char basename[48];  // 32
};

struct SoundStreamName {
  char chars[48];
};

struct RPC_Play_Cmd {
  u16 rsvd;
  u16 result;
  u32 address;
  u32 section;
  u32 maxlen;
  u32 id[4];
  SoundStreamName names[4];
  u32 pad[8];
};

struct SoundName {
  char data[16];
};

enum class SoundCommand : u16 {
  // IOP_STORE = 0,
  // IOP_FREE = 1,
  // LOAD_BANK = 2,
  // LOAD_BANK_FROM_IOP = 3,
  // LOAD_BANK_FROM_EE = 4,
  // LOAD_MUSIC = 5,
  // UNLOAD_BANK = 6,
  PLAY = 7,
  PAUSE_SOUND = 8,
  STOP_SOUND = 9,
  CONTINUE_SOUND = 10,
  SET_PARAM = 11,
  SET_MASTER_VOLUME = 12,
  PAUSE_GROUP = 13,
  STOP_GROUP = 14,
  CONTINUE_GROUP = 15,
  // GET_IRX_VERSION = 16,
  // SET_FALLOFF_CURVE = 17,
  // SET_SOUND_FALLOFF = 18,
  // RELOAD_INFO = 19,
  // SET_LANGUAGE = 20,
  // SET_FLAVA = 21,
  // SET_MIDI_REG = 22,
  SET_REVERB = 23,
  SET_EAR_TRANS = 24,
  SHUTDOWN = 25,
  // LIST_SOUNDS = 26,
  // UNLOAD_MUSIC = 27,
  SET_FPS = 28,
  // BOOT_LOAD = 29,
  // GAME_LOAD = 30,
  // NUM_TESTS = 31,
  // NUM_TESTRUNS = 32,
  // NUM_SECTORS = 33,
  // NUM_STREAMSECTORS = 34,
  // NUM_STREMBANKS = 35,
  // TRACK_PITCH = 36,
  // LINVEL_NOM = 37,
  CANCEL_DGO = 49,

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
  s32 trans[3];
  u8 group;
  u8 reg[3];
};

struct Rpc_Player_Base_Cmd {
  u16 rsvd1 = 0;
  SoundCommand command;
};
static_assert(sizeof(Rpc_Player_Base_Cmd) == 4);

struct Rpc_Player_Sound_Cmd : public Rpc_Player_Base_Cmd {
  s32 sound_id = 0;
};
static_assert(sizeof(Rpc_Player_Sound_Cmd) == 8);

struct Rpc_Player_Group_Cmd : public Rpc_Player_Base_Cmd {
  u32 group = 0;
};
static_assert(sizeof(Rpc_Player_Group_Cmd) == 8);

struct Rpc_Player_Play_Cmd : public Rpc_Player_Sound_Cmd {
  s32 pad[2];
  SoundName name;
  SoundPlayParams params;
};
static_assert(sizeof(Rpc_Player_Play_Cmd) == 0x40);

struct Rpc_Player_Set_Param_Cmd : public Rpc_Player_Sound_Cmd {
  SoundPlayParams params;
  s32 auto_time;
  s32 auto_from;
};
static_assert(sizeof(Rpc_Player_Set_Param_Cmd) == 0x30);

struct Rpc_Player_Set_Master_Volume_Cmd : public Rpc_Player_Group_Cmd {
  s32 volume;
};
static_assert(sizeof(Rpc_Player_Set_Master_Volume_Cmd) == 12);

struct Rpc_Player_Set_Ear_Trans_Cmd : public Rpc_Player_Base_Cmd {
  s32 ear_trans1[3];
  s32 ear_trans0[3];
  s32 ear_trans[3];
  s32 cam_forward[3];
  s32 cam_left[3];
  s32 cam_scale;
  s32 cam_inverted;
};
static_assert(sizeof(Rpc_Player_Set_Ear_Trans_Cmd) == 0x48);

struct Rpc_Player_Set_Fps_Cmd : public Rpc_Player_Base_Cmd {
  u8 fps;
  u8 pad;
};
static_assert(sizeof(Rpc_Player_Set_Fps_Cmd) == 5 + 1);

struct Rpc_Player_Cancel_Dgo_Cmd : public Rpc_Player_Group_Cmd {
  u32 id;
};
static_assert(sizeof(Rpc_Player_Cancel_Dgo_Cmd) == 12);

constexpr int kPlayerCommandStride = 0x50;
}  // namespace jak3