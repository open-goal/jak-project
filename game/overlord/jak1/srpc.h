#pragma once

#include "ssound.h"

#include "common/common_types.h"

#include "game/overlord/common/srpc.h"

namespace jak1 {
void srpc_init_globals();

extern s32 gVAG_Id;

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
  UNLOAD_MUSIC = 22,
  MIRROR_MODE = 201,
};

struct SoundRpcCommand {
  u16 rsvd1;
  union {
    Jak1SoundCommand j1command;
    // Jak2SoundCommand j2command;
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
    SoundRpcSetMirrror mirror;
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

u32 Thread_Loader();
u32 Thread_Player();

s32 VBlank_Handler(void*);

}  // namespace jak1