#pragma once

#include "common/common_types.h"

#include "game/overlord/common/srpc.h"

namespace jak2 {

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
  mirror_mode = 201,
};

struct SoundRpcCommand {
  u16 rsvd1;
  Jak2SoundCommand j2command;
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

/*
 *
  ((frame uint32 :offset-assert 0)
   (strpos int32 :offset-assert 4)
   (str-id uint32 :offset-assert 8)
   (str-id-sign int32 :offset 8)
   (freemem uint32 :offset-assert 12)
   (chinfo uint8 48 :offset-assert 16)
   (freemem2 uint32 :offset-assert 64)
   (nocd uint32 :offset-assert 68)
   (dirtycd uint32 :offset-assert 72)
   (diskspeed uint32 2 :offset-assert 76)
   (lastspeed uint32 :offset-assert 84)
   (dupseg int32 :offset-assert 88)
   (times int32 41 :offset-assert 92)
   (times-seq uint32 :offset-assert 256)
   (iop-ticks uint32 :offset-assert 260)
   (stream-position uint32 4 :offset 272)
   (stream-status stream-status 4 :offset-assert 288)
   (stream-name sound-stream-name 4 :inline :offset-assert 304)
   (stream-id sound-id 4 :offset-assert 496)
   (stream-id-int32 int32 4 :offset 496) ;; ughhhh...
   (music-register uint8 17 :offset-assert 512)
   (music-excite int8 :offset 528)
   (ramdisk-name uint8 48 :offset-assert 529)
 */
struct StreamName {
  char dat[48];
};
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
  u32 iop_ticks;
  u32 p;
  u32 q;
  s32 stream_position[4];
  s32 stream_status[4];
  StreamName stream_name[4];
  s32 stream_id[4];
  u8 music_register[17];
  char ramdisk_name[48];
  char pad[3];
  char more_padding[12];
};
static_assert(sizeof(SoundIopInfo) == 0x250);

struct VagCmd;
void SetVagStreamName(VagCmd* param_1, int param_2, int param_3);
void srpc_init_globals();
int VBlank_Handler(void*);
u32 Thread_Player();
u32 Thread_Loader();

}  // namespace jak2