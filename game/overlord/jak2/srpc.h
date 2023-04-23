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

void srpc_init_globals();

}