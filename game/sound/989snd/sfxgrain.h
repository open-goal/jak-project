#pragma once
#include <map>

#include "common/common_types.h"
#include "common/log/log.h"

#include "game/sound/989snd/vagvoice.h"

#include "third-party/magic_enum.hpp"

namespace snd {

struct XREFGrainParams {
  /*   0 */ u32 BankID;
  /*   4 */ u32 SoundIndex;
  /*   8 */ s32 PitchMod;
  /*   c */ u32 Flags;
};

struct RandDelayParams {
  /*   0 */ s32 Amount;
};

struct ControlParams {
  /*   0 */ s16 param[4];
};

struct LFOParams {
  /*   0 */ u8 which_lfo;
  /*   1 */ u8 target;
  /*   2 */ u8 target_extra;
  /*   3 */ u8 shape;
  /*   4 */ u16 duty_cycle;
  /*   6 */ u16 depth;
  /*   8 */ u16 flags;
  /*   a */ u16 start_offset;
  /*   c */ u32 step_size;
};

struct PlaySoundParams {
  /*   0 */ s32 vol;
  /*   4 */ s32 pan;
  /*   8 */ s8 reg_settings[4];
  /*   c */ s32 sound_id;
  /*  10 */ char snd_name[16];
};

struct PluginParams {
  /*   0 */ u32 id;
  /*   4 */ u32 index;
  /*   8 */ u8 data[24];
};

enum class GrainType : u32 {
  NULL_GRAIN = 0,
  TONE = 1,
  TONE2 = 9,
  XREF_ID = 2,
  XREF_NUM = 3,
  LFO_SETTINGS = 4,
  STARTCHILDSOUND = 5,
  STOPCHILDSOUND = 6,
  PLUGIN_MESSAGE = 7,
  BRANCH = 8,
  CONTROL_NULL = 20,
  LOOP_START = 21,
  LOOP_END = 22,
  LOOP_CONTINUE = 23,
  STOP = 24,
  RAND_PLAY = 25,
  RAND_DELAY = 26,
  RAND_PB = 27,
  PB = 28,
  ADD_PB = 29,
  SET_REGISTER = 30,
  SET_REGISTER_RAND = 31,
  INC_REGISTER = 32,
  DEC_REGISTER = 33,
  TEST_REGISTER = 34,
  MARKER = 35,
  GOTO_MARKER = 36,
  GOTO_RANDOM_MARKER = 37,
  WAIT_FOR_ALL_VOICES = 38,
  PLAY_CYCLE = 39,
  ADD_REGISTER = 40,
  KEY_OFF_VOICES = 41,
  KILL_VOICES = 42,
  ON_STOP_MARKER = 43,
  COPY_REGISTER = 44,
};

class BlockSoundHandler;

struct Grain {
  GrainType Type;
  s32 Delay;
  std::variant<Tone, RandDelayParams, ControlParams, LFOParams, PlaySoundParams, PluginParams> data;
  s32 operator()(BlockSoundHandler& handler) { return (this->*func[(u32)Type])(handler); }

 private:
  s32 snd_SFX_GRAIN_TYPE_NULL(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_TONE(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_XREF_ID(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_XREF_NUM(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_LFO_SETTINGS(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_STARTCHILDSOUND(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_STOPCHILDSOUND(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_PLUGIN_MESSAGE(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_BRANCH(BlockSoundHandler& handler);
  s32 snd_SFX_UNKNOWN_GRAIN_TYPE(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_CONTROL_NULL(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_LOOP_START(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_LOOP_END(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_LOOP_CONTINUE(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_STOP(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_RAND_PLAY(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_RAND_DELAY(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_RAND_PB(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_ADD_PB(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_PB(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_SET_REGISTER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_SET_REGISTER_RAND(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_INC_REGISTER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_DEC_REGISTER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_TEST_REGISTER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_MARKER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_GOTO_MARKER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_GOTO_RANDOM_MARKER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_WAIT_FOR_ALL_VOICES(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_PLAY_CYCLE(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_ADD_REGISTER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_KEY_OFF_VOICES(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_KILL_VOICES(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_ON_STOP_MARKER(BlockSoundHandler& handler);
  s32 snd_SFX_GRAIN_TYPE_COPY_REGISTER(BlockSoundHandler& handler);

  using GrainFunc = s32 (Grain::*)(BlockSoundHandler& handler);

  static constexpr std::array<GrainFunc, 45> func = {
      &Grain::snd_SFX_GRAIN_TYPE_NULL,
      &Grain::snd_SFX_GRAIN_TYPE_TONE,
      &Grain::snd_SFX_GRAIN_TYPE_XREF_ID,
      &Grain::snd_SFX_GRAIN_TYPE_XREF_NUM,
      &Grain::snd_SFX_GRAIN_TYPE_LFO_SETTINGS,
      &Grain::snd_SFX_GRAIN_TYPE_STARTCHILDSOUND,
      &Grain::snd_SFX_GRAIN_TYPE_STOPCHILDSOUND,
      &Grain::snd_SFX_GRAIN_TYPE_PLUGIN_MESSAGE,
      &Grain::snd_SFX_GRAIN_TYPE_BRANCH,
      &Grain::snd_SFX_GRAIN_TYPE_TONE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_UNKNOWN_GRAIN_TYPE,
      &Grain::snd_SFX_GRAIN_TYPE_CONTROL_NULL,
      &Grain::snd_SFX_GRAIN_TYPE_LOOP_START,
      &Grain::snd_SFX_GRAIN_TYPE_LOOP_END,
      &Grain::snd_SFX_GRAIN_TYPE_LOOP_CONTINUE,
      &Grain::snd_SFX_GRAIN_TYPE_STOP,
      &Grain::snd_SFX_GRAIN_TYPE_RAND_PLAY,
      &Grain::snd_SFX_GRAIN_TYPE_RAND_DELAY,
      &Grain::snd_SFX_GRAIN_TYPE_RAND_PB,
      &Grain::snd_SFX_GRAIN_TYPE_PB,
      &Grain::snd_SFX_GRAIN_TYPE_ADD_PB,
      &Grain::snd_SFX_GRAIN_TYPE_SET_REGISTER,
      &Grain::snd_SFX_GRAIN_TYPE_SET_REGISTER_RAND,
      &Grain::snd_SFX_GRAIN_TYPE_INC_REGISTER,
      &Grain::snd_SFX_GRAIN_TYPE_DEC_REGISTER,
      &Grain::snd_SFX_GRAIN_TYPE_TEST_REGISTER,
      &Grain::snd_SFX_GRAIN_TYPE_MARKER,
      &Grain::snd_SFX_GRAIN_TYPE_GOTO_MARKER,
      &Grain::snd_SFX_GRAIN_TYPE_GOTO_RANDOM_MARKER,
      &Grain::snd_SFX_GRAIN_TYPE_WAIT_FOR_ALL_VOICES,
      &Grain::snd_SFX_GRAIN_TYPE_PLAY_CYCLE,
      &Grain::snd_SFX_GRAIN_TYPE_ADD_REGISTER,
      &Grain::snd_SFX_GRAIN_TYPE_KEY_OFF_VOICES,
      &Grain::snd_SFX_GRAIN_TYPE_KILL_VOICES,
      &Grain::snd_SFX_GRAIN_TYPE_ON_STOP_MARKER,
      &Grain::snd_SFX_GRAIN_TYPE_COPY_REGISTER,
  };
};

}  // namespace snd
