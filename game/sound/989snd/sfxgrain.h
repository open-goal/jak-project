#pragma once
#include "sfxblock.h"

#include "common/common_types.h"

#include "game/sound/989snd/vagvoice.h"

namespace snd {

struct SFXGrain2 {
  union {
    struct {
      s8 arg[3];
      u8 type;
    };

    u32 Opcode;
  } OpcodeData;

  s32 Delay;
};

enum class grain_type : u32 {
  NULL_GRAIN,
  TONE,
  XREF_ID,
  XREF_NUM,
  LFO_SETTINGS,
  STARTCHILDSOUND,
  STOPCHILDSOUND,
  PLUGIN_MESSAGE,
  BRANCH,
  CONTROL_NULL,
  LOOP_START,
  LOOP_END,
  LOOP_CONTINUE,
  STOP,
  RAND_PLAY,
  RAND_DELAY,
  RAND_PB,
  PB,
  ADD_PB,
  SET_REGISTER,
  SET_REGISTER_RAND,
  INC_REGISTER,
  DEC_REGISTER,
  TEST_REGISTER,
  MARKER,
  GOTO_MARKER,
  GOTO_RANDOM_MARKER,
  WAIT_FOR_ALL_VOICES,
  PLAY_CYCLE,
  ADD_REGISTER,
  KEY_OFF_VOICES,
  KILL_VOICES,
  ON_STOP_MARKER,
  COPY_REGISTER,
};

class blocksound_handler2;

class Grain {
 public:
  virtual void execute(blocksound_handler2& handler) = 0;
  s32 delay() { return m_delay; }

  s32 m_delay{0};
};

class SFXGrain_Null : Grain {
 public:
  SFXGrain_Null() = default;
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_Tone : Grain {
 public:
  SFXGrain_Tone(SFXGrain& grain);
  SFXGrain_Tone(SFXGrain2& grain, u8* data);
  void execute(blocksound_handler2& handler) override;

  Tone m_tone;
};

class SFXGrain_XrefID : Grain {
 public:
  SFXGrain_XrefID();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_XrefNum : Grain {
 public:
  SFXGrain_XrefNum();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_LfoSettings : Grain {
 public:
  SFXGrain_LfoSettings();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_StartChildSound : Grain {
 public:
  SFXGrain_StartChildSound();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_StopChildSound : Grain {
 public:
  SFXGrain_StopChildSound();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_PluginMessage : Grain {
 public:
  SFXGrain_PluginMessage();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_Branch : Grain {
 public:
  SFXGrain_Branch();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_ControlNull : Grain {
 public:
  SFXGrain_ControlNull();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_LoopStart : Grain {
 public:
  SFXGrain_LoopStart();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_LoopEnd : Grain {
 public:
  SFXGrain_LoopEnd();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_LoopContinue : Grain {
 public:
  SFXGrain_LoopContinue();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_Stop : Grain {
 public:
  SFXGrain_Stop();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_RandPlay : Grain {
 public:
  SFXGrain_RandPlay();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_RandDelay : Grain {
 public:
  SFXGrain_RandDelay();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_RandPB : Grain {
 public:
  SFXGrain_RandPB();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_PB : Grain {
 public:
  SFXGrain_PB();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_AddPB : Grain {
 public:
  SFXGrain_AddPB();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_SetRegister : Grain {
 public:
  SFXGrain_SetRegister();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_SetRegisterRand : Grain {
 public:
  SFXGrain_SetRegisterRand();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_IncRegister : Grain {
 public:
  SFXGrain_IncRegister();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_DecRegister : Grain {
 public:
  SFXGrain_DecRegister();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_TestRegister : Grain {
 public:
  SFXGrain_TestRegister();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_Marker : Grain {
 public:
  SFXGrain_Marker();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_GotoMarker : Grain {
 public:
  SFXGrain_GotoMarker();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_GotoRandomMarker : Grain {
 public:
  SFXGrain_GotoRandomMarker();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_WaitForAllVoices : Grain {
 public:
  SFXGrain_WaitForAllVoices();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_PlayCycle : Grain {
 public:
  SFXGrain_PlayCycle();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_AddRegister : Grain {
 public:
  SFXGrain_AddRegister();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_KeyOffVoices : Grain {
 public:
  SFXGrain_KeyOffVoices();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_KillVoices : Grain {
 public:
  SFXGrain_KillVoices();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_OnStopMarker : Grain {
 public:
  SFXGrain_OnStopMarker();
  void execute(blocksound_handler2& handler) override;
};

class SFXGrain_CopyRegister : Grain {
 public:
  SFXGrain_CopyRegister();
  void execute(blocksound_handler2& handler) override;
};

}  // namespace snd
