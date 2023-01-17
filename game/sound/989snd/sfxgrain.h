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

struct LargestGrainParamStruct {
  /*   0 */ char blank[32];
};

struct SFXGrain {
  /*   0 */ u32 Type;
  /*   4 */ s32 Delay;
  union {
    /*   8 */ Tone tone;
    /*   8 */ XREFGrainParams xref;
    /*   8 */ RandDelayParams delay;
    /*   8 */ ControlParams control;
    /*   8 */ LFOParams lfo;
    /*   8 */ PlaySoundParams play_sound;
    /*   8 */ PluginParams plugin_params;
    /*   8 */ LargestGrainParamStruct junk;
  } GrainParams;
};

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

class blocksound_handler;

class Grain {
 public:
  Grain(SFXGrain& grain) : m_type((grain_type)grain.Type), m_delay(grain.Delay) {}
  Grain(SFXGrain2& grain) : m_type((grain_type)grain.OpcodeData.type), m_delay(grain.Delay) {}
  Grain(SFXGrain2& grain, [[maybe_unused]] u8* data)
      : m_type((grain_type)grain.OpcodeData.type), m_delay(grain.Delay) {}

  virtual ~Grain() = default;

  virtual s32 execute(blocksound_handler& /*handler*/) { return 0; };
  virtual std::string_view inspect() { return magic_enum::enum_name(type()); };
  s32 delay() { return m_delay; }
  grain_type type() { return m_type; }
  std::array<int, 4>& args() { return m_args; }

 private:
  std::array<int, 4> m_args;
  grain_type m_type{0};
  s32 m_delay{0};
};

class SFXGrain_Null : public Grain {
 public:
  SFXGrain_Null(SFXGrain& grain) : Grain(grain){};
  SFXGrain_Null(SFXGrain2& grain, [[maybe_unused]] u8* data) : Grain(grain){};
};

class SFXGrain_Tone : public Grain {
 public:
  SFXGrain_Tone(SFXGrain& grain);
  SFXGrain_Tone(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

  Tone m_tone;
};

class SFXGrain_XrefID : public Grain {
 public:
  SFXGrain_XrefID(SFXGrain& grain) : Grain(grain){};
  SFXGrain_XrefID(SFXGrain2& grain, [[maybe_unused]] u8* data) : Grain(grain){};
};

class SFXGrain_XrefNum : public Grain {
 public:
  SFXGrain_XrefNum(SFXGrain& grain) : Grain(grain){};
  SFXGrain_XrefNum(SFXGrain2& grain, [[maybe_unused]] u8* data) : Grain(grain){};
};

class SFXGrain_LfoSettings : public Grain {
 public:
  SFXGrain_LfoSettings(SFXGrain& grain);
  SFXGrain_LfoSettings(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  LFOParams m_lfop{};
};

class SFXGrain_StartChildSound : public Grain {
 public:
  SFXGrain_StartChildSound(SFXGrain& grain);
  SFXGrain_StartChildSound(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  PlaySoundParams m_psp{};
};

class SFXGrain_StopChildSound : public Grain {
 public:
  SFXGrain_StopChildSound(SFXGrain& grain);
  SFXGrain_StopChildSound(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  PlaySoundParams m_psp{};
};

class SFXGrain_PluginMessage : public Grain {
 public:
  SFXGrain_PluginMessage(SFXGrain& grain);
  SFXGrain_PluginMessage(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_Branch : public Grain {
 public:
  SFXGrain_Branch(SFXGrain& grain);
  SFXGrain_Branch(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_ControlNull : public Grain {
 public:
  SFXGrain_ControlNull(SFXGrain& grain) : Grain(grain){};
  SFXGrain_ControlNull(SFXGrain2& grain, u8* /*data*/) : Grain(grain){};
};

class SFXGrain_LoopStart : public Grain {
 public:
  SFXGrain_LoopStart(SFXGrain& grain) : Grain(grain){};
  SFXGrain_LoopStart(SFXGrain2& grain, u8* /*data*/) : Grain(grain){};
};

class SFXGrain_LoopEnd : public Grain {
 public:
  SFXGrain_LoopEnd(SFXGrain& grain);
  SFXGrain_LoopEnd(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_LoopContinue : public Grain {
 public:
  SFXGrain_LoopContinue(SFXGrain& grain);
  SFXGrain_LoopContinue(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_Stop : public Grain {
 public:
  SFXGrain_Stop(SFXGrain& grain);
  SFXGrain_Stop(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_RandPlay : public Grain {
 public:
  SFXGrain_RandPlay(SFXGrain& grain);
  SFXGrain_RandPlay(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int options{0};
  int count{0};
  int previous{0};
};

class SFXGrain_RandDelay : public Grain {
 public:
  SFXGrain_RandDelay(SFXGrain& grain);
  SFXGrain_RandDelay(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_max{0};
};

class SFXGrain_RandPB : public Grain {
 public:
  SFXGrain_RandPB(SFXGrain& grain);
  SFXGrain_RandPB(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_pb{0};
};

class SFXGrain_PB : public Grain {
 public:
  SFXGrain_PB(SFXGrain& grain);
  SFXGrain_PB(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_pb{0};
};

class SFXGrain_AddPB : public Grain {
 public:
  SFXGrain_AddPB(SFXGrain& grain);
  SFXGrain_AddPB(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_pb{0};
};

class SFXGrain_SetRegister : public Grain {
 public:
  SFXGrain_SetRegister(SFXGrain& grain);
  SFXGrain_SetRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_reg{0};
  int m_value{0};
};

class SFXGrain_SetRegisterRand : public Grain {
 public:
  SFXGrain_SetRegisterRand(SFXGrain& grain);
  SFXGrain_SetRegisterRand(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_reg{0};
  int m_lower_bound{0};
  int m_upper_bound{0};
};

class SFXGrain_IncRegister : public Grain {
 public:
  SFXGrain_IncRegister(SFXGrain& grain);
  SFXGrain_IncRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_reg{0};
};

class SFXGrain_DecRegister : public Grain {
 public:
  SFXGrain_DecRegister(SFXGrain& grain);
  SFXGrain_DecRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_reg{0};
};

class SFXGrain_TestRegister : public Grain {
 public:
  SFXGrain_TestRegister(SFXGrain& grain);
  SFXGrain_TestRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_reg{0};
  int m_cmp{0};
  int m_action{0};
};

class SFXGrain_Marker : public Grain {
 public:
  SFXGrain_Marker(SFXGrain& grain) : Grain(grain), m_mark(grain.GrainParams.control.param[0]) {}
  SFXGrain_Marker(SFXGrain2& grain, u8* /*data*/) : Grain(grain), m_mark(grain.OpcodeData.arg[0]) {}
  int marker() { return m_mark; }

 private:
  int m_mark{0};
};

class SFXGrain_GotoMarker : public Grain {
 public:
  SFXGrain_GotoMarker(SFXGrain& grain);
  SFXGrain_GotoMarker(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_mark{0};
};

class SFXGrain_GotoRandomMarker : public Grain {
 public:
  SFXGrain_GotoRandomMarker(SFXGrain& grain);
  SFXGrain_GotoRandomMarker(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_upper_bound{0};
  int m_lower_bound{0};
};

class SFXGrain_WaitForAllVoices : public Grain {
 public:
  SFXGrain_WaitForAllVoices(SFXGrain& grain);
  SFXGrain_WaitForAllVoices(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_PlayCycle : public Grain {
 public:
  SFXGrain_PlayCycle(SFXGrain& grain);
  SFXGrain_PlayCycle(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_group_size;
  int m_group_count;
  int m_index;
};

class SFXGrain_AddRegister : public Grain {
 public:
  SFXGrain_AddRegister(SFXGrain& grain);
  SFXGrain_AddRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_val{0};
  int m_reg{0};
};

class SFXGrain_KeyOffVoices : public Grain {
 public:
  SFXGrain_KeyOffVoices(SFXGrain& grain);
  SFXGrain_KeyOffVoices(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_KillVoices : public Grain {
 public:
  SFXGrain_KillVoices(SFXGrain& grain);
  SFXGrain_KillVoices(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_OnStopMarker : public Grain {
 public:
  SFXGrain_OnStopMarker(SFXGrain& grain);
  SFXGrain_OnStopMarker(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;
};

class SFXGrain_CopyRegister : public Grain {
 public:
  SFXGrain_CopyRegister(SFXGrain& grain);
  SFXGrain_CopyRegister(SFXGrain2& grain, u8* data);
  s32 execute(blocksound_handler& handler) override;

 private:
  int m_src{0};
  int m_dst{0};
};

template <typename... Args>
std::unique_ptr<Grain> new_grain(grain_type id, Args&&... args) {
  switch (id) {
    case grain_type::NULL_GRAIN:
      return std::make_unique<SFXGrain_Null>(std::forward<Args>(args)...);
    case grain_type::TONE:
    case grain_type::TONE2:
      return std::make_unique<SFXGrain_Tone>(std::forward<Args>(args)...);
    case grain_type::XREF_ID:
      return std::make_unique<SFXGrain_XrefID>(std::forward<Args>(args)...);
    case grain_type::XREF_NUM:
      return std::make_unique<SFXGrain_XrefNum>(std::forward<Args>(args)...);
    case grain_type::LFO_SETTINGS:
      return std::make_unique<SFXGrain_LfoSettings>(std::forward<Args>(args)...);
    case grain_type::STARTCHILDSOUND:
      return std::make_unique<SFXGrain_StartChildSound>(std::forward<Args>(args)...);
    case grain_type::STOPCHILDSOUND:
      return std::make_unique<SFXGrain_StopChildSound>(std::forward<Args>(args)...);
    case grain_type::PLUGIN_MESSAGE:
      return std::make_unique<SFXGrain_PluginMessage>(std::forward<Args>(args)...);
    case grain_type::BRANCH:
      return std::make_unique<SFXGrain_Branch>(std::forward<Args>(args)...);
    case grain_type::CONTROL_NULL:
      return std::make_unique<SFXGrain_ControlNull>(std::forward<Args>(args)...);
    case grain_type::LOOP_START:
      return std::make_unique<SFXGrain_LoopStart>(std::forward<Args>(args)...);
    case grain_type::LOOP_END:
      return std::make_unique<SFXGrain_LoopEnd>(std::forward<Args>(args)...);
    case grain_type::LOOP_CONTINUE:
      return std::make_unique<SFXGrain_LoopContinue>(std::forward<Args>(args)...);
    case grain_type::STOP:
      return std::make_unique<SFXGrain_Stop>(std::forward<Args>(args)...);
    case grain_type::RAND_PLAY:
      return std::make_unique<SFXGrain_RandPlay>(std::forward<Args>(args)...);
    case grain_type::RAND_DELAY:
      return std::make_unique<SFXGrain_RandDelay>(std::forward<Args>(args)...);
    case grain_type::RAND_PB:
      return std::make_unique<SFXGrain_RandPB>(std::forward<Args>(args)...);
    case grain_type::PB:
      return std::make_unique<SFXGrain_PB>(std::forward<Args>(args)...);
    case grain_type::ADD_PB:
      return std::make_unique<SFXGrain_AddPB>(std::forward<Args>(args)...);
    case grain_type::SET_REGISTER:
      return std::make_unique<SFXGrain_SetRegister>(std::forward<Args>(args)...);
    case grain_type::SET_REGISTER_RAND:
      return std::make_unique<SFXGrain_SetRegisterRand>(std::forward<Args>(args)...);
    case grain_type::INC_REGISTER:
      return std::make_unique<SFXGrain_IncRegister>(std::forward<Args>(args)...);
    case grain_type::DEC_REGISTER:
      return std::make_unique<SFXGrain_DecRegister>(std::forward<Args>(args)...);
    case grain_type::TEST_REGISTER:
      return std::make_unique<SFXGrain_TestRegister>(std::forward<Args>(args)...);
    case grain_type::MARKER:
      return std::make_unique<SFXGrain_Marker>(std::forward<Args>(args)...);
    case grain_type::GOTO_MARKER:
      return std::make_unique<SFXGrain_GotoMarker>(std::forward<Args>(args)...);
    case grain_type::GOTO_RANDOM_MARKER:
      return std::make_unique<SFXGrain_GotoRandomMarker>(std::forward<Args>(args)...);
    case grain_type::WAIT_FOR_ALL_VOICES:
      return std::make_unique<SFXGrain_WaitForAllVoices>(std::forward<Args>(args)...);
    case grain_type::PLAY_CYCLE:
      return std::make_unique<SFXGrain_PlayCycle>(std::forward<Args>(args)...);
    case grain_type::ADD_REGISTER:
      return std::make_unique<SFXGrain_AddRegister>(std::forward<Args>(args)...);
    case grain_type::KEY_OFF_VOICES:
      return std::make_unique<SFXGrain_KeyOffVoices>(std::forward<Args>(args)...);
    case grain_type::KILL_VOICES:
      return std::make_unique<SFXGrain_KillVoices>(std::forward<Args>(args)...);
    case grain_type::ON_STOP_MARKER:
      return std::make_unique<SFXGrain_OnStopMarker>(std::forward<Args>(args)...);
    case grain_type::COPY_REGISTER:
      return std::make_unique<SFXGrain_CopyRegister>(std::forward<Args>(args)...);
    default:
      throw std::runtime_error(fmt::format("Unknown grain type {}", fmt::underlying(id)));
  }
  return nullptr;
}

}  // namespace snd
