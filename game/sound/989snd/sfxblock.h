#pragma once
#include <vector>
#include "soundbank.h"

namespace snd {

struct SFXBlockData : BankTag {
  /*  10 */ s8 BlockNum;
  /*  11 */ s8 pad1;
  /*  12 */ s16 pad2;
  /*  14 */ s16 pad3;
  /*  16 */ s16 NumSounds;
  /*  18 */ s16 NumGrains;
  /*  1a */ s16 NumVAGs;
  /*  1c */ u32 FirstSound;
  /*  20 */ u32 FirstGrain;
  /*  24 */ u32 VagsInSR;
  /*  28 */ u32 VagDataSize;
  /*  2c */ u32 SRAMAllocSize;
  /*  30 */ u32 NextBlock;
  /*  34 */ u32 BlockNames;
  /*  38 */ u32 SFXUD;
};

static_assert(sizeof(SFXBlockData) == 0x38 + 4);

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

/*
** Type 1 = Tone
*/

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

struct SFXData {
  /*   0 */ s8 Vol;
  /*   1 */ s8 VolGroup;
  /*   2 */ s16 Pan;
  /*   4 */ s8 NumGrains;
  /*   5 */ s8 InstanceLimit;
  /*   6 */ u16 Flags;
  /*   8 */ u32 FirstGrain;
};

enum SFXFlags {
  Looper = 1,
  SoloSound = 2,  // Stop previous instances
};

struct SFX {
  SFXData d;
  std::vector<SFXGrain> grains;
};

class SFXBlock : public SoundBank {
 public:
  SFXBlock(locator& loc) : m_locator(loc) {}
  std::unique_ptr<sound_handler> make_handler(voice_manager& vm,
                                              u32 sound_id,
                                              s32 vol,
                                              s32 pan,
                                              s32 pm,
                                              s32 pb) override;

  std::vector<SFX> sounds;

 private:
  locator& m_locator;
};

}  // namespace snd
