#pragma once
#include <vector>

#include "sfxgrain.h"
#include "soundbank.h"

#include "sfxblock2.h"

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

  /* these last ones are probably not in jak1?  */
  /*  34 */ u32 BlockNames;
  /*  38 */ u32 SFXUD;
};

static_assert(sizeof(SFXBlockData) == 0x38 + 4);

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
  std::vector<std::unique_ptr<Grain>> grains;
};

class SFXBlock : public SoundBank {
 public:
  SFXBlock(locator& loc, u32 handle, BankTag* tag);
  std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                             u32 sound_id,
                                                             s32 vol,
                                                             s32 pan,
                                                             SndPlayParams& params) override;

 private:
  [[maybe_unused]] locator& m_locator;
  std::vector<SFX2> m_sounds;
};

}  // namespace snd
