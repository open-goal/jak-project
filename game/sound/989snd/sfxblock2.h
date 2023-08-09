#pragma once
#include <vector>

#include "sfxgrain.h"
#include "soundbank.h"

namespace snd {

enum class SFX2BlockFlags : u32 {
  HasBlockNames = 1 << 8,
};

struct SFXBlockData2 : BankTag {
  /*  10 */ s8 BlockNum;
  /*  11 */ s8 pad1;
  /*  12 */ s16 pad2;
  /*  14 */ s16 pad3;
  /*  16 */ s16 NumSounds;
  /*  18 */ s16 NumGrains;
  /*  1a */ s16 NumVAGs;
  /*  1c */ /* SFX2Ptr */ u32 FirstSound;
  /*  20 */ /* SFXGrain2Ptr */ u32 FirstGrain;
  /*  24 */ u32 VagsInSR;
  /*  28 */ u32 VagDataSize;
  /*  2c */ u32 SRAMAllocSize;
  /*  30 */ u32 NextBlock;
  /*  34 */ u32 GrainData;  // new
  /*  38 */ /* SFXBlockNames* */ u32 BlockNames;
  /*  3c */ /* SFXUserData* */ u32 SFXUD;
};

static_assert(sizeof(SFXBlockData2) == 0x3c + 4);

struct SFXUserData {
  /*   0 */ u32 data[4];
};

struct SFXName {
  /*   0 */ u32 Name[4];
  /*  10 */ s16 Index;
  /*  12 */ s16 reserved;
};

struct VAGName {
  /*   0 */ u32 Name[4];
  /*  10 */ u32 Offset;
  /*  14 */ u32 res1;
  /*  18 */ u32 res2;
};

struct VAGImport {
  /*   0 */ u32 BlockName[2];
  /*   8 */ u32 VAGName[4];
  /*  18 */ u32 VAGLocation;
  /*  1c */ u32 VAGSR;
};

struct VAGExport {
  /*   0 */ u32 VAGName[4];
  /*  10 */ u32 VAGLocation;
  /*  14 */ u32 VAGSR;
};

struct SFXBlockNames {
  /*   0 */ u32 BlockName[2];
  /*   8 */ u32 SFXNameTableOffset;
  /*   c */ u32 VAGNameTableOffset;
  /*  10 */ u32 VAGImportsTableOffset;
  /*  14 */ u32 VAGExportsTableOffset;
  /*  18 */ s16 SFXHashOffsets[32];
  /*  58 */ s16 VAGHashOffsets[32];
};

struct SFX2Data {
  /*   0 */ s8 Vol;
  /*   1 */ s8 VolGroup;
  /*   2 */ s16 Pan;
  /*   4 */ s8 NumGrains;
  /*   5 */ s8 InstanceLimit;
  /*   6 */ u16 Flags;
  /*   8 */ u32 FirstGrain;
};

struct SFX2 {
  SFX2Data d;
  std::string name;
  std::vector<std::unique_ptr<Grain>> grains;
  SFXUserData user_data;
  int index;
};

class SFXBlock2 : public SoundBank {
 public:
  SFXBlock2(locator& loc, u32 handle, BankTag* tag);
  std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                             u32 sound_id,
                                                             s32 vol,
                                                             s32 pan,
                                                             SndPlayParams& params) override;

  std::optional<std::string_view> get_name() override { return m_name; };
  std::optional<u32> get_sound_by_name(const char* name) override;

  std::optional<const SFXUserData*> get_sound_user_data(u32 sound_id) override {
    return &m_sounds.at(sound_id).user_data;
  };

 private:
  [[maybe_unused]] locator& m_locator;
  std::string m_name;
  std::unordered_map<std::string, u32> m_names;
  std::vector<SFX2> m_sounds;
};

}  // namespace snd
