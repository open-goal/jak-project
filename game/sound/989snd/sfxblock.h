#pragma once
#include <span>
#include <string_view>
#include <vector>

#include "sfxgrain.h"
#include "soundbank.h"

namespace snd {

struct SFXUserData {  // 0x10
  /* 0x0 */ u32 data[4];
};

class SFXBlock : public SoundBank {
 public:
  struct SFXFlags {
    u16 flags;

    static constexpr u32 SFX_LOOP = 1;
    static constexpr u32 SFX_SOLO = 2;
    static constexpr u32 SFX_INSTLIMIT = 8;
    static constexpr u32 SFX_INSTLIMIT_VOL = 0x10;
    static constexpr u32 SFX_INSTLIMIT_TICK = 0x20;

    bool has_loop() { return flags & SFX_LOOP; }
    bool solo() { return flags & SFX_SOLO; }
    bool has_instlimit() { return flags & SFX_INSTLIMIT; }
    bool instlimit_vol() { return flags & SFX_INSTLIMIT_VOL; }
    bool instlimit_tick() { return flags & SFX_INSTLIMIT_TICK; }
  };

  struct SFX {
    s8 Vol;
    s8 VolGroup;
    s16 Pan;
    s8 InstanceLimit;
    SFXFlags Flags;

    std::vector<Grain> Grains;
    SFXUserData UserData;
  };

  std::vector<SFX> Sounds;
  std::string Name;
  std::map<std::string, u32> Names;
  std::unique_ptr<u8[]> SampleData;

  static SFXBlock* ReadBlock(std::span<u8> bank_data, std::span<u8> samples);

  std::optional<std::unique_ptr<SoundHandler>> MakeHandler(VoiceManager& vm,
                                                           u32 sound_id,
                                                           s32 vol,
                                                           s32 pan,
                                                           SndPlayParams& params,
                                                           s32 current_tick) override;

  std::optional<std::string_view> GetName() override { return Name; };
  std::optional<u32> GetSoundByName(const char* name) override;
  std::optional<const SFXUserData*> GetSoundUserData(u32 sound_id) override {
    return &Sounds.at(sound_id).UserData;
  };
  void DebugPrintAllSounds() override;
};

}  // namespace snd
