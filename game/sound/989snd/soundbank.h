#pragma once
#include <memory>
#include <optional>
#include <string_view>

#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

#include "../common/synth.h"
#include "game/sound/989snd/loader.h"

namespace snd {

struct SndPlayParams {
  std::optional<s32> vol;
  std::optional<s32> pan;
  std::optional<s32> pitch_mod;
  std::optional<s32> pitch_bend;
  std::optional<std::array<s8, 4>> registers;
};

struct BankTag {
  /*   0 */ u32 DataID;
  /*   4 */ u32 Version;
  /*   8 */ u32 Flags;
  /*   c */ u32 BankID;
};

enum class BankType {
  Music,
  SFX,
};

struct SFXUserData;
class SoundBank {
 public:
  struct BlockFlags {
    u32 flags;

    static constexpr u32 BLOCK_HAS_NAMES = 0x100;
    static constexpr u32 BLOCK_HAS_USERDATA = 0x200;

    bool hasNames() { return flags & BLOCK_HAS_NAMES; }
    bool hasUserdata() { return flags & BLOCK_HAS_USERDATA; }
  };

  virtual ~SoundBank() = default;

  u32 DataID;
  u32 Version;
  BlockFlags Flags;
  u32 BankID;
  s8 BankNum;

  virtual std::optional<std::unique_ptr<SoundHandler>>
  MakeHandler(VoiceManager& vm, u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb, s32 current_tick) {
    SndPlayParams params{};
    params.vol = vol;
    params.pan = pan;
    params.pitch_mod = pm;
    params.pitch_bend = pb;

    return MakeHandler(vm, sound_id, -1, -1, params, current_tick);
  };

  virtual std::optional<std::unique_ptr<SoundHandler>> MakeHandler(VoiceManager& vm,
                                                                   u32 sound_id,
                                                                   s32 vol,
                                                                   s32 pan,
                                                                   SndPlayParams& params,
                                                                   s32 current_tick) = 0;

  virtual std::optional<std::string_view> GetName() { return std::nullopt; };
  virtual std::optional<u32> GetSoundByName(const char* /*name*/) { return std::nullopt; };
  virtual std::optional<const SFXUserData*> GetSoundUserData(u32 /*sound_id*/) {
    return std::nullopt;
  };

  virtual void DebugPrintAllSounds() {}
};

}  // namespace snd
