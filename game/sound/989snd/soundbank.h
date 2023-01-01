#pragma once
#include <memory>
#include <optional>

#include "locator.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

#include "../common/synth.h"

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
  SoundBank(u32 id, BankType type) : type(type), bank_id(id){};
  virtual ~SoundBank() = default;

  virtual std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                                     u32 sound_id,
                                                                     s32 vol,
                                                                     s32 pan,
                                                                     s32 pm,
                                                                     s32 pb) {
    SndPlayParams params{};
    params.vol = vol;
    params.pan = pan;
    params.pitch_mod = pm;
    params.pitch_bend = pb;

    return make_handler(vm, sound_id, -1, -1, params);
  };

  virtual std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                                     u32 sound_id,
                                                                     s32 vol,
                                                                     s32 pan,
                                                                     SndPlayParams& params) = 0;

  virtual std::optional<std::string_view> get_name() { return std::nullopt; };
  virtual std::optional<u32> get_sound_by_name(const char* /*name*/) { return std::nullopt; };
  virtual std::optional<const SFXUserData*> get_sound_user_data(u32 /*sound_id*/) {
    return std::nullopt;
  };

  BankType type;
  u32 bank_id;
  u32 bank_name;
  std::unique_ptr<u8[]> sampleBuf;
};

}  // namespace snd
