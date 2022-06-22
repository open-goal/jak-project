#pragma once
#include <memory>

#include "locator.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

#include "../common/synth.h"

namespace snd {
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

class SoundBank {
 public:
  virtual ~SoundBank() = default;
  BankType type;
  virtual std::unique_ptr<sound_handler> make_handler(voice_manager& vm,
                                                      u32 sound_id,
                                                      s32 vol,
                                                      s32 pan,
                                                      s32 pm,
                                                      s32 pb) = 0;

  u32 bank_id;
  u32 bank_name;
  std::unique_ptr<u8[]> sampleBuf;
};

}  // namespace snd
