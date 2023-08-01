#pragma once

#include "common/common_types.h"

namespace snd {

class MusicBank;
class SoundBank;
struct MIDIBlock;

class locator {
 public:
  virtual ~locator() = default;
  virtual SoundBank* get_bank_by_handle(u32 id) = 0;
  virtual MusicBank* get_bank_by_id(u32 id) = 0;
  virtual u8* get_bank_samples(u32 id) = 0;
  virtual MIDIBlock* get_midi(u32 id) = 0;
};
}  // namespace snd
