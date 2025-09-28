// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include <forward_list>
#include <memory>
#include <unordered_map>
#include <vector>

#include "envelope.h"
#include "sound_types.h"
#include "voice.h"

#include "common/common_types.h"

namespace snd {
struct SpuVolume {
  /*   0 */ s16 left;
  /*   2 */ s16 right;
};

class Synth {
 public:
  Synth() {
    mVolume.left.Set(0x3FFF);
    mVolume.right.Set(0x3FFF);
  }

  s16Output Tick();
  void AddVoice(std::shared_ptr<Voice> voice);
  void SetMasterVol(u32 volume);

 private:
  std::forward_list<std::shared_ptr<Voice>> mVoices;

  VolumePair mVolume{};
};
}  // namespace snd
