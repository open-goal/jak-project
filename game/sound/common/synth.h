// Copyright: 2021 - 2022, Ziemas
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

class synth {
 public:
  synth() {
    m_Volume.left.Set(0x3FFF);
    m_Volume.right.Set(0x3FFF);
  }

  s16_output tick();
  void add_voice(std::shared_ptr<voice> voice);
  void set_master_vol(u32 volume);

 private:
  std::forward_list<std::shared_ptr<voice>> m_voices;

  VolumePair m_Volume{};
};
}  // namespace snd
