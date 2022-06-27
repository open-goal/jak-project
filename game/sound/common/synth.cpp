// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "synth.h"

#include <stdexcept>

namespace snd {

static s16 ApplyVolume(s16 sample, s32 volume) {
  return (sample * volume) >> 15;
}

s16_output synth::tick() {
  s16_output out{};

  m_voices.remove_if([](std::shared_ptr<voice>& v) { return v->dead(); });
  for (auto& v : m_voices) {
    out += v->run();
  }

  out.left = ApplyVolume(out.left, m_Volume.left.Get());
  out.right = ApplyVolume(out.right, m_Volume.right.Get());

  m_Volume.Run();

  return out;
}

void synth::add_voice(std::shared_ptr<voice> voice) {
  m_voices.emplace_front(voice);
}

void synth::set_master_vol(u32 volume) {
  m_Volume.left.Set(volume);
  m_Volume.right.Set(volume);
}
}  // namespace snd
