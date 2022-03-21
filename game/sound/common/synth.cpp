// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "synth.h"
#include "../989snd/util.h"

namespace snd {

static s16 ApplyVolume(s16 sample, s32 volume) {
  return (sample * volume) >> 15;
}

s16_output synth::tick() {
  s16_output out{};
  for (auto& v : m_voices) {
    out += v->run();
  }

  m_voices.remove_if([](std::unique_ptr<voice>& v) { return v->dead(); });

  out.left = ApplyVolume(out.left, m_Volume.left.Get());
  out.right = ApplyVolume(out.right, m_Volume.right.Get());

  m_Volume.Run();

  return out;
}

// TODO verify
static std::pair<s16, s16> pitchbend(Tone& tone,
                                     int current_pb,
                                     int current_pm,
                                     int start_note,
                                     int start_fine) {
  auto v9 = (start_note << 7) + start_fine * current_pm;
  u32 v7;
  if (current_pb >= 0)
    v7 = tone.PBHigh * (current_pb << 7) / 0x7fff + v9;
  else
    v7 = tone.PBLow * (current_pb << 7) / 0x7fff + v9;
  return {v7 / 128, v7 % 128};
}

// Seems correct, given same input produces same output as 989snd
s16 synth::adjust_vol_to_group(s16 involume, int group) {
  s32 volume = involume;
  if (group >= 15)
    return volume;

  if (volume >= 0x7fff)
    volume = 0x7ffe;

  s32 modifier = (m_master_vol[group] * m_group_duck[group]) / 0x10000;
  volume = (volume * modifier) / 0x400;
  int sign = 1;
  if (volume < 0) {
    sign = -1;
  }

  // fmt::print("made volume {:x} -> {:x}\n", involume, volume);
  s16 retval = static_cast<s16>((volume * volume) / 0x7ffe * sign);
  return retval;
}

void synth::key_on(Tone& tone, u8 channel, u8 note, vol_pair volume, u64 owner, u32 group) {
  auto v = std::make_unique<voice>((u16*)(m_locator.get_bank_samples(tone.BankID) + tone.VAGInSR),
                                   channel, owner, note);

  s16 left = adjust_vol_to_group(volume.left, group);
  s16 right = adjust_vol_to_group(volume.right, group);

  v->set_volume(left >> 1, right >> 1);

  if ((tone.Flags & 0x10) != 0x0) {
    throw std::runtime_error("reverb only voice not handler");
  }

  auto notes = pitchbend(tone, 0, 0, note, 0);
  auto pitch = PS1Note2Pitch(tone.CenterNote, tone.CenterFine, notes.first, notes.second);
  v->set_pitch(pitch);
  v->set_asdr1(tone.ADSR1);
  v->set_asdr2(tone.ADSR2);
  v->key_on();
  m_voices.emplace_front(std::move(v));
}

void synth::key_off(u8 channel, u8 note, u64 owner) {
  for (auto& v : m_voices) {
    if (v->m_channel == channel && v->m_owner == owner && v->m_note == note) {
      v->key_off();
    }
  }
}

void synth::set_group_vol(u8 group, u32 volume) {
  m_master_vol[group] = volume;
}

void synth::set_master_vol(u32 volume) {
  m_Volume.left.Set(volume);
  m_Volume.right.Set(volume);
}

}  // namespace snd
