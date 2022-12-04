// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "vagvoice.h"

#include <stdexcept>

#include "util.h"

#include "../common/voice.h"

namespace snd {
voice_manager::voice_manager(synth& synth, locator& loc) : m_synth(synth), m_locator(loc) {
  m_pan_table = normalPanTable;
  m_master_vol.fill(0x400);
  m_group_duck.fill(0x10000);
}

void voice_manager::start_tone(std::shared_ptr<vag_voice> voice, u32 bank) {
  s16 left = adjust_vol_to_group(voice->basevol.left, voice->group);
  s16 right = adjust_vol_to_group(voice->basevol.right, voice->group);

  voice->set_volume(left >> 1, right >> 1);

  if ((voice->tone.Flags & 0x10) != 0x0) {
    throw std::runtime_error("reverb only voice not handler");
  }

  std::pair<s16, s16> note = pitchbend(voice->tone, voice->current_pb, voice->current_pm,
                                       voice->start_note, voice->start_fine);

  auto pitch =
      PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

  voice->set_pitch(pitch);
  voice->set_asdr1(voice->tone.ADSR1);
  voice->set_asdr2(voice->tone.ADSR2);

  u8* sbuf = m_locator.get_bank_samples(bank);
  voice->set_sample((u16*)(sbuf + voice->tone.VAGInSR));

  voice->key_on();

  clean_voices();
  m_voices.emplace_front(voice);
  m_synth.add_voice(voice);
}
vol_pair voice_manager::make_volume(int vol1, int pan1, int vol2, int pan2, int vol3, int pan3) {
  // Scale up as close as we can to max positive 16bit volume
  // I'd have just used shifting but I guess this does get closer

  s32 vol = vol1 * 258;
  vol = (vol * vol2) / 0x7f;
  vol = (vol * vol3) / 0x7f;

  // volume accurate up to here for sure
  if (vol == 0) {
    return {0, 0};
  }

  if (m_stereo_or_mono == 1) {
    return {(s16)vol, (s16)vol};
  }

  int total_pan = pan1 + pan3 + pan2;
  while (total_pan >= 360) {
    total_pan -= 360;
  }

  while (total_pan < 0) {
    total_pan += 360;
  }

  if (total_pan >= 270) {
    total_pan -= 270;
  } else {
    total_pan += 90;
  }

  // fmt::print("total pan {}\n", total_pan);
  s16 lvol = 0;
  s16 rvol = 0;

  // TODO Presumable for the purposes of some effects this function needs
  // to know the sign of the previous volume so that it can maintain
  // it. (For surround audio positioning?)

  if (total_pan < 180) {
    lvol = (m_pan_table[total_pan].left * vol) / 0x3fff;
    rvol = (m_pan_table[total_pan].right * vol) / 0x3fff;
  } else {
    rvol = (m_pan_table[total_pan - 180].left * vol) / 0x3fff;
    lvol = (m_pan_table[total_pan - 180].right * vol) / 0x3fff;
  }

  // TODO rest of this function
  // there is a whole bunch of math depending on what the volume was previously?

  return {lvol, rvol};
}

vol_pair voice_manager::make_volume_b(int sound_vol,
                                      int velocity_volume,
                                      int pan,
                                      int prog_vol,
                                      int prog_pan,
                                      int tone_vol,
                                      int tone_pan) {
  // Scale up as close as we can to max positive 16bit volume
  // I'd have just used shifting but I guess this does get closer

  s32 vol = sound_vol * 258;
  vol = (vol * velocity_volume) / 0x7f;
  vol = (vol * prog_vol) / 0x7f;
  vol = (vol * tone_vol) / 0x7f;

  // volume accurate up to here for sure
  if (vol == 0) {
    return {0, 0};
  }

  if (m_stereo_or_mono == 1) {
    return {(s16)vol, (s16)vol};
  }

  int total_pan = pan + tone_pan + prog_pan;
  while (total_pan >= 360) {
    total_pan -= 360;
  }

  while (total_pan < 0) {
    total_pan += 360;
  }

  if (total_pan >= 270) {
    total_pan -= 270;
  } else {
    total_pan += 90;
  }

  // fmt::print("total pan {}\n", total_pan);
  s16 lvol = 0;
  s16 rvol = 0;

  // TODO Presumable for the purposes of some effects this function needs
  // to know the sign of the previous volume so that it can maintain
  // it. (For surround audio positioning?)

  if (total_pan < 180) {
    lvol = (m_pan_table[total_pan].left * vol) / 0x3fff;
    rvol = (m_pan_table[total_pan].right * vol) / 0x3fff;
  } else {
    rvol = (m_pan_table[total_pan - 180].left * vol) / 0x3fff;
    lvol = (m_pan_table[total_pan - 180].right * vol) / 0x3fff;
  }

  // TODO rest of this function
  // there is a whole bunch of math depending on what the volume was previously?

  return {lvol, rvol};
}

s16 voice_manager::adjust_vol_to_group(s16 involume, int group) {
  s32 volume = involume;
  // NOTE grou >= 7 in version 2
  if (group >= 15)
    return volume;

  if (volume >= 0x7fff)
    volume = 0x7ffe;

  // NOTE no duckers in version 2
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

void voice_manager::set_master_vol(u8 group, s32 volume) {
  m_master_vol[group] = volume;

  for (auto& p : m_voices) {
    auto voice = p.lock();
    if (voice == nullptr || voice->paused) {
      continue;
    }

    if (voice->group == group) {
      s16 left = adjust_vol_to_group(voice->basevol.left, voice->group);
      s16 right = adjust_vol_to_group(voice->basevol.right, voice->group);

      voice->set_volume(left >> 1, right >> 1);
    }
  }
}

void voice_manager::pause(std::shared_ptr<vag_voice> voice) {
  voice->set_volume(0, 0);
  voice->set_pitch(0);
  voice->paused = true;
}

void voice_manager::unpause(std::shared_ptr<vag_voice> voice) {
  s16 left = adjust_vol_to_group(voice->basevol.left, voice->group);
  s16 right = adjust_vol_to_group(voice->basevol.right, voice->group);

  voice->set_volume(left >> 1, right >> 1);

  std::pair<s16, s16> note = pitchbend(voice->tone, voice->current_pb, voice->current_pm,
                                       voice->start_note, voice->start_fine);

  auto pitch =
      PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

  voice->set_pitch(pitch);
  voice->paused = false;
}

};  // namespace snd
