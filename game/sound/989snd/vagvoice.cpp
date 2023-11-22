// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "vagvoice.h"

#include <stdexcept>

#include "player.h"
#include "util.h"

#include "../common/voice.h"

namespace snd {

std::list<std::weak_ptr<VagVoice>> gVoices;
std::array<s32, 32> gMasterVol;
std::array<s32, 32> gGroupDuck;
const VolPair* gPanTable{nullptr};
s32 gStereoOrMono{0};

// TODO remove
Synth* mSynth;

void VoiceManagerInit(Synth& synth) {
  mSynth = &synth;
  gPanTable = normalPanTable;
  gMasterVol.fill(0x400);
  gGroupDuck.fill(0x10000);
}

void SetPanTable(VolPair* table) {
  std::scoped_lock lock(gTickLock);
  gPanTable = table;
};

void CleanVoices() {
  gVoices.remove_if([](auto& v) { return v.expired(); });
}

void SetPlaybackMode(s32 mode) {
  std::scoped_lock lock(gTickLock);
  gStereoOrMono = mode;
}

void StartTone(std::shared_ptr<VagVoice> voice) {
  s16 left = AdjustVolToGroup(voice->basevol.left, voice->group);
  s16 right = AdjustVolToGroup(voice->basevol.right, voice->group);

  voice->SetVolume(left >> 1, right >> 1);

  if ((voice->tone.Flags & 0x10) != 0x0) {
    throw std::runtime_error("reverb only voice not handler");
  }

  std::pair<s16, s16> note = PitchBend(voice->tone, voice->current_pb, voice->current_pm,
                                       voice->start_note, voice->start_fine);

  auto pitch =
      PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

  voice->SetPitch(pitch);
  voice->SetAsdr1(voice->tone.ADSR1);
  voice->SetAsdr2(voice->tone.ADSR2);

  voice->SetSample((u16*)(voice->tone.Sample));

  voice->KeyOn();

  CleanVoices();
  gVoices.emplace_front(voice);
  mSynth->AddVoice(voice);
}
VolPair MakeVolume(int vol1, int pan1, int vol2, int pan2, int vol3, int pan3) {
  // Scale up as close as we can to max positive 16bit volume
  // I'd have just used shifting but I guess this does get closer

  s32 vol = vol1 * 258;
  vol = (vol * vol2) / 0x7f;
  vol = (vol * vol3) / 0x7f;

  // volume accurate up to here for sure
  if (vol == 0) {
    return {0, 0};
  }

  if (gStereoOrMono == 1) {
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
    lvol = (gPanTable[total_pan].left * vol) / 0x3fff;
    rvol = (gPanTable[total_pan].right * vol) / 0x3fff;
  } else {
    rvol = (gPanTable[total_pan - 180].left * vol) / 0x3fff;
    lvol = (gPanTable[total_pan - 180].right * vol) / 0x3fff;
  }

  // TODO rest of this function
  // there is a whole bunch of math depending on what the volume was previously?

  return {lvol, rvol};
}

VolPair MakeVolumeB(int sound_vol,
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

  if (gStereoOrMono == 1) {
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
    lvol = (gPanTable[total_pan].left * vol) / 0x3fff;
    rvol = (gPanTable[total_pan].right * vol) / 0x3fff;
  } else {
    rvol = (gPanTable[total_pan - 180].left * vol) / 0x3fff;
    lvol = (gPanTable[total_pan - 180].right * vol) / 0x3fff;
  }

  // TODO rest of this function
  // there is a whole bunch of math depending on what the volume was previously?

  return {lvol, rvol};
}

s16 AdjustVolToGroup(s16 involume, int group) {
  s32 volume = involume;
  // NOTE grou >= 7 in version 2
  if (group >= 15)
    return volume;

  if (volume >= 0x7fff)
    volume = 0x7ffe;

  // NOTE no duckers in version 2
  s32 modifier = (gMasterVol[group] * gGroupDuck[group]) / 0x10000;
  volume = (volume * modifier) / 0x400;
  int sign = 1;
  if (volume < 0) {
    sign = -1;
  }

  // fmt::print("made volume {:x} -> {:x}\n", involume, volume);
  s16 retval = static_cast<s16>((volume * volume) / 0x7ffe * sign);
  return retval;
}

void SetMasterVol(u8 group, s32 volume) {
  gMasterVol[group] = volume;

  for (auto& p : gVoices) {
    auto voice = p.lock();
    if (voice == nullptr || voice->paused) {
      continue;
    }

    if (voice->group == group) {
      s16 left = AdjustVolToGroup(voice->basevol.left, voice->group);
      s16 right = AdjustVolToGroup(voice->basevol.right, voice->group);

      voice->SetVolume(left >> 1, right >> 1);
    }
  }
}

void PauseTone(std::shared_ptr<VagVoice> voice) {
  voice->SetVolume(0, 0);
  voice->SetPitch(0);
  voice->paused = true;
}

void UnpauseTone(std::shared_ptr<VagVoice> voice) {
  s16 left = AdjustVolToGroup(voice->basevol.left, voice->group);
  s16 right = AdjustVolToGroup(voice->basevol.right, voice->group);

  voice->SetVolume(left >> 1, right >> 1);

  std::pair<s16, s16> note = PitchBend(voice->tone, voice->current_pb, voice->current_pm,
                                       voice->start_note, voice->start_fine);

  auto pitch =
      PS1Note2Pitch(voice->tone.CenterNote, voice->tone.CenterFine, note.first, note.second);

  voice->SetPitch(pitch);
  voice->paused = false;
}

};  // namespace snd
