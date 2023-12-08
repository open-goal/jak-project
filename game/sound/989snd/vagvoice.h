// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include <list>
#include <memory>

#include "common/common_types.h"

#include "game/sound/common/synth.h"
#include "game/sound/common/voice.h"

namespace snd {

enum class toneflag : u16 {
  out_reverb = 1,
  out_dry = 0x10,
};

struct Tone {
  s8 Priority;
  s8 Vol;
  s8 CenterNote;
  s8 CenterFine;
  s16 Pan;
  s8 MapLow;
  s8 MapHigh;
  s8 PBLow;
  s8 PBHigh;
  u16 ADSR1;
  u16 ADSR2;
  u16 Flags;
  u8* Sample;
};

class VagVoice : public Voice {
 public:
  VagVoice(Tone& t) : tone(t) {}
  Tone& tone;
  u8 group{0};
  VolPair basevol{};
  s32 current_pm{0};
  s32 current_pb{0};
  s32 start_note{0};
  s32 start_fine{0};
  bool paused{false};
};

void VoiceManagerInit(Synth& synth);
void StartTone(std::shared_ptr<VagVoice> voice);
void PauseTone(std::shared_ptr<VagVoice> voice);
void UnpauseTone(std::shared_ptr<VagVoice> voice);
void SetPanTable(VolPair* table);

VolPair MakeVolume(int vol1, int pan1, int vol2, int pan2, int vol3, int pan3);

// This is super silly, but it's what 989snd does
VolPair MakeVolumeB(int sound_vol,
                    int velocity_volume,
                    int pan,
                    int prog_vol,
                    int prog_pan,
                    int tone_vol,
                    int tone_pan);

void SetMasterVol(u8 group, s32 volume);
void SetPlaybackMode(s32 mode);
s16 AdjustVolToGroup(s16 involume, int group);
void CleanVoices();

}  // namespace snd
