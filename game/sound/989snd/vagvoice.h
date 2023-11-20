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

class vag_voice : public voice {
 public:
  vag_voice(Tone& t) : tone(t) {}
  Tone& tone;
  u8 group{0};
  vol_pair basevol{};
  s32 current_pm{0};
  s32 current_pb{0};
  s32 start_note{0};
  s32 start_fine{0};
  bool paused{false};
};

class voice_manager {
 public:
  voice_manager(Synth& synth);
  void start_tone(std::shared_ptr<vag_voice> voice);
  void pause(std::shared_ptr<vag_voice> voice);
  void unpause(std::shared_ptr<vag_voice> voice);
  void set_pan_table(vol_pair* table) { m_pan_table = table; };

  vol_pair make_volume(int vol1, int pan1, int vol2, int pan2, int vol3, int pan3);

  // This is super silly, but it's what 989snd does
  vol_pair make_volume_b(int sound_vol,
                         int velocity_volume,
                         int pan,
                         int prog_vol,
                         int prog_pan,
                         int tone_vol,
                         int tone_pan);

  void set_master_vol(u8 group, s32 volume);
  void set_playback_mode(s32 mode) { m_stereo_or_mono = mode; }
  s16 adjust_vol_to_group(s16 involume, int group);

 private:
  Synth& m_synth;

  std::list<std::weak_ptr<vag_voice>> m_voices;
  void clean_voices() {
    m_voices.remove_if([](auto& v) { return v.expired(); });
  }

  s32 m_stereo_or_mono{0};

  std::array<s32, 32> m_master_vol;
  std::array<s32, 32> m_group_duck;

  const vol_pair* m_pan_table{nullptr};
};

}  // namespace snd
