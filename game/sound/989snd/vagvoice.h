// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include <list>
#include <memory>

#include "locator.h"

#include "common/common_types.h"

#include "game/sound/common/synth.h"
#include "game/sound/common/voice.h"

namespace snd {

enum class toneflag : u16 {
  out_reverb = 1,
  out_dry = 0x10,
};

struct Tone {
  /*   0 */ s8 Priority;
  /*   1 */ s8 Vol;
  /*   2 */ s8 CenterNote;
  /*   3 */ s8 CenterFine;
  /*   4 */ s16 Pan;
  /*   6 */ s8 MapLow;
  /*   7 */ s8 MapHigh;
  /*   8 */ s8 PBLow;
  /*   9 */ s8 PBHigh;
  /*   a */ s16 ADSR1;
  /*   c */ s16 ADSR2;
  /*   e */ s16 Flags;
  /*  10 */ /*void**/ u32 VAGInSR;
  /*  14 */ u32 reserved1;
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
  voice_manager(synth& synth, locator& loc);
  void start_tone(std::shared_ptr<vag_voice> voice, u32 bank);
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
  synth& m_synth;
  locator& m_locator;

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
