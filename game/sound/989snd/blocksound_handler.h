#pragma once
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

#include "game/sound/989snd/lfo.h"
#include "sfxblock2.h"

namespace snd {

extern std::array<s8, 32> g_block_reg;

class blocksound_voice : public vag_voice {
 public:
  blocksound_voice(Tone& t) : vag_voice(t) {}
  s32 g_vol;
  s32 g_pan;
};

class blocksound_handler : public sound_handler {
 public:
  blocksound_handler(SoundBank& bank,
                     SFX2& sfx,
                     voice_manager& vm,
                     s32 sfx_vol,
                     s32 sfx_pan,
                     SndPlayParams& params)
      : m_group(sfx.d.VolGroup), m_sfx(sfx), m_vm(vm), m_bank(bank) {
    s32 vol, pan, pitch_mod, pitch_bend;
    if (sfx_vol == -1) {
      sfx_vol = sfx.d.Vol;
    }
    if (sfx_pan == -1) {
      sfx_pan = sfx.d.Pan;
    }

    if (params.vol.has_value()) {
      vol = params.vol.value();
    } else {
      vol = 1024;
    }

    if (params.pan.has_value()) {
      pan = params.pan.value();
    } else {
      pan = -1;
    }

    if (params.pitch_mod.has_value()) {
      pitch_mod = params.pitch_mod.value();
    } else {
      pitch_mod = 0;
    }

    if (params.pitch_bend.has_value()) {
      pitch_bend = params.pitch_bend.value();
    } else {
      pitch_bend = 0;
    }

    if (vol == VOLUME_DONT_CHANGE) {
      vol = 1024;
    }
    s32 play_vol = (sfx_vol * vol) >> 10;
    if (play_vol >= 128) {
      play_vol = 127;
    }

    if (pan == PAN_RESET || pan == PAN_DONT_CHANGE) {
      pan = sfx_pan;
    }

    m_orig_volume = sfx_vol;
    m_orig_pan = sfx_pan;

    m_cur_volume = play_vol;
    m_cur_pan = pan;
    m_cur_pb = pitch_bend;
    m_cur_pm = pitch_mod;

    m_app_volume = vol;
    m_app_pan = pan;
    m_app_pb = pitch_bend;
    m_app_pm = pitch_mod;

    m_lfo_volume = 0;
    m_lfo_pan = 0;
    m_lfo_pb = 0;
    m_lfo_pm = 0;

    if (params.registers.has_value()) {
      m_registers = params.registers.value();
    }

    // Figure this stuff out properly someday
    // if (m_sfx.d.Flags & 2) {
    //   fmt::print("solo flag\n");
    //   m_done = true;
    //   return;
    // }

    m_next_grain = 0;
    m_countdown = m_sfx.grains[0]->delay();
    while (m_countdown <= 0 && !m_done) {
      do_grain();
    }
  }

  ~blocksound_handler() override {
    for (auto& p : m_voices) {
      auto v = p.lock();
      if (v != nullptr) {
        v->stop();
      }
    }
  }

  bool tick() override;
  SoundBank& bank() override { return m_bank; };

  void pause() override;
  void unpause() override;
  void stop() override;
  u8 group() override { return m_group; };
  void set_vol_pan(s32 vol, s32 pan) override;
  void set_pmod(s32 mod) override;
  void set_register(u8 reg, u8 value) override { m_registers.at(reg) = value; };
  void set_pbend(s32 bend) override;

  void do_grain();

  void update_pitch();

  bool m_paused{false};

  u8 m_group{0};
  bool m_done{false};

  u32 m_grains_to_play{0};
  u32 m_grains_to_skip{0};
  bool m_skip_grains{false};

  SFX2& m_sfx;
  voice_manager& m_vm;

  std::list<std::weak_ptr<blocksound_voice>> m_voices;

  std::list<std::unique_ptr<sound_handler>> m_children;

  s32 m_orig_volume{0};
  s32 m_orig_pan{0};
  s32 m_cur_volume{0};
  s32 m_cur_pan{0};
  s32 m_cur_pm{0};
  s32 m_cur_pb{0};
  s32 m_app_volume{0};
  s32 m_app_pan{0};
  s32 m_app_pm{0};
  s32 m_app_pb{0};

  s32 m_lfo_volume{0};
  s32 m_lfo_pan{0};
  s32 m_lfo_pm{0};
  s32 m_lfo_pb{0};

  SoundBank& m_bank;

  u8 m_note{60};
  u8 m_fine{0};

  std::array<s8, 4> m_registers{};
  std::array<LFOTracker, 4> m_lfo{{*this, *this, *this, *this}};

  // TODO LFO

  s32 m_countdown{0};
  u32 m_next_grain{0};
};
}  // namespace snd
