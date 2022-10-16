#pragma once
#include <unordered_map>

#include "sfxblock.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

namespace snd {
class blocksound_handler : public sound_handler {
 public:
  blocksound_handler(SFX& sfx, voice_manager& vm, s32 vol, s32 pan, s32 pm, s32 pb, u32 bank_id)
      : m_sfx(sfx), m_vm(vm), m_bank(bank_id) {
    vol = (vol * m_sfx.d.Vol) >> 10;
    if (vol >= 128) {
      vol = 127;
    }

    if (pan >= PAN_DONT_CHANGE) {
      pan = m_sfx.d.Pan;
    }

    m_cur_volume = vol;
    m_cur_pan = pan;
    m_cur_pm = pm;
    m_cur_pb = pb;

    m_app_volume = vol;
    m_app_pan = pan;
    m_app_pm = 0;  // why only this one?
    m_app_pb = pb;

    m_orig_pan = m_sfx.d.Pan;
    m_orig_volume = m_sfx.d.Vol;

    m_group = sfx.d.VolGroup;

    m_grain_handler.insert(std::make_pair(grain_type::null, &blocksound_handler::null));
    m_grain_handler.insert(std::make_pair(grain_type::tone, &blocksound_handler::play_tone));
    m_grain_handler.insert(std::make_pair(grain_type::rand_play, &blocksound_handler::rand_play));
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
  u32 bank() override { return m_bank; };

  void pause() override;
  void unpause() override;
  void stop() override;
  u8 group() override { return m_group; };
  void set_vol_pan(s32 vol, s32 pan) override;
  void set_pmod(s32 mod) override;
  void set_pbend(s32 bend);  // TODO override;

  void init();

 private:
  enum class grain_type : u32 {
    null = 0,
    tone = 1,
    xref_id = 2,
    xref_num = 3,
    lfo_settings = 4,
    loop_start = 21,
    loop_end = 22,
    loop_continue = 23,
    rand_play = 25,
    rand_delay = 26,
  };

  void do_grain();

  s32 null(SFXGrain& grain);
  s32 play_tone(SFXGrain& grain);
  s32 rand_play(SFXGrain& grain);
  void update_pitch();

  using grain_fp = int (blocksound_handler::*)(SFXGrain& grain);
  std::unordered_map<grain_type, grain_fp> m_grain_handler;

  bool m_paused{false};

  u8 m_group{0};
  bool m_done{false};

  u32 m_grains_to_play{0};
  u32 m_grains_to_skip{0};
  bool m_skip_grains{false};

  SFX& m_sfx;
  voice_manager& m_vm;

  std::list<std::weak_ptr<vag_voice>> m_voices;

  s32 m_current_pb{0};
  s32 m_current_pm{0};

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

  u32 m_bank{0};

  u8 m_note{60};
  u8 m_fine{0};

  std::array<u8, 4> m_registers{};

  // TODO LFO

  s32 m_countdown{0};
  u32 m_next_grain{0};
};
}  // namespace snd
