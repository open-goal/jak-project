// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include <exception>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "ame_handler.h"
#include "loader.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

namespace snd {

struct ProgData {
  /*   0 */ s8 NumTones;
  /*   1 */ s8 Vol;
  /*   2 */ s16 Pan;
  /*   4 */ /*Tone**/ u32 FirstTone;
};

struct Prog {
  ProgData d;
  std::vector<Tone> tones;
};

class midi_voice : public vag_voice {
 public:
  midi_voice(Tone& t) : vag_voice(t) {}
  u8 note{0};
  u8 channel{0};
};

class ame_handler;
class midi_handler : public sound_handler {
 public:
  midi_handler(MIDIBlockHeader* block,
               voice_manager& vm,
               MIDISound& sound,
               s32 vol,
               s32 pan,
               locator& loc,
               SoundBank& bank);

  midi_handler(MIDIBlockHeader* block,
               voice_manager& vm,
               MIDISound& sound,
               s32 vol,
               s32 pan,
               locator& loc,
               SoundBank& bank,
               std::optional<ame_handler*> parent);

  ~midi_handler() override {
    for (auto& p : m_voices) {
      auto v = p.lock();
      if (v != nullptr) {
        v->stop();
      }
    }
  }
  void init_midi();
  void start();
  bool tick() override;
  void mute_channel(u8 channel);
  void unmute_channel(u8 channel);
  SoundBank& bank() override { return m_bank; };

  void pause() override;
  void stop() override;
  void unpause() override;
  u8 group() override { return m_sound.VolGroup; }
  void set_vol_pan(s32 vol, s32 pan) override;

  bool complete() { return m_track_complete; };
  void set_pmod(s32 mod) override;

 private:
  static constexpr int tickrate = 240;
  static constexpr int mics_per_tick = 1000000 / tickrate;
  struct midi_error : public std::exception {
    midi_error(std::string text) : msg(std::move(text)) {}
    midi_error() : msg("Unknown MIDI error") {}
    std::string msg;
    const char* what() const noexcept override { return msg.c_str(); }
  };

  std::optional<ame_handler*> m_parent;

  std::list<std::weak_ptr<midi_voice>> m_voices;

  MIDISound& m_sound;
  locator& m_locator;
  s32 m_vol{0x7f};
  s32 m_pan{0};
  s32 m_cur_pm{0};
  s8 m_repeats{0};
  SoundBank& m_bank;

  bool m_paused{false};

  MIDIBlockHeader* m_header{nullptr};

  std::array<bool, 16> m_mute_state{};
  std::array<s8, 16> m_chanvol{};
  std::array<s16, 16> m_chanpan{};
  std::array<s16, 16> m_pitch_bend{};
  u8* m_sample_data{nullptr};

  u8* m_seq_data_start{nullptr};
  u8* m_seq_ptr{nullptr};
  u8 m_status{0};
  u32 m_tempo{500000};
  u32 m_ppq{480};
  u32 m_time{0};
  s32 m_tickerror{0};
  s32 m_tickdelta{0};
  s32 m_ppt{0};
  u64 m_tick_countdown{0};
  bool m_get_delta{true};
  bool m_track_complete{false};
  u32 m_muted_channels{0};

  std::array<u8, 16> m_programs{};

  voice_manager& m_vm;

  void step();
  void new_delta();

  void note_on();
  void note_off();
  void controller_change();
  void channel_pressure();
  void program_change();
  void meta_event();
  void system_event();
  void channel_pitch();

  static std::pair<size_t, u32> read_vlq(u8* value);
};
}  // namespace snd
