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
#include "musicbank.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

namespace snd {

class midi_voice : public VagVoice {
 public:
  midi_voice(Tone& t, MusicBank::Prog& _prog) : VagVoice(t), prog(_prog) {}
  u8 note{0};
  u8 channel{0};
  u8 velocity{0};
  MusicBank::Prog& prog;
};

class ame_handler;
class midi_handler : public SoundHandler {
 public:
  midi_handler(Midi* block,
               VoiceManager& vm,
               MusicBank::MIDISound& sound,
               s32 vol,
               s32 pan,
               SoundBank& bank);

  midi_handler(Midi* block,
               VoiceManager& vm,
               MusicBank::MIDISound& sound,
               s32 vol,
               s32 pan,
               SoundBank& bank,
               std::optional<ame_handler*> parent);

  ~midi_handler() override {
    for (auto& p : m_voices) {
      auto v = p.lock();
      if (v != nullptr) {
        v->Stop();
      }
    }
  }
  void init_midi();
  void start();
  bool Tick() override;
  void mute_channel(u8 channel);
  void unmute_channel(u8 channel);
  SoundBank& Bank() override { return m_bank; };

  void Pause() override;
  void Stop() override;
  void Unpause() override;
  u8 Group() override { return m_sound.VolGroup; }
  void SetVolPan(s32 vol, s32 pan) override;

  bool complete() { return m_track_complete; };
  void SetPMod(s32 mod) override;

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

  MusicBank::MIDISound& m_sound;
  s32 m_vol{0x7f};
  s32 m_pan{0};
  s32 m_cur_pm{0};
  s8 m_repeats{0};
  SoundBank& m_bank;

  bool m_paused{false};

  Midi* m_header{nullptr};

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

  VoiceManager& m_vm;

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
