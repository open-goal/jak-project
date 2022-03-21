// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "ame_handler.h"
#include "loader.h"
#include "sound_handler.h"
#include "../common/synth.h"
#include "common/common_types.h"
#include "../common/voice.h"
#include <exception>
#include <optional>
#include <string>
#include <utility>

namespace snd {
class ame_handler;

class midi_handler : public sound_handler {
 public:
  midi_handler(MIDIBlockHeader* block,
               synth& synth,
               s32 vol,
               s32 pan,
               s8 repeats,
               u32 group,
               locator& loc,
               u32 bank,
               std::optional<ame_handler*> parent = std::nullopt)
      : m_parent(parent),
        m_locator(loc),
        m_vol(vol),
        m_pan(pan),
        m_repeats(repeats),
        m_header(block),
        m_group(group),
        m_synth(synth),
        m_bank(bank) {
    m_seq_data_start = (u8*)((uintptr_t)block + (uintptr_t)block->DataStart);
    m_seq_ptr = m_seq_data_start;
    m_tempo = block->Tempo;
    m_ppq = block->PPQ;
    m_chanvol.fill(0x7f);
    m_chanpan.fill(0);
  };

  void start();
  bool tick() override;
  void mute_channel(u8 channel);
  void unmute_channel(u8 channel);
  u32 bank() { return m_bank; };

  bool complete() { return m_track_complete; };

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

  locator& m_locator;
  s32 m_vol{0x7f};
  s32 m_pan{0};
  s8 m_repeats{0};
  u32 m_bank;

  MIDIBlockHeader* m_header{nullptr};

  std::array<bool, 16> m_mute_state{};
  std::array<s8, 16> m_chanvol{};
  std::array<s8, 16> m_chanpan{};
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
  u32 m_group{0};
  u64 m_tick_countdown{0};
  bool m_get_delta{true};
  bool m_track_complete{false};
  u32 m_muted_channels{0};

  std::array<u8, 16> m_programs{};

  synth& m_synth;

  void step();
  void new_delta();

  void note_on();
  void note_off();
  void channel_pressure();
  void program_change();
  void meta_event();
  void system_event();
  void channel_pitch();

  static std::pair<size_t, u32> read_vlq(u8* value);
};
}  // namespace snd
