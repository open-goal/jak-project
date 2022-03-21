// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "loader.h"
#include "midi_handler.h"
#include "sound_handler.h"
#include "../common/synth.h"
#include "common/common_types.h"
#include <array>
#include <forward_list>

namespace snd {

class midi_handler;
class ame_handler : public sound_handler {
  friend class midi_handler;

 public:
  ame_handler(MultiMIDIBlockHeader* block,
              synth& synth,
              s32 vol,
              s32 pan,
              s8 repeats,
              u32 group,
              locator& loc,
              u32 bank);
  bool tick() override;
  u32 bank() override { return m_bank; };

  void set_register(u8 reg, u8 value) { m_register[reg] = value; }

 private:
  struct ame_error : public std::exception {
    ame_error(std::string text) : msg(std::move(text)) {}
    ame_error() : msg("Unknown AME error") {}
    std::string msg;
    const char* what() const noexcept override { return msg.c_str(); }
  };

  struct GroupDescription {
    /*   0 */ s8 num_channels;
    /*   1 */ s8 basis;
    /*   2 */ s8 pad[2];
    /*   4 */ s8 channel[16];
    /*  14 */ s8 excite_min[16];
    /*  24 */ s8 excite_max[16];
  };

  void start_segment(u32 id);
  void stop_segment(u32 id);
  std::pair<bool, u8*> run_ame(midi_handler&, u8* stream);

  u32 m_bank;

  MultiMIDIBlockHeader* m_header{nullptr};
  locator& m_locator;
  synth& m_synth;
  s32 m_vol{0};
  s32 m_pan{0};
  s8 m_repeats{0};
  u32 m_group{0};

  std::forward_list<std::unique_ptr<midi_handler>> m_midis;

  u8 m_excite{0};
  std::array<GroupDescription, 16> m_groups{};
  std::array<u8, 16> m_register{};
  std::array<u8*, 16> m_macro{};
};
}  // namespace snd
