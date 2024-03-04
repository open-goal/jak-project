// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include <array>

#include "loader.h"
#include "midi_handler.h"
#include "musicbank.h"
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

namespace snd {

// added!
extern u64 SoundFlavaHack;

extern u8 GlobalExcite;

class MidiHandler;
class AmeHandler : public SoundHandler {
  friend class MidiHandler;

 public:
  AmeHandler(MultiMidi* block,
             VoiceManager& vm,
             MusicBank::MIDISound& sound,
             s32 vol,
             s32 pan,
             SoundBank& bank);
  bool Tick() override;
  SoundBank& Bank() override { return m_bank; };

  void Pause() override;
  void Unpause() override;
  void Stop() override;
  u8 Group() override { return m_sound.VolGroup; };
  void SetVolPan(s32 vol, s32 pan) override;

  void SetRegister(u8 reg, u8 value) override { m_register[reg] = value; }
  void SetPMod(s32 mod) override;

 private:
  struct AMEError : public std::exception {
    AMEError(std::string text) : msg(std::move(text)) {}
    AMEError() : msg("Unknown AME error") {}
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

  void StartSegment(u32 id);
  void StopSegment(u32 id);
  std::pair<bool, u8*> RunAME(MidiHandler&, u8* stream);

  MusicBank::MIDISound& m_sound;
  SoundBank& m_bank;

  MultiMidi* m_header{nullptr};
  VoiceManager& m_vm;
  s32 m_vol{0};
  s32 m_pan{0};
  s8 m_repeats{0};

  std::unordered_map<u32, std::unique_ptr<MidiHandler>> m_midis;

  std::array<GroupDescription, 16> m_groups{};
  std::array<u8, 16> m_register{};
  std::array<u8*, 16> m_macro{};
};
}  // namespace snd
