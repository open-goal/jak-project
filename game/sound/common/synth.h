// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "common/common_types.h"
#include "../989snd/locator.h"
#include "sound_types.h"
#include "voice.h"
#include "envelope.h"
#include <forward_list>
#include <memory>
#include <unordered_map>
#include <vector>

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
  ///*  14 */ u32 reserved1; confiscated

  // FIXME I'd rather restructure things than mess about like this.
  // If we have to edit the structs they should't be loaded like this
  /*  14 */ u32 BankID;
};

struct ProgData {
  /*   0 */ s8 NumTones;
  /*   1 */ s8 Vol;
  /*   2 */ s16 Pan;
  /*   4 */ /*Tone**/ u32 FirstTone;
};

struct basic_data {
  /*   0 */ u32 pad1;
};

struct midi_data {
  /*   0 */ s8 MidiChannel;
  /*   1 */ u8 KeyOnVelocity;
  /*   2 */ s16 pad2;
  /*   4 */ s32 ShouldBeOff;
  /*   8 */ u32 KeyOnProg;
};

struct block_data {
  /*   0 */ s8 g_vol;
  /*   1 */ s8 pad;
  /*   2 */ s16 g_pan;
};

union ownerdata_tag {
  /*   0 */ basic_data BasicData;
  /*   0 */ midi_data MIDIData;
  /*   0 */ block_data BlockData;
};

struct SpuVolume {
  /*   0 */ s16 left;
  /*   2 */ s16 right;
};

struct VoiceAttributes {
  /*   0 */ u32 playlist;
  /*   4 */ s32 voice;
  /*   8 */ u32 Status;
  /*   c */ u32 Owner;
  /*  10 */ u32 OwnerProc;
  /*  14 */ u32 Tone;
  /*  18 */ s8 StartNote;
  /*  19 */ s8 StartFine;
  /*  1a */ u8 Priority;
  /*  1b */ s8 VolGroup;
  /*  1c */ u32 StartTick;
  /*  20 */ SpuVolume Volume;
  /*  24 */ s16 Current_PB;
  /*  26 */ s16 Current_PM;
  /*  28 */ u32 Flags;
  /*  2c */ ownerdata_tag OwnerData;
};

struct Prog {
  ProgData d;
  std::vector<Tone> tones;
};

class synth {
 public:
  synth(locator& loc) : m_locator(loc) {
    m_master_vol.fill(0x400);
    m_group_duck.fill(0x10000);

    m_Volume.left.Set(0x3FFF);
    m_Volume.right.Set(0x3FFF);
  }

  s16_output tick();
  void key_on(Tone& tone, u8 channel, u8 note, vol_pair volume, u64 owner, u32 group);
  void key_off(u8 channel, u8 note, u64 owner);

  void set_group_vol(u8 group, u32 volume);
  void set_master_vol(u32 volume);

 private:
  std::array<s32, 32> m_master_vol;
  std::array<s32, 32> m_group_duck;
  locator& m_locator;

  std::forward_list<std::unique_ptr<voice>> m_voices;

  VolumePair m_Volume{};

  s16 adjust_vol_to_group(s16 volume, int group);
};
}  // namespace snd
