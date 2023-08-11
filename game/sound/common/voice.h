// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "bitfield.h"
#include "envelope.h"
#include "fifo.h"
#include "sound_types.h"

#include "common/common_types.h"

#include "third-party/fmt/core.h"

namespace snd {

class voice {
 public:
  enum class AllocationType {
    managed,
    permanent,
  };

  voice(AllocationType alloc = AllocationType::managed) : m_Alloc(alloc) {}
  s16_output run();

  void key_on();

  void key_off();

  bool dead() {
    if (m_Alloc == AllocationType::permanent) {
      return false;
    }

    return m_ADSR.GetPhase() == ADSR::Phase::Stopped;
  }

  void set_pitch(u16 reg) {
    // fmt::print("VOICE[{}] PITCH WRITE {:x}\n", m_channel, reg);
    m_Pitch = reg;
  }
  void set_asdr1(u16 reg) {
    // fmt::print("VOICE[{}] ADSR1 WRITE {:x}\n", m_channel, reg);
    m_ADSR.m_Reg.lo.set(reg);
  }
  void set_asdr2(u16 reg) {
    // fmt::print("VOICE[{}] ADSR2 WRITE {:x}\n", m_channel, reg);
    m_ADSR.m_Reg.hi.set(reg);
  }
  void set_volume(u16 left, u16 right) {
    // fmt::print("VOICE[{}] VOLL WRITE {:x}\n", m_channel, left);
    // fmt::print("VOICE[{}] VOLR WRITE {:x}\n", m_channel, right);
    m_Volume.left.Set(left);
    m_Volume.right.Set(right);
  }

  void set_volume_l(u16 vol) { m_Volume.left.Set(vol); }

  void set_volume_r(u16 vol) { m_Volume.right.Set(vol); }

  s16 get_envx() { return m_ADSR.Level(); }

  void set_sample(u16* sample) {
    m_sample = sample;
    m_SSA = 0;
  }

  u32 get_nax() { return m_NAX; }

  void set_ssa(u32 addr) { m_SSA = addr; }

  void set_lsa(u32 addr) {
    m_LSA = addr;
    m_CustomLoop = true;
  }

  void stop() { m_ADSR.Stop(); }

 private:
  union ADPCMHeader {
    u16 bits;
    bitfield<u16, bool, 10, 1> LoopStart;
    bitfield<u16, bool, 9, 1> LoopRepeat;
    bitfield<u16, bool, 8, 1> LoopEnd;
    bitfield<u16, u8, 4, 3> Filter;
    bitfield<u16, u8, 0, 4> Shift;
  };

  AllocationType m_Alloc;
  bool m_Noise{false};
  [[maybe_unused]] bool m_PitchMod{false};
  [[maybe_unused]] bool m_KeyOn{false};
  [[maybe_unused]] bool m_KeyOff{false};
  bool m_ENDX{false};

  void DecodeSamples();
  void UpdateBlockHeader();

  fifo<s16, 0x20> m_DecodeBuf{};
  s16 m_DecodeHist1{0};
  s16 m_DecodeHist2{0};
  u32 m_Counter{0};

  u16 m_Pitch{0};
  [[maybe_unused]] s16 m_Out{0};

  u16* m_sample{nullptr};
  u32 m_SSA{0};
  u32 m_NAX{0};
  u32 m_LSA{0};
  bool m_CustomLoop{false};

  ADPCMHeader m_CurHeader{};

  ADSR m_ADSR{};
  VolumePair m_Volume{};
};
}  // namespace snd
