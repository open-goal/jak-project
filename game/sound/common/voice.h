// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "bitfield.h"
#include "envelope.h"
#include "fifo.h"
#include "sound_types.h"

#include "common/common_types.h"

namespace snd {

class Voice {
 public:
  enum class AllocationType {
    Managed,
    Permanent,
  };

  Voice(AllocationType alloc = AllocationType::Managed) : mAlloc(alloc) {}
  s16Output Run();

  void KeyOn();

  void KeyOff();

  bool Dead() {
    if (mAlloc == AllocationType::Permanent) {
      return false;
    }

    return mADSR.GetPhase() == ADSR::Phase::Stopped;
  }

  void SetPitch(u16 reg) {
    // fmt::print("VOICE[{}] PITCH WRITE {:x}\n", m_channel, reg);
    mPitch = reg;
  }
  void SetAsdr1(u16 reg) {
    // fmt::print("VOICE[{}] ADSR1 WRITE {:x}\n", m_channel, reg);
    mADSR.m_Reg.lo.set(reg);
  }
  void SetAsdr2(u16 reg) {
    // fmt::print("VOICE[{}] ADSR2 WRITE {:x}\n", m_channel, reg);
    mADSR.m_Reg.hi.set(reg);
  }
  void SetVolume(u16 left, u16 right) {
    // fmt::print("VOICE[{}] VOLL WRITE {:x}\n", m_channel, left);
    // fmt::print("VOICE[{}] VOLR WRITE {:x}\n", m_channel, right);
    mVolume.left.Set(left);
    mVolume.right.Set(right);
  }

  void SetVolumeL(u16 vol) { mVolume.left.Set(vol); }

  void SetVolumeR(u16 vol) { mVolume.right.Set(vol); }

  s16 GetEnvx() { return mADSR.Level(); }

  void SetSample(u16* sample) {
    mSample = sample;
    mSSA = 0;
  }

  u32 GetNax() { return mNAX; }

  void SetSsa(u32 addr) { mSSA = addr; }

  void SetLsa(u32 addr) {
    mLSA = addr;
    mCustomLoop = true;
  }

  void Stop() { mADSR.Stop(); }

 private:
  union ADPCMHeader {
    u16 bits;
    bitfield<u16, bool, 10, 1> LoopStart;
    bitfield<u16, bool, 9, 1> LoopRepeat;
    bitfield<u16, bool, 8, 1> LoopEnd;
    bitfield<u16, u8, 4, 3> Filter;
    bitfield<u16, u8, 0, 4> Shift;
  };

  AllocationType mAlloc;
  bool mNoise{false};
  bool mENDX{false};

  void DecodeSamples();
  void UpdateBlockHeader();

  fifo<s16, 0x20> mDecodeBuf{};
  s16 mDecodeHist1{0};
  s16 mDecodeHist2{0};
  u32 mCounter{0};

  u16 mPitch{0};

  u16* mSample{nullptr};
  u32 mSSA{0};
  u32 mNAX{0};
  u32 mLSA{0};
  bool mCustomLoop{false};

  ADPCMHeader mCurHeader{};

  ADSR mADSR{};
  VolumePair mVolume{};
};
}  // namespace snd
