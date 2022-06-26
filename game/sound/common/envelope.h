// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include "bitfield.h"

#include "common/common_types.h"

namespace snd {
union ADSRReg {
  u32 bits;

  bitfield<u32, u16, 16, 16> hi;
  bitfield<u32, u16, 0, 16> lo;

  bitfield<u32, bool, 31, 1> SustainExp;
  bitfield<u32, bool, 30, 1> SustainDecr;
  bitfield<u32, u8, 29, 1> Unused;
  bitfield<u32, u8, 24, 5> SustainShift;
  bitfield<u32, u8, 22, 2> SustainStep;
  bitfield<u32, bool, 21, 1> ReleaseExp;
  bitfield<u32, u8, 16, 5> ReleaseShift;

  bitfield<u32, bool, 15, 1> AttackExp;
  bitfield<u32, u8, 10, 5> AttackShift;
  bitfield<u32, u8, 8, 2> AttackStep;
  bitfield<u32, u8, 4, 4> DecayShift;
  bitfield<u32, u8, 0, 4> SustainLevel;
};

union VolReg {
  u16 bits;

  bitfield<u16, bool, 15, 1> EnableSweep;
  bitfield<u16, bool, 14, 1> SweepExp;
  bitfield<u16, bool, 13, 1> SweepDecrease;
  bitfield<u16, bool, 12, 1> NegativePhase;
  bitfield<u16, u8, 2, 5> SweepShift;
  bitfield<u16, u8, 0, 2> SweepStep;
};

class Envelope {
 public:
  void Step();

 protected:
  u8 m_Shift{0};
  s8 m_Step{0};
  bool m_Inv{false};
  bool m_Exp{false};
  bool m_Decrease{false};

  u32 m_Counter{0};
  s32 m_Level{0};
};

class ADSR : Envelope {
 public:
  enum class Phase {
    Attack,
    Decay,
    Sustain,
    Release,
    Stopped,
  };

  void Run();
  void Attack();
  void Release();
  void Stop();
  [[nodiscard]] s16 Level() const;
  void SetLevel(s16 value) { m_Level = value; }
  void UpdateSettings();
  ADSRReg m_Reg{0};
  [[nodiscard]] Phase GetPhase() const { return m_Phase; }

  void Reset() {
    m_Phase = Phase::Stopped;
    m_Target = 0;
    m_Counter = 0;
    m_Level = 0;
    m_Exp = false;
    m_Decrease = false;
    m_Inv = false;
    m_Step = 0;
    m_Shift = 0;
  }

 private:
  Phase m_Phase{Phase::Stopped};
  s32 m_Target{0};
};

class Volume : Envelope {
 public:
  void Run();
  void Set(u16 volume);
  [[nodiscard]] u16 Get() const;
  [[nodiscard]] s16 GetCurrent() const;

  void Reset() {
    m_Sweep.bits = 0;

    m_Counter = 0;
    m_Level = 0;
    m_Exp = false;
    m_Decrease = false;
    m_Inv = false;
    m_Step = 0;
    m_Shift = 0;
  }

 private:
  VolReg m_Sweep{0};
};

struct VolumePair {
  Volume left{};
  Volume right{};

  void Run() {
    left.Run();
    right.Run();
  }

  void Reset() {
    left.Reset();
    right.Reset();
  }
};
}  // namespace snd
