// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "voice.h"

#include <array>

#include "third-party/fmt/core.h"

namespace snd {
#include "interp_table.inc"

// Integer math version of ps-adpcm coefs
static constexpr std::array<std::array<s16, 2>, 5> adpcm_coefs = {{
    {0, 0},
    {60, 0},
    {115, -52},
    {98, -55},
    {122, -60},
}};

void voice::DecodeSamples() {
  // This doesn't exactly match the real behaviour,
  // it seems to initially decode a bigger chunk
  // and then decode more data after a bit has drained
  if (m_DecodeBuf.Size() >= 16) {
    // sufficient data buffered
    return;
  }

  // Skip decoding for stopped voices.
  if (m_ADSR.GetPhase() == ADSR::Phase::Stopped) {
    for (int i = 0; i < 4; i++)
      m_DecodeBuf.Push(0);
  } else {
    u32 data = m_sample[m_NAX];
    for (int i = 0; i < 4; i++) {
      s32 sample = (s16)((data & 0xF) << 12);
      sample >>= m_CurHeader.Shift.get();

      // TODO do the right thing for invalid shift/filter values
      sample += (adpcm_coefs[m_CurHeader.Filter.get()][0] * m_DecodeHist1) >> 6;
      sample += (adpcm_coefs[m_CurHeader.Filter.get()][1] * m_DecodeHist2) >> 6;

      // We do get overflow here otherwise, should we?
      sample = std::clamp<s32>(sample, INT16_MIN, INT16_MAX);

      m_DecodeHist2 = m_DecodeHist1;
      m_DecodeHist1 = static_cast<s16>(sample);

      m_DecodeBuf.Push(static_cast<s16>(sample));
      data >>= 4;
    }
  }

  m_NAX++;

  if ((m_NAX & 0x7) == 0) {
    if (m_CurHeader.LoopEnd.get()) {
      m_NAX = m_LSA;
      m_ENDX = true;

      if (!m_CurHeader.LoopRepeat.get()) {
        // Need to inhibit stopping here in noise is on
        // seems to result in the right thing but would like to verify
        if (!m_Noise)
          m_ADSR.Stop();
      }
    }

    UpdateBlockHeader();

    m_NAX++;
  }
}

void voice::UpdateBlockHeader() {
  m_CurHeader.bits = m_sample[m_NAX & ~0x7];
  if (m_CurHeader.LoopStart.get() && !m_CustomLoop)
    m_LSA = m_NAX & ~0x7;
}

static s16 ApplyVolume(s16 sample, s32 volume) {
  return (sample * volume) >> 15;
}

void voice::key_on() {
  m_NAX = m_SSA;
  m_NAX++;

  UpdateBlockHeader();

  m_ENDX = false;
  m_ADSR.Attack();
  m_Counter = 0;
  m_DecodeHist1 = 0;
  m_DecodeHist2 = 0;
  m_DecodeBuf.Reset();
  m_CustomLoop = false;
  // Console.WriteLn("SPU[%d]:VOICE[%d] Key On, SSA %08x", m_SPU.m_Id, m_Id, m_SSA);
}

void voice::key_off() {
  m_ADSR.Release();
  // fmt::print("Key Off\n");
}

s16_output voice::run() {
  DecodeSamples();

  u32 index = (m_Counter & 0x0FF0) >> 4;

  s16 sample = 0;
  sample = static_cast<s16>(sample + ((m_DecodeBuf.Peek(0) * interp_table[index][0]) >> 15));
  sample = static_cast<s16>(sample + ((m_DecodeBuf.Peek(1) * interp_table[index][1]) >> 15));
  sample = static_cast<s16>(sample + ((m_DecodeBuf.Peek(2) * interp_table[index][2]) >> 15));
  sample = static_cast<s16>(sample + ((m_DecodeBuf.Peek(3) * interp_table[index][3]) >> 15));

  s32 step = m_Pitch;
  step = std::min(step, 0x3FFF);
  m_Counter += step;

  auto steps = m_Counter >> 12;
  m_Counter &= 0xFFF;

  while (steps > 0) {
    steps--;
    m_DecodeBuf.Pop();
  }

  sample = ApplyVolume(sample, m_ADSR.Level());
  s16 left = ApplyVolume(sample, m_Volume.left.GetCurrent());
  s16 right = ApplyVolume(sample, m_Volume.right.GetCurrent());

  m_ADSR.Run();
  m_Volume.Run();

  return s16_output{left, right};
}
}  // namespace snd
