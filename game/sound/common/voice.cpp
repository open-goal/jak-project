// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#include "voice.h"

#include <array>

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

void Voice::DecodeSamples() {
  // This doesn't exactly match the real behaviour,
  // it seems to initially decode a bigger chunk
  // and then decode more data after a bit has drained
  if (mDecodeBuf.Size() >= 16) {
    // sufficient data buffered
    return;
  }

  // Skip decoding for stopped voices.
  if (mADSR.GetPhase() == ADSR::Phase::Stopped) {
    for (int i = 0; i < 4; i++)
      mDecodeBuf.Push(0);
  } else {
    u32 data = mSample[mNAX];
    for (int i = 0; i < 4; i++) {
      s32 sample = (s16)((data & 0xF) << 12);
      sample >>= mCurHeader.Shift.get();

      // TODO do the right thing for invalid shift/filter values
      sample += (adpcm_coefs[mCurHeader.Filter.get()][0] * mDecodeHist1) >> 6;
      sample += (adpcm_coefs[mCurHeader.Filter.get()][1] * mDecodeHist2) >> 6;

      // We do get overflow here otherwise, should we?
      sample = std::clamp<s32>(sample, INT16_MIN, INT16_MAX);

      mDecodeHist2 = mDecodeHist1;
      mDecodeHist1 = static_cast<s16>(sample);

      mDecodeBuf.Push(static_cast<s16>(sample));
      data >>= 4;
    }
  }

  mNAX++;

  if ((mNAX & 0x7) == 0) {
    if (mCurHeader.LoopEnd.get()) {
      mNAX = mLSA;
      mENDX = true;

      if (!mCurHeader.LoopRepeat.get()) {
        // Need to inhibit stopping here in noise is on
        // seems to result in the right thing but would like to verify
        if (!mNoise)
          mADSR.Stop();
      }
    }

    UpdateBlockHeader();

    mNAX++;
  }
}

void Voice::UpdateBlockHeader() {
  mCurHeader.bits = mSample[mNAX & ~0x7];
  if (mCurHeader.LoopStart.get() && !mCustomLoop)
    mLSA = mNAX & ~0x7;
}

static s16 ApplyVolume(s16 sample, s32 volume) {
  return (sample * volume) >> 15;
}

void Voice::KeyOn() {
  mNAX = mSSA;
  mNAX++;

  UpdateBlockHeader();

  mENDX = false;
  mADSR.Attack();
  mCounter = 0;
  mDecodeHist1 = 0;
  mDecodeHist2 = 0;
  mDecodeBuf.Reset();
  mCustomLoop = false;
  // Console.WriteLn("SPU[%d]:VOICE[%d] Key On, SSA %08x", m_SPU.m_Id, m_Id, m_SSA);
}

void Voice::KeyOff() {
  mADSR.Release();
  // fmt::print("Key Off\n");
}

s16Output Voice::Run() {
  DecodeSamples();

  u32 index = (mCounter & 0x0FF0) >> 4;

  s16 sample = 0;
  sample = static_cast<s16>(sample + ((mDecodeBuf.Peek(0) * interp_table[index][0]) >> 15));
  sample = static_cast<s16>(sample + ((mDecodeBuf.Peek(1) * interp_table[index][1]) >> 15));
  sample = static_cast<s16>(sample + ((mDecodeBuf.Peek(2) * interp_table[index][2]) >> 15));
  sample = static_cast<s16>(sample + ((mDecodeBuf.Peek(3) * interp_table[index][3]) >> 15));

  s32 step = mPitch;
  step = std::min(step, 0x3FFF);
  mCounter += step;

  auto steps = mCounter >> 12;
  mCounter &= 0xFFF;

  while (steps > 0) {
    steps--;
    mDecodeBuf.Pop();
  }

  sample = ApplyVolume(sample, mADSR.Level());
  s16 left = ApplyVolume(sample, mVolume.left.GetCurrent());
  s16 right = ApplyVolume(sample, mVolume.right.GetCurrent());

  mADSR.Run();
  mVolume.Run();

  return s16Output{left, right};
}
}  // namespace snd
