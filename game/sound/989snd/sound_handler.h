// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include "common/common_types.h"

namespace snd {
static constexpr int PAN_RESET = -1;
static constexpr int PAN_DONT_CHANGE = -2;
static constexpr int VOLUME_DONT_CHANGE = 0x7fffffff;

class SoundBank;

class sound_handler {
 public:
  virtual ~sound_handler() = default;
  virtual bool tick() = 0;
  virtual SoundBank& bank() = 0;
  virtual void pause() = 0;
  virtual void unpause() = 0;
  virtual u8 group() = 0;
  virtual void stop() = 0;
  virtual void set_vol_pan(s32 vol, s32 pan) = 0;
  virtual void set_pmod(s32 mod) = 0;
  virtual void set_pbend(s32 /*mod*/){};
  virtual void set_register(u8 /*reg*/, u8 /*value*/) {}
};
}  // namespace snd
