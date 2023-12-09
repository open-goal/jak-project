// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include "common/common_types.h"

namespace snd {
static constexpr int PAN_RESET = -1;
static constexpr int PAN_DONT_CHANGE = -2;
static constexpr int VOLUME_DONT_CHANGE = 0x7fffffff;

class SoundBank;

class SoundHandler {
 public:
  virtual ~SoundHandler() = default;
  virtual bool Tick() = 0;
  virtual SoundBank& Bank() = 0;
  virtual void Pause() = 0;
  virtual void Unpause() = 0;
  virtual u8 Group() = 0;
  virtual void Stop() = 0;
  virtual void SetVolPan(s32 vol, s32 pan) = 0;
  virtual void SetPMod(s32 mod) = 0;
  virtual void SetPBend(s32 /*mod*/){};
  virtual void SetRegister(u8 /*reg*/, u8 /*value*/) {}
};
}  // namespace snd
