// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include "common/common_types.h"

namespace snd {
class sound_handler {
 public:
  virtual ~sound_handler() = default;
  virtual bool tick() = 0;
  virtual u32 bank() = 0;

 private:
};
}  // namespace snd
