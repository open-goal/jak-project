// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "common/common_types.h"
#include "third-party/fmt/core.h"
#include "../common/sound_types.h"
#include <algorithm>
#include <utility>

namespace snd {
vol_pair make_volume(int sound_vol,
                     int velocity_volume,
                     int pan,
                     int prog_vol,
                     int prog_pan,
                     int tone_vol,
                     int tone_pan);

u16 sceSdNote2Pitch(u16 center_note, u16 center_fine, u16 note, short fine);
u16 PS1Note2Pitch(s8 center_note, s8 center_fine, short note, short fine);
u32 hash(void* data, size_t size);
}  // namespace snd
