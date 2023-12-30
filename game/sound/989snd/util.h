#pragma once

#include <utility>

#include "common/common_types.h"

#include "game/sound/989snd/vagvoice.h"

namespace snd {

extern const u16 NotePitchTable[];
extern const VolPair normalPanTable[181];

u16 PS1Note2Pitch(s8 center_note, s8 center_fine, short note, short fine);
u16 sceSdNote2Pitch(u16 center_note, u16 center_fine, u16 note, short fine);
std::pair<s16, s16> PitchBend(Tone& tone,
                              int current_pb,
                              int current_pm,
                              int start_note,
                              int start_fine);

}  // namespace snd
