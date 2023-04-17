#pragma once

/*!
 * @file iso.h
 * CD/DVD Reading.
 * This is a huge mess
 */

#include "isocommon.h"

#include "common/common_types.h"

namespace jak1 {
extern s32 gFakeVAGClockPaused;
extern s32 gFakeVAGClockRunning;
extern s32 gFakeVAGClock;
extern s32 gRealVAGClock;
extern s32 gVoice;


void iso_init_globals();
s32 GetVAGStreamPos();
void SetVAGVol();
u32 ISOThread();
}  // namespace jak1