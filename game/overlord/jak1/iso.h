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
extern IsoFs* isofs;
extern s32 iso_mbx;
extern s32 sync_mbx;
extern DgoCommand sLoadDGO;  // renamed from scmd to sLoadDGO in Jak 2

void iso_init_globals();
s32 GetVAGStreamPos();
void SetVAGVol();
u32 ISOThread();
u32 InitISOFS(const char* fs_mode, const char* loading_screen);
VagDirEntry* FindVAGFile(const char* name);
void InitDriver(u8* buffer);
u32 LookMbx(s32 mbx);
FileRecord* FindISOFile(const char* name);
u32 GetISOFileLength(FileRecord* f);

}  // namespace jak1