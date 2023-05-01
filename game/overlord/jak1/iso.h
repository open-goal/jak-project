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

struct VagDirEntry {
  char name[8];
  u32 offset;
};
static constexpr int VAG_COUNT = 868;
struct VagDir {
  u32 count;
  VagDirEntry vag[VAG_COUNT];
};
extern VagDir gVagDir;

void iso_init_globals();
s32 GetVAGStreamPos();
void SetVAGVol();
u32 ISOThread();
u32 InitISOFS(const char* fs_mode, const char* loading_screen);
void InitDriver(u8* buffer);
FileRecord* FindISOFile(const char* name);
u32 GetISOFileLength(FileRecord* f);
VagDirEntry* FindVAGFile(const char* name);

}  // namespace jak1