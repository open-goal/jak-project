#pragma once

/*!
 * @file iso.h
 * CD/DVD Reading.
 * This is a huge mess
 */

#include "isocommon.h"

#include "common/common_types.h"

extern s32 gFakeVAGClockPaused;
extern s32 gFakeVAGClockRunning;
extern s32 gFakeVAGClock;
extern s32 gRealVAGClock;
extern s32 gVoice;

struct VagDirEntry {
  char name[8];
  u32 offset;
};

static constexpr int VAG_COUNT = 868;
struct VagDir {
  u32 count;
  VagDirEntry vag[VAG_COUNT];
};

void iso_init_globals();
FileRecord* FindISOFile(const char* name);
u32 GetISOFileLength(FileRecord* f);
u32 InitISOFS(const char* fs_mode, const char* loading_screen);
VagDirEntry* FindVAGFile(const char* name);
s32 GetVAGStreamPos();
