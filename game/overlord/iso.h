#pragma once

/*!
 * @file iso.h
 * CD/DVD Reading.
 * This is a huge mess
 */

#include "common/common_types.h"
#include "isocommon.h"

void iso_init_globals();
FileRecord* FindISOFile(const char* name);
u32 GetISOFileLength(FileRecord* f);
u32 InitISOFS(const char* fs_mode, const char* loading_screen);
