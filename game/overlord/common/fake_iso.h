#pragma once

/*!
 * @file fake_iso.h
 * This provides an implementation of IsoFs for reading a "fake iso".
 * A "fake iso" is just a map file which maps 8.3 ISO file names to files in the source folder.
 * This way we don't need to actually create an ISO.
 *
 * The game has this compilation unit, but there is nothing in it. Probably it is removed to save
 * IOP memory and was only included on TOOL-only builds.  So this is my interpretation of how it
 * should work.
 */

#include "isocommon.h"

#include "third-party/BS_thread_pool.hpp"

void fake_iso_init_globals();
int fake_iso_FS_Init();
const char* get_file_path(FileRecord* fr);
FileRecord* FS_Find(const char* name);
FileRecord* FS_FindIN(const char* iso_name);
uint32_t FS_GetLength(FileRecord* fr);
void LoadMusicTweaks();
extern u32 fake_iso_entry_count;

extern BS::thread_pool thpool;
