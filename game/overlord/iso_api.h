#pragma once

#include "isocommon.h"

struct SoundBank;

s32 LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length);
s32 LoadISOFileToEE(FileRecord* file, uint32_t ee_addr, uint32_t length);
s32 LoadISOFileChunkToEE(FileRecord* file, uint32_t dest_addr, uint32_t length, uint32_t offset);
void LoadSoundBank(const char* bank_name, SoundBank* bank);
void LoadMusic(const char* music_name, s32* bank);
