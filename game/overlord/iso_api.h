#pragma once

#include "isocommon.h"

struct SoundBank;
struct VagDirEntry;

s32 LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length);
s32 LoadISOFileToEE(FileRecord* file, uint32_t ee_addr, uint32_t length);
s32 LoadISOFileChunkToEE(FileRecord* file, uint32_t dest_addr, uint32_t length, uint32_t offset);
void LoadSoundBank(const char* bank_name, SoundBank* bank);
void LoadMusic(const char* music_name, s32* bank);

void QueueVAGStream(FileRecord* file, VagDirEntry* vag, u32 sound_id, u32 unk);
void PlayVAGStream(FileRecord* file,
                   VagDirEntry* vag,
                   u32 sound_id,
                   s32 volume,
                   u32 unk,
                   Vec3w* trans);
void SetVAGStreamVolume(s32 volume);
void SetDialogVolume(s32 volume);
void StopVAGStream(VagDirEntry* vag, u32 unk);
void PauseVAGStream();
void UnpauseVAGStream();
