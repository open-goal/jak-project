#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_iso_api();

struct ISOFileDef;
struct VagStreamData;
struct SoundBankInfo;

int LoadISOFileToEE(const ISOFileDef* file_def, u32 addr, int length);
int LoadISOFileToIOP(const ISOFileDef* file_def, void* addr, int length);
void PlayMusicStream(VagStreamData* data);
int LoadISOFileChunkToEE(const ISOFileDef* file_def, u32 addr, int max_len, int sector_offset);
void SetVAGStreamPitch(s32 id, s32 pitch);
void UnpauseVAGStreams();
u32 LoadSoundBankToIOP(const char* name, SoundBankInfo* bank, u32 mode);
}  // namespace jak3