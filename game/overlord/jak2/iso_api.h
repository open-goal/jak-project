#pragma once

#include "common/common_types.h"

#include "game/sound/sndshim.h"

struct SoundBank;
struct FileRecord;
namespace jak2 {
struct VagStrListNode;
void SetVAGStreamPitch(int param_1, int param_2);
void SetDialogVolume(int param_1);
void LoadSoundBank(char* param_1, SoundBank* param_2);
void UnLoadMusic(snd::BankHandle* param_1);
void LoadMusic(char* param_1, snd::BankHandle* param_2);
void QueueVAGStream(VagStrListNode* param_1);
int LoadISOFileToEE(FileRecord* param_1, uint32_t param_2, int param_3);
int LoadISOFileToIOP(FileRecord* fr, uint8_t* addr, int len);
int LoadISOFileChunkToEE(FileRecord* param_1, uint32_t param_2, int param_3, int param_4);
}  // namespace jak2
