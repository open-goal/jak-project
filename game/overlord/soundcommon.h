#pragma once

#ifndef JAK_V2_SOUNDCOMMON_H
#define JAK_V2_SOUNDCOMMON_H
#include "common/common_types.h"

#include "game/overlord/sbank.h"

void strcpy_toupper(char* dest, const char* source);
void PrintBankInfo(SoundBank* buffer);
void ReadBankSoundInfo(SoundBank* bank, SoundBank* unk, s32 unk2);

#endif  // JAK_V2_SOUNDCOMMON_H
