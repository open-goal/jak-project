#pragma once

#include "common/common_types.h"

#include "game/overlord/common/sbank.h"

void strcpy_toupper(char* dest, const char* source);
void PrintBankInfo(SoundBank* buffer);
void ReadBankSoundInfo(SoundBank* bank, SoundBank* unk, s32 unk2);
