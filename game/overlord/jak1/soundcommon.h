#pragma once

#include "common/common_types.h"

#include "game/overlord/jak1/sbank.h"

namespace jak1 {
void strcpy_toupper(char* dest, const char* source);
void PrintBankInfo(SoundBank* buffer);
void ReadBankSoundInfo(SoundBank* bank, SoundBank* unk, s32 unk2);

}  // namespace jak1
