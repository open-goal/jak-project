#pragma once

#include "common/common_types.h"

struct SoundBank {
  char name[16];
  u32 unk1;  // maybe status?
  u32 unk2;  // maybe size
};

void sbank_init_globals();

void InitBanks();

SoundBank* AllocateBank();
SoundBank* LookupBank(const char* name);