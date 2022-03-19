#pragma once

#include "common/common_types.h"

struct SoundRecord {
  char name[16];
  u32 fallof_params;
};

struct SoundBank {
  char name[16];
  u32 snd_id;
  u32 sound_count;
  union {
    SoundRecord sound[1];
    u8 buffer[0x800];
  };
};

void sbank_init_globals();

void InitBanks();
void ReloadBankInfo();

SoundBank* AllocateBank();
s32 LookupSoundIndex(const char* name, SoundBank** bank_out);
SoundBank* LookupBank(const char* name);
