#pragma once

#include <array>
#include <string>
#include <vector>

#include "common/common_types.h"

#include "game/sound/sndshim.h"

struct SoundRecord {
  std::array<char, 16> name;
  u32 fallof_params;
};

struct SoundBank {
  std::array<char, 16> name;
  snd::BankHandle bank_handle;
  u32 sound_count;

  // name list, only for jak1
  std::vector<SoundRecord> sound;

  // jak2 additions
  u32 spu_loc;
  u32 spu_size;
  u32 unk4;
  bool in_use;
};

void sbank_init_globals();

void InitBanks();
void ReloadBankInfo();

SoundBank* AllocateBank();
SoundBank* AllocateBankName(const char* name);
s32 LookupSoundIndex(const char* name, SoundBank** bank_out);
SoundBank* LookupBank(const char* name);
