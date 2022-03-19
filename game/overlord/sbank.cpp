#include <cstring>
#include "sbank.h"

constexpr int N_BANKS = 3;
SoundBank* gBanks[N_BANKS];
SoundBank gCommonBank;
SoundBank gLevelBank[2];

void sbank_init_globals() {
  gBanks[0] = &gCommonBank;
  gBanks[1] = &gLevelBank[0];
  gBanks[2] = &gLevelBank[1];
  memset((void*)&gCommonBank, 0, sizeof(gCommonBank));
  memset((void*)&gLevelBank, 0, sizeof(gLevelBank));
}

void InitBanks() {
  for (auto& gBank : gBanks) {
    gBank->unk1 = 0;
    gBank->sound_count = 0;
    strcpy(gBank->name, "<unused>");
  }
}

SoundBank* AllocateBank() {
  int idx = 0;
  // find a bank with unk1 = 0, or return nullptr if none exists
  while (true) {
    if (idx >= N_BANKS) {
      return nullptr;
    }

    if (gBanks[idx]->unk1 == 0) {
      break;
    }
    idx++;
  }

  if (idx == 0) {
    gBanks[0]->sound_count = 0x3fe;  // ??
  } else {
    gBanks[idx]->sound_count = 0x65;  // ??
  }

  return gBanks[idx];
}

s32 LookupSoundIndex(const char* name, SoundBank** bank_out) {
  int idx = 0;
  while (true) {
    if (idx > N_BANKS - 1) {
      return -1;
    }

    auto& bank = gBanks[idx];
    if (bank->unk1 == 0) {
      break;
    }

    for (int i = 0; i < bank->sound_count; i++) {
      if (memcmp(bank->name, name, 16) == 0) {
        *bank_out = bank;
        return i;
      }
    }
    idx++;
  }

  return -1;
}

// name should be a 16-byte "sound name"
SoundBank* LookupBank(const char* name) {
  int idx = N_BANKS - 1;
  while (true) {
    if (idx < 0) {
      return nullptr;  // not found.
    }
    auto& bank = gBanks[idx];
    // they had some weird stuff here that took advantage of the fact that this region was
    // 16-byte aligned, so it probably wasn't a memcmp, but this is easier.
    if (memcmp(bank->name, name, 16) == 0) {
      return bank;
    }
    idx--;
  }
}
