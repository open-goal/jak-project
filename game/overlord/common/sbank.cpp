#include "sbank.h"

#include <cstring>

#include "soundcommon.h"

#include "common/log/log.h"

#include "game/runtime.h"

static constexpr int N_BANKS = 6;

SoundBank gCommonBank;
SoundBank gGunBank;
SoundBank gBoardBank;
SoundBank gLevelBanks[3];

SoundBank* gBanks[N_BANKS] = {&gCommonBank,    &gGunBank,       &gBoardBank,
                              &gLevelBanks[0], &gLevelBanks[1], &gLevelBanks[2]};

void sbank_init_globals() {
  gCommonBank = {};
  gGunBank = {};
  gBoardBank = {};
  for (auto& b : gLevelBanks) {
    b = {};
  }
}

void InitBanks() {
  for (auto bank : gBanks) {
    bank->bank_handle = 0;
    bank->sound_count = 0;

    // paper over version differences
    // in_use doesn't exist in jak1, but setting it to true
    // for all banks results in the same behaviour
    if (g_game_version == GameVersion::Jak1) {
      bank->in_use = true;
    } else {
      bank->in_use = false;
    }

    bank->unk4 = 0;
    strcpy(bank->name.data(), "<unused>");
  }

  if (g_game_version == GameVersion::Jak2) {
    strncpy(gBanks[0]->name.data(), "common", 16);
    gBanks[0]->spu_loc = 0x20000;
    gBanks[0]->spu_size = 0xAFCC0;

    strncpy(gBanks[1]->name.data(), "gun", 16);
    gBanks[1]->spu_loc = 0x131740;
    gBanks[1]->spu_size = 0;

    strncpy(gBanks[2]->name.data(), "board", 16);
    gBanks[2]->spu_loc = 0x131740;
    gBanks[2]->spu_size = 0;

    strncpy(gBanks[3]->name.data(), "level0", 16);
    gBanks[3]->spu_loc = 0x131740;
    gBanks[3]->spu_size = 0x42800;

    strncpy(gBanks[4]->name.data(), "level1", 16);
    gBanks[4]->spu_loc = 0x173f40;
    gBanks[4]->spu_size = 0x42800;

    strncpy(gBanks[5]->name.data(), "level2", 16);
    gBanks[5]->spu_loc = 0x1B6740;
    gBanks[5]->spu_size = 0x42800;
  }
}

SoundBank* AllocateBank() {
  int idx = 0;
  // find a bank with unk1 = 0, or return nullptr if none exists
  while (true) {
    if (idx >= N_BANKS) {
      return nullptr;
    }

    if (gBanks[idx]->bank_handle == 0) {
      break;
    }
    idx++;
  }

  // Funny hack: The loader will read this out of the destination buffer in order to determine how
  // many sectors of sound name mapping it needs to read.
  if (idx == 0) {
    gBanks[0]->sound_count = 0x3fe;
  } else {
    gBanks[idx]->sound_count = 0x65;
  }

  return gBanks[idx];
}

SoundBank* AllocateBankName(const char* name) {
  if ((!strncmp(name, "common", 16) || !strncmp(name, "commonj", 16)) && !gBanks[0]->in_use) {
    return gBanks[0];
  }

  for (int i = 3; i < N_BANKS; i++) {
    if (!gBanks[i]->in_use) {
      gBanks[i]->bank_handle = 0;
      gBanks[i]->unk4 = 0;
      return gBanks[i];
    }
  }

  return nullptr;
}

s32 LookupSoundIndex(const char* name, SoundBank** bank_out) {
  for (auto bank : gBanks) {
    if (!bank->bank_handle) {
      continue;
    }

    for (int i = 0; i < (int)bank->sound_count; i++) {
      if (memcmp(bank->sound[i].name.data(), name, 16) == 0) {
        *bank_out = bank;
        return i;
      }
    }
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
    auto bank = gBanks[idx];

    if ((memcmp(bank->name.data(), name, 16) == 0) && bank->in_use) {
      return bank;
    }
    idx--;
  }
}

void ReloadBankInfo() {
  for (auto& b : gBanks) {
    if (b->bank_handle) {
      ReadBankSoundInfo(b, b, 1);
    }
  }
}
