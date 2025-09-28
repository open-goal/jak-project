#include "sbank.h"

#include "common/util/Assert.h"

#include "game/overlord/jak3/overlord.h"

namespace jak3 {

constexpr int kNumBanks = 8;
SoundBankInfo* gBanks[kNumBanks];

SoundBankInfo gCommonBank;
SoundBankInfo gModeBank;
SoundBankInfo gLevel0Bank, gLevel0hBank;
SoundBankInfo gLevel1Bank, gLevel1hBank;
SoundBankInfo gLevel2Bank, gLevel2hBank;

void jak3_overlord_init_globals_sbank() {
  gBanks[0] = &gCommonBank;
  gBanks[1] = &gModeBank;
  gBanks[2] = &gLevel0Bank;
  gBanks[3] = &gLevel0hBank;
  gBanks[4] = &gLevel1Bank;
  gBanks[5] = &gLevel1hBank;
  gBanks[6] = &gLevel2Bank;
  gBanks[7] = &gLevel2hBank;
}

// added
void PrintBanks() {
  printf("Loaded Banks\n");
  for (int i = 0; i < kNumBanks; i++) {
    printf(" [%d] %s %s (%d/%d)\n", i, gBanks[i]->m_name1, gBanks[i]->m_name2, gBanks[i]->in_use,
           gBanks[i]->loaded);
  }
}

void InitBanks() {
  for (int i = 0; i < kNumBanks; i++) {
    auto* bank = gBanks[i];
    bank->in_use = 0;
    bank->snd_handle = nullptr;
    bank->loaded = false;
    bank->idx = i;
    bank->unk0 = 0;
  }

  strncpyz(gBanks[0]->m_name2, "common", 0x10);
  gBanks[0]->m_nSpuMemSize = 0xbbe40;
  gBanks[0]->m_nSpuMemLoc = 0x1d1c0;

  strncpyz(gBanks[1]->m_name2, "mode", 0x10);
  gBanks[1]->m_nSpuMemSize = 0x25400;
  gBanks[1]->m_nSpuMemLoc = 0xe0000;

  strncpyz(gBanks[2]->m_name2, "level0", 0x10);
  gBanks[2]->m_nSpuMemLoc = 0x105400;
  gBanks[2]->m_nSpuMemSize = 0x51400;

  strncpyz(gBanks[3]->m_name2, "level0h", 0x10);
  gBanks[3]->m_nSpuMemLoc = 0x12de00;
  gBanks[3]->m_nSpuMemSize = 0x28a00;

  strncpyz(gBanks[4]->m_name2, "level1", 0x10);
  gBanks[4]->m_nSpuMemSize = 0x51400;
  gBanks[4]->m_nSpuMemLoc = 0x156800;

  strncpyz(gBanks[5]->m_name2, "level1h", 0x10);
  gBanks[5]->m_nSpuMemSize = 0x28a00;
  gBanks[5]->m_nSpuMemLoc = 0x17f200;

  strncpyz(gBanks[6]->m_name2, "level2", 0x10);
  gBanks[6]->m_nSpuMemSize = 0x51400;
  gBanks[6]->m_nSpuMemLoc = 0x1a7c00;

  strncpyz(gBanks[7]->m_name2, "level2h", 0x10);
  gBanks[7]->m_nSpuMemSize = 0x28a00;
  gBanks[7]->m_nSpuMemLoc = 0x1d0600;
}

SoundBankInfo* AllocateBankName(const char* name, u32 mode) {
  int iVar1;
  int mem_sz;
  SoundBankInfo** ppSVar2;
  int iVar3;
  int iVar4;
  SoundBankInfo* pSVar5;
  int iVar6;
  int iVar6_d4 = 2;
  SoundBankInfo* bank = nullptr;

  // handle common case
  if (memcmp(name, "common", 7) == 0 || memcmp(name, "commonj", 8) == 0) {
    if (!gBanks[0]->in_use) {
      bank = gBanks[0];
    }
  } else if (memcmp(name, "mode", 4) == 0) {
    if (!gBanks[1]->in_use) {
      bank = gBanks[1];
    }
  }

  if (mode == 4) {
    for (int bank_idx = 2; bank_idx < kNumBanks; bank_idx += 2) {
      if (!gBanks[bank_idx]->in_use && !gBanks[bank_idx + 1]->in_use) {
        bank = gBanks[bank_idx];
        bank->m_nSpuMemSize = 0x51400;
        break;
      }
    }
  } else if (mode > 3 && (mode - 6u < 3)) {  // wtf
    iVar1 = 2;
    // iVar6 = 8;
    iVar6_d4 = 2;
    while (gBanks[iVar6_d4]->in_use == 0 || gBanks[iVar6_d4]->mode != mode) {
      auto* sbi = gBanks[iVar6_d4 + 1];
      iVar6 = iVar6 + 8;
      if (((sbi->in_use != 0) && (iVar4 = iVar1, sbi->mode == mode)) ||
          (iVar1 = iVar1 + 2, iVar4 = -1, 7 < iVar1))
        goto LAB_0000c2fc;
    }
    iVar4 = iVar1 + 1;
  LAB_0000c2fc:
    if (iVar4 < 0) {
      iVar1 = 2;
      ppSVar2 = gBanks;
    LAB_0000c36c:
      ppSVar2 = ppSVar2 + 2;
      pSVar5 = *ppSVar2;
      iVar1 = iVar1 + 2;
      if ((pSVar5->in_use != 0) || (ppSVar2[1]->in_use != 0))
        goto LAB_0000c39c;
      mem_sz = 0x28a00;
      pSVar5->m_nSpuMemSize = mem_sz;
      bank = pSVar5;
      goto LAB_0000c3a4;
    }
    pSVar5 = gBanks[iVar4];
    if (pSVar5->in_use == 0) {
      gBanks[iVar1]->m_nSpuMemSize = 0x28a00;
      bank = pSVar5;
    }
  }
LAB_0000c3a4:
  if (bank) {
    bank->mode = mode;
    bank->snd_handle = nullptr;
    bank->unk0 = 0;
  }
  return bank;

LAB_0000c39c:
  if (7 < iVar1)
    goto LAB_0000c3a4;
  goto LAB_0000c36c;
}

SoundBankInfo* LookupBank(const char* name) {
  for (int i = kNumBanks; i-- > 0;) {
    if (memcmp(name, gBanks[i]->m_name1, 16) == 0) {
      return gBanks[i];
    }
  }
  return nullptr;
}

int GetFalloffCurve(int x) {
  if (x < 0) {
    return 1;
  }
  if (x == 0 || 0x28 < x) {
    x = 2;
  }
  return x;
}
}  // namespace jak3