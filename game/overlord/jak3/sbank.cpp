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

}  // namespace jak3