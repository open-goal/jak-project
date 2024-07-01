#include "sbank.h"

#include "overlord.h"

namespace jak3 {

SoundBankInfo gCommonBank, gModeBank;
SoundBankInfo gLevelBanks[6];

SoundBankInfo* gBanks[8] = {
    &gCommonBank,    &gModeBank,      &gLevelBanks[0], &gLevelBanks[1],
    &gLevelBanks[2], &gLevelBanks[3], &gLevelBanks[4], &gLevelBanks[5],
};

void InitBanks() {
  for (int i = 0; i < 8; i++) {
    gBanks[i]->in_use = false;
    gBanks[i]->unk = 0;
    gBanks[i]->unk2 = 0;
    gBanks[i]->index = i;
  }

  strncpyz(gBanks[0]->slot_name, "common", 16);
  gBanks[0]->spu_size = 0xbbe40;
  gBanks[0]->spu_loc = 0x1d1c0;
  strncpyz(gBanks[1]->slot_name, "mode", 16);
  gBanks[1]->spu_size = 0x25400;
  gBanks[1]->spu_loc = 0xe0000;
  strncpyz(gBanks[2]->slot_name, "level0", 16);
  gBanks[2]->spu_size = 0x51400;
  gBanks[2]->spu_loc = 0x105400;
  strncpyz(gBanks[3]->slot_name, "level0h", 16);
  gBanks[3]->spu_size = 0x28a00;
  gBanks[3]->spu_loc = 0x12de00;
  strncpyz(gBanks[4]->slot_name, "level1", 16);
  gBanks[4]->spu_size = 0x51400;
  gBanks[4]->spu_loc = 0x156800;
  strncpyz(gBanks[5]->slot_name, "level1h", 16);
  gBanks[5]->spu_size = 0x28a00;
  gBanks[5]->spu_loc = 0x17f200;
  strncpyz(gBanks[6]->slot_name, "level2", 16);
  gBanks[6]->spu_size = 0x51400;
  gBanks[6]->spu_loc = 0x1a7c00;
  strncpyz(gBanks[7]->slot_name, "level2h", 16);
  gBanks[7]->spu_size = 0x28a00;
  gBanks[7]->spu_loc = 0x1d0600;
}

}  // namespace jak3
