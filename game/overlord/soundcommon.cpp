#include "soundcommon.h"
#include <cstdio>
#include "common/util/Assert.h"

// TODO strcpy_toupper
// TODO atoi
// TODO ReadBankSoundNames

void ReadBankSoundInfo(SoundBank* bank, SoundBank* unk, s32 unk2) {
  (void)bank;
  (void)unk;
  (void)unk2;
  ASSERT(false);
}

void PrintBankInfo(SoundBank* bank) {
  printf("Bank %s\n\n", bank->name);
  for (u32 i = 0; i < bank->sound_count; i++) {
    printf("%d : %16s : min %d max %d curve %d\n", i, bank->sound[i].name,
           bank->sound[i].fallof_params & 0x3fff, (bank->sound[i].fallof_params >> 14) & 0x3fff,
           bank->sound[i].fallof_params >> 28);
  }
}
