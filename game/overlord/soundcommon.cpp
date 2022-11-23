#include "soundcommon.h"

#include <algorithm>
#include <cstdio>
#include <string.h>
#include <string>

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

// I'm not bored enough to reimplement their strcpy
// Only for use with 16 character sound names!
void strcpy_toupper(char* dest, const char* source) {
  // clear the dest string
  memset(dest, 0, 16);
  std::string string(source);
  std::transform(string.begin(), string.end(), string.begin(), ::toupper);
  std::replace(string.begin(), string.end(), '-', '_');
  string.copy(dest, 16);
}

void PrintBankInfo(SoundBank* bank) {
  // we dont need this and it spams the console too much
  return;

  printf("Bank %s\n\n", bank->name);
  for (u32 i = 0; i < bank->sound_count; i++) {
    // Some characters use the full 16 characters (bonelurker-grunt) and dont have a null terminator
    std::string name = std::string(bank->sound[i].name, 16);
    printf("%d : %16s : min %d max %d curve %d\n", i, name.c_str(),
           bank->sound[i].fallof_params & 0x3fff, (bank->sound[i].fallof_params >> 14) & 0x3fff,
           bank->sound[i].fallof_params >> 28);
  }
}
