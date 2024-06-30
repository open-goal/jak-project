#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_ssound();
void InitSound();

extern s32 g_n989Semaphore;
extern bool g_bSoundEnable;

struct SoundBankInfo {
  int m_name1[4];
  char m_name2[16];
  int m_nSpuMemLoc;
  int m_nSpuMemSize;

};
}