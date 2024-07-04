#include "ssound.h"

#include "common/util/Assert.h"

namespace jak3 {

s32 g_n989Semaphore = -1;
bool g_bSoundEnable = true;
u32 g_anStreamVoice[6];
VolumePair g_aPanTable[361];
void jak3_overlord_init_globals_ssound() {
  g_bSoundEnable = true;
  g_n989Semaphore = -1;
  for (auto& x : g_anStreamVoice) {
    x = 0;
  }
}
void InitSound() {
  ASSERT_NOT_REACHED();
}
}  // namespace jak3