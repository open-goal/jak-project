#include "ssound.h"

#include "common/util/Assert.h"

namespace jak3 {

s32 g_n989Semaphore = -1;
bool g_bSoundEnable = true;
void jak3_overlord_init_globals_ssound() {
  g_bSoundEnable = true;
  g_n989Semaphore = -1;
}
void InitSound() {
  ASSERT_NOT_REACHED();
}
}  // namespace jak3