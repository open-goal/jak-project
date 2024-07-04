#include "vblank_handler.h"

#include "common/util/Assert.h"

namespace jak3 {

u32 g_nInfoEE = 0;
SoundIOPInfo g_SRPCSoundIOPInfo;
void jak3_overlord_init_globals_vblank_handler() {
  g_nInfoEE = 0;
  g_SRPCSoundIOPInfo = {};
}

void VBlank_Initialize() {
  ASSERT_NOT_REACHED();
}
}  // namespace jak3