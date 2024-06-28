#include "ramdisk.h"

#include "common/util/Assert.h"

namespace jak3 {
void jak3_overlord_init_globals_ramdisk() {}

u32 Thread_LoadToEE() {
  ASSERT_NOT_REACHED();
}
}  // namespace jak3