#include "stubs.h"

#include <stdexcept>

#include "common/util/Assert.h"

namespace ee {

void sceGsResetGraph() {
  ASSERT(false);
}

void sceGsPutIMR() {
  ASSERT(false);
}

void sceGsGetIMR() {
  ASSERT(false);
}

void sceGsExecStoreImage() {
  ASSERT(false);
}

void FlushCache() {}

}  // namespace ee

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk) {
  (void)sector;
  (void)unk;
  ASSERT(false);
  return 0;
}

u32 snd_GetLastLoadError() {
  ASSERT(false);
  return 0;
}

void snd_ResolveBankXREFS() {
  ASSERT(false);
}

}  // namespace iop
