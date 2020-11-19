#include <stdexcept>
#include <cassert>
#include "stubs.h"

namespace ee {
int scePadPortOpen(int port, int slot, void* data) {
  (void)port;
  (void)slot;
  (void)data;
  assert(false);
  return 0;
}

void sceGsSyncV() {
  assert(false);
}

void sceGsSyncPath() {
  assert(false);
}

void sceGsResetPath() {
  assert(false);
}

void sceGsResetGraph() {
  assert(false);
}

void sceDmaSync() {
  assert(false);
}

void sceGsPutIMR() {
  assert(false);
}

void sceGsGetIMR() {
  assert(false);
}

void sceGsExecStoreImage() {
  assert(false);
}

void FlushCache() {
  assert(false);
}

}  // namespace ee

namespace iop {
u32 snd_BankLoadByLoc(u32 sector, u32 unk) {
  (void)sector;
  (void)unk;
  assert(false);
  return 0;
}

u32 snd_GetLastLoadError() {
  assert(false);
  return 0;
}

void snd_ResolveBankXREFS() {
  assert(false);
}

}  // namespace iop
