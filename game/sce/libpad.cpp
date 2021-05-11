#include "common/util/assert.h"
#include "libpad.h"

namespace ee {
int scePadPortOpen(int port, int slot, void*) {
  // we are expected to return a non-zero file descriptor.
  // we return the port + 1 and succeed always.
  // the game just opens this once at the beginning, we don't have to implement closing/reopening.
  assert(slot == 0);
  return port + 1;
}
}  // namespace ee
