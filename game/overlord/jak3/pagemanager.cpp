#include "pagemanager.h"

namespace jak3 {

static inline int ctz(u32 x) {
  // Isolate lowest bit and subtract 1
  x = (x & -x) - 1;

  // popcount
  x -= (x >> 1) & 0x55555555;
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x += (x >> 8);
  x += (x >> 16);
  x &= 0x3f;

  return static_cast<int>(x);
}

}  // namespace jak3
