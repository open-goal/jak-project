#include "overlord.h"

#include <cstdint>
#include <cstdio>
#include <cstring>

namespace jak3 {
extern int start_overlord(int, const char* const*);

void Panic() {
  printf(
      "IOP: *** Overlord panic at 0x%08lx (rel. address 0x%08lx)\n"
      "IOP: *** Check mapfile to determine function name.\n"
      "IOP: *** Thread halted.\n",
      (intptr_t)__builtin_return_address(0),
      (intptr_t)__builtin_return_address(0) - (intptr_t)start_overlord);
  while (true)
    ;
}

char* strncpyz(char* dst, const char* src, size_t sz) {
  if (sz) {
    strncpy(dst, src, sz);
    dst[sz - 1] = '\0';
  }

  return dst;
}

}  // namespace jak3
