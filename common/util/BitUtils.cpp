#include "BitUtils.h"

#include <cstring>

bool integer_fits(s64 in, int size, bool is_signed) {
  switch (size) {
    case 1:
      if (is_signed) {
        return in >= INT8_MIN && in <= INT8_MAX;
      } else {
        return in >= 0 && in <= UINT8_MAX;
      }
    case 2:
      if (is_signed) {
        return in >= INT16_MIN && in <= INT16_MAX;
      } else {
        return in >= 0 && in <= UINT16_MAX;
      }
    case 4:
      if (is_signed) {
        return in >= INT32_MIN && in <= INT32_MAX;
      } else {
        return in >= 0 && in <= UINT32_MAX;
      }
    case 8:
      return true;
    default:
      ASSERT(false);
      return false;
  }
}

u32 float_as_u32(float x) {
  u32 result;
  memcpy(&result, &x, 4);
  return result;
}