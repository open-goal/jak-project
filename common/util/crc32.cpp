#include "crc32.h"

#include <cstring>

#ifdef __aarch64__
#include <arm_acle.h>
u32 crc32(const u8* data, size_t size) {
  u32 result = 0xffffffff;
  while (size >= 4) {
    u32 x;
    memcpy(&x, data, 4);
    data += 4;
    size -= 4;
    result = __crc32w(result, x);
  }
  while (size) {
    result = __crc32b(result, *data);
    data++;
    size--;
  }
  return ~result;
}
#else
#include <immintrin.h>
u32 crc32(const u8* data, size_t size) {
  u32 result = 0xffffffff;
  while (size >= 4) {
    u32 x;
    memcpy(&x, data, 4);
    data += 4;
    size -= 4;
    result = _mm_crc32_u32(result, x);
  }
  while (size) {
    result = _mm_crc32_u8(result, *data);
    data++;
    size--;
  }
  return ~result;
}
#endif
