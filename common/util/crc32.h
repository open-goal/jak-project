#pragma once

#include <cstdlib>

#include "common/common_types.h"

u32 crc32(const u8* data, size_t size);

#include <cstring>

#ifdef __aarch64__
#include <arm_acle.h>
// Computes CRC32C
inline u32 crc32(const u8* data, size_t size) {
  u32 result = 0xffffffff;
  while (size >= 4) {
    result = __crc32cw(result, *reinterpret_cast<const u32*>(data));
    data += 4;
    size -= 4;
  }
  while (size) {
    result = __crc32cb(result, *data);
    data++;
    size--;
  }
  return ~result;
}
#else
#include <immintrin.h>
// Computes CRC32C
inline u32 crc32(const u8* data, size_t size) {
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