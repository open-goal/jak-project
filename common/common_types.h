#pragma once

/*!
 * @file common_types.h
 * Common Integer Types.
 */

#include <cstdint>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using s8 = int8_t;
using s16 = int16_t;
using s32 = int32_t;
using s64 = int64_t;

struct u128 {
  union {
    u64 du64[2];
    s64 ds64[2];
    u32 du32[4];
    s32 ds32[4];
    u16 du16[8];
    s16 ds16[8];
    u8 du8[16];
    s8 ds8[16];
    float f[4];
  };
};
static_assert(sizeof(u128) == 16, "u128");

#if defined __linux || defined __linux__ || defined __APPLE__
#define OS_POSIX
#endif
