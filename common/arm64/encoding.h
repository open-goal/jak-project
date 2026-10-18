#pragma once

#include "common/common_types.h"
#include "common/util/Assert.h"

namespace arm64 {

constexpr u32 encode_movz_64(u32 reg, u16 value, u32 halfword) {
  ASSERT(reg < 32);
  ASSERT(halfword < 4);
  return 0xd2800000 | (halfword << 21) | (u32(value) << 5) | reg;
}

constexpr u32 encode_movk_64(u32 reg, u16 value, u32 halfword) {
  ASSERT(reg < 32);
  ASSERT(halfword < 4);
  return 0xf2800000 | (halfword << 21) | (u32(value) << 5) | reg;
}

}  // namespace arm64
