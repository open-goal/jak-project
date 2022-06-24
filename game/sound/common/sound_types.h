#pragma once
#include <algorithm>

#include "common/common_types.h"

namespace snd {

struct vol_pair {
  s16 left;
  s16 right;
};

struct s16_output {
  s16 left{0}, right{0};

  s16_output& operator+=(const s16_output& rhs) {
    left = static_cast<s16>(std::clamp<s32>(left + rhs.left, INT16_MIN, INT16_MAX));
    right = static_cast<s16>(std::clamp<s32>(right + rhs.right, INT16_MIN, INT16_MAX));
    return *this;
  }
};

}  // namespace snd
