#pragma once
#include <algorithm>

#include "common/common_types.h"

namespace snd {

struct VolPair {
  s16 left;
  s16 right;
};

struct s16Output {
  s16 left{0}, right{0};

  s16Output& operator+=(const s16Output& rhs) {
    left = static_cast<s16>(std::clamp<s32>(left + rhs.left, INT16_MIN, INT16_MAX));
    right = static_cast<s16>(std::clamp<s32>(right + rhs.right, INT16_MIN, INT16_MAX));
    return *this;
  }
};

}  // namespace snd
