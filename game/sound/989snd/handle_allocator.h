// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include "common/common_types.h"

namespace snd {

class IdAllocator {
 public:
  // Use a unique ID each time - the Overlord will sometimes do the wrong thing if handles are
  // reused. The real 989snd also includes type + handle index in this.
  u32 GetId() { return mNextId++; }
  void FreeId(u32) {}

 private:
  u32 mNextId{0};
};

}  // namespace snd
