// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <queue>
#include <unordered_map>

#include "common/common_types.h"

namespace snd {

class IdAllocator {
 public:
  u32 GetId() {
    u32 id = 0;
    if (mFreeIds.empty()) {
      id = mNextId++;
    } else {
      id = mFreeIds.front();
      mFreeIds.pop();
    }
    return id;
  }

  void FreeId(u32 id) { mFreeIds.push(id); }

 private:
  u32 mNextId{1};
  std::queue<u32> mFreeIds;
};

}  // namespace snd
