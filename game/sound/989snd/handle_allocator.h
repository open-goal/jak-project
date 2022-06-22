// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <queue>
#include <unordered_map>

#include "common/common_types.h"

namespace snd {

class id_allocator {
 public:
  u32 get_id() {
    u32 id = 0;
    if (m_free_ids.empty()) {
      id = next_id++;
    } else {
      id = m_free_ids.front();
      m_free_ids.pop();
    }
    return id;
  }

  void free_id(u32 id) { m_free_ids.push(id); }

 private:
  u32 next_id{1};
  std::queue<u32> m_free_ids;
};

}  // namespace snd
