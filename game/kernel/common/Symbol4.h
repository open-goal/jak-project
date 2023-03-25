#pragma once

#include "common/common_types.h"
#include "common/goal_constants.h"
#include "common/util/Assert.h"

#include "game/kernel/common/Ptr.h"

template <typename T>
struct Symbol4 {
  u8 foo;
  T& value() { return *reinterpret_cast<T*>(&foo - 1); }
  const T& value() const { return *reinterpret_cast<T*>(&foo - 1); }
  const char* name_cstr() const {
    return (const char*)(g_ee_main_mem + 4 +
                         *reinterpret_cast<const u32*>(&foo + jak2::SYM_TO_STRING_OFFSET));
  }
};
