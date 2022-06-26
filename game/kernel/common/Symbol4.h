#pragma once

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/kernel/common/Ptr.h"

template <typename T>
struct Symbol4 {
  u8 foo;
  T& value() { return *reinterpret_cast<T*>(&foo - 1); }
  const T& value() const { return *reinterpret_cast<T*>(&foo - 1); }
  const char* name_cstr() const {
    ASSERT_MSG(false, "nyi");
    return nullptr;
  }
};
