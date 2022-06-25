#pragma once

#include "common/common_types.h"

template <typename T>
struct Symbol4 {
  u8 foo;
  T& value() { return *reinterpret_cast<T*>(&foo - 1); }
  const T& value() const { return *reinterpret_cast<T*>(&foo - 1); }
};