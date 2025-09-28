#pragma once

#include "common/common_types.h"
#include "common/goal_constants.h"
#include "common/util/Assert.h"

#include "game/kernel/common/Ptr.h"

/*!
 * This type is bit strange. The Ptr<Symbol4> should have the same value as the original game's
 * symbol address. The actual value in here is useless - the address of this thing is what matters.
 */
template <typename T>
struct Symbol4 {
  u8 foo;
  T& value() { return *reinterpret_cast<T*>(&foo - 1); }
  const T& value() const { return *reinterpret_cast<T*>(&foo - 1); }
};
