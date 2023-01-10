#pragma once

#include "common/common_types.h"

class IBoot {
 public:
  virtual ~IBoot() = default;

  virtual s32 goal_main(int argc, const char* const* argv) = 0;
  virtual void init_globals(void) = 0;
};
