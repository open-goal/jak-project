#pragma once

#include "common/common_types.h"
#include "common/util/Timer.h"

class FrameLimiter {
 public:
  FrameLimiter();
  ~FrameLimiter();

  void run(double target_fps, bool do_sleeps, double engine_time);

 private:
  double round_to_nearest_60fps(double current);

  Timer m_timer;

  s64 m_us_target = 0;
};