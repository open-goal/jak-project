#pragma once

#include "common/util/Timer.h"

class FrameLimiter {
 public:
  FrameLimiter();
  ~FrameLimiter();

  void run(double target_fps, bool experimental_accurate_lag, bool do_sleeps, double engine_time);

 private:
  double round_to_nearest_60fps(double current);

  Timer m_timer;
};