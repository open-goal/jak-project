#pragma once

#include "common/util/Timer.h"

class FrameLimiter {
 public:
  void run(double target_fps, bool experimental_accurate_lag, double engine_time) {
    double target_seconds;
    if (experimental_accurate_lag) {
      target_seconds = round_to_nearest_60fps(engine_time);
    } else {
      target_seconds = 1.f / target_fps;
    }
    double remaining_time = target_seconds - m_timer.getSeconds();
    while (remaining_time > 0) {
      if (remaining_time > 0.003) {
        std::this_thread::sleep_for(std::chrono::microseconds(int(remaining_time * 1e6 * 0.5)));
      }
      remaining_time = target_seconds - m_timer.getSeconds();
    }

    m_timer.start();
  }

 private:
  double round_to_nearest_60fps(double current) {
    double one_frame = 1.f / 60.f;
    int frames_missed = (current / one_frame);  // rounds down
    if (frames_missed > 4) {
      frames_missed = 4;
    }
    return (frames_missed + 1) * one_frame;
  }

  Timer m_timer;
};