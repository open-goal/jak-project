#include "FrameLimiter.h"
#include "common/common_types.h"
#include <thread>
#include <stdio.h>

double FrameLimiter::round_to_nearest_60fps(double current) {
  double one_frame = 1.f / 60.f;
  int frames_missed = (current / one_frame);  // rounds down
  if (frames_missed > 4) {
    frames_missed = 4;
  }
  return (frames_missed + 1) * one_frame;
}

#ifdef __linux__

FrameLimiter::FrameLimiter() {}

FrameLimiter::~FrameLimiter() {}

void sleep_us(u64 us) {
  std::this_thread::sleep_for(std::chrono::microseconds(us));
}

#else
#define NOMINMAX
#include <Windows.h>

void sleep_us(u64 us) {
  Sleep(us);
}

FrameLimiter::FrameLimiter() {
  timeBeginPeriod(1);
}

FrameLimiter::~FrameLimiter() {
  timeEndPeriod(0);
}

#endif
void FrameLimiter::run(double target_fps, bool do_sleeps, double engine_time) {
  // we want to exit this function when the current time reaches m_us_target.
  // and update m_us_target for the next frame.
  constexpr s64 max_lag_us = 20000;
  constexpr s64 max_ahead_us = 50000;

  s64 entry_time = m_timer.getUs();

  // first, see if we're lagging too far behind:
  if (entry_time > m_us_target + max_lag_us) {
    m_us_target = entry_time - max_lag_us;
  }

  // see if we're too far ahead
  if (m_us_target > entry_time + max_ahead_us) {
    m_us_target = entry_time + max_ahead_us;
  }

  // sleep!
  s64 remaining = m_us_target - entry_time;
  if (do_sleeps && remaining > 1000) {
    sleep_us(remaining - 1000);
  }
  while (m_timer.getUs() < m_us_target) {
    // wait....
  }

  // now update the next target...
  m_us_target += 1e6 * round_to_nearest_60fps(engine_time) * 60 / target_fps;
}