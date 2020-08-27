#ifndef RUNTIME_TIMER_H
#define RUNTIME_TIMER_H

#include <cstdint>
#include <ctime>
#include <cstdint>

class Timer {
 public:
  explicit Timer() { start(); }

  void start() { clock_gettime(CLOCK_MONOTONIC, &_startTime); }

  double getMs() { return (double)getNs() / 1.e6; }

  int64_t getNs() {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (int64_t)(now.tv_nsec - _startTime.tv_nsec) +
           1000000000 * (now.tv_sec - _startTime.tv_sec);
  }

  double getSeconds() { return (double)getNs() / 1.e9; }

  struct timespec _startTime;
};

#endif  // RUNTIME_TIMER_H
