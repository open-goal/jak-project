#pragma once

#include <cstdint>
#include <ctime>
#include "common/util/Assert.h"

/*!
 * Timer for measuring time elapsed with clock_monotonic
 */
class Timer {
 public:
  /*!
   * Construct and start timer
   */
  explicit Timer() { start(); }

#ifdef _WIN32
  int clock_gettime_monotonic(struct timespec* tv);
#endif

  /*!
   * Start the timer
   */
  void start();

  /*!
   * Get milliseconds elapsed
   */
  double getMs() { return (double)getNs() / 1.e6; }

  double getUs() { return (double)getNs() / 1.e3; }

  /*!
   * Get nanoseconds elapsed
   */
  int64_t getNs();

  /*!
   * Get seconds elapsed
   */
  double getSeconds() { return (double)getNs() / 1.e9; }

  struct timespec _startTime = {};
};
