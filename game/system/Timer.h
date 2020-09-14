#pragma once

#ifndef RUNTIME_TIMER_H
#define RUNTIME_TIMER_H

#ifdef _WIN32
#include <Windows.h>
#endif

#include <cstdint>
#include <ctime>
#include <cstdint>

class Timer {
 public:
  explicit Timer() { start(); }

#ifdef _WIN32
#define MS_PER_SEC 1000ULL  // MS = milliseconds
#define US_PER_MS 1000ULL   // US = microseconds
#define HNS_PER_US 10ULL    // HNS = hundred-nanoseconds (e.g., 1 hns = 100 ns)
#define NS_PER_US 1000ULL

#define HNS_PER_SEC (MS_PER_SEC * US_PER_MS * HNS_PER_US)
#define NS_PER_HNS (100ULL)  // NS = nanoseconds
#define NS_PER_SEC (MS_PER_SEC * US_PER_MS * NS_PER_US)
  int Timer::clock_gettime_monotonic(struct timespec* tv) {
    static LARGE_INTEGER ticksPerSec;
    LARGE_INTEGER ticks;
    double seconds;

    if (!ticksPerSec.QuadPart) {
      QueryPerformanceFrequency(&ticksPerSec);
      if (!ticksPerSec.QuadPart) {
        errno = ENOTSUP;
        return -1;
      }
    }

    QueryPerformanceCounter(&ticks);

    seconds = (double)ticks.QuadPart / (double)ticksPerSec.QuadPart;
    tv->tv_sec = (time_t)seconds;
    tv->tv_nsec = (long)((ULONGLONG)(seconds * NS_PER_SEC) % NS_PER_SEC);

    return 0;
  }
#endif

  void start() {
#ifdef _WIN32
    clock_gettime_monotonic(&_startTime);
#elif __linux__
    clock_gettime(CLOCK_MONOTONIC, &_startTime);
#endif
  }

  double getMs() { return (double)getNs() / 1.e6; }

  int64_t getNs() {
    struct timespec now;
#ifdef _WIN32
    clock_gettime_monotonic(&now);
#elif __linux__
    clock_gettime(CLOCK_MONOTONIC, &now);
#endif

    return (int64_t)(now.tv_nsec - _startTime.tv_nsec) +
           1000000000 * (now.tv_sec - _startTime.tv_sec);
  }

  double getSeconds() { return (double)getNs() / 1.e9; }

  struct timespec _startTime;
};

#endif  // RUNTIME_TIMER_H
