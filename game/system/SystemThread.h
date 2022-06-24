#pragma once

/*!
 * @file SystemThread.h
 * Threads for the runtime.
 */

#ifndef RUNTIME_SYSTEMTHREAD_H
#define RUNTIME_SYSTEMTHREAD_H

#include <array>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <string>
#include <thread>

#include "common/util/Timer.h"

constexpr int MAX_SYSTEM_THREADS = 16;

class SystemThreadInterface;
class SystemThreadManager;

/*!
 * Runs a function in a thread and provides a SystemThreadInterface to that function.
 * Once the thread is ready, it should tell the interface with intitialization_complete().
 * Thread functions should try to return when get_want_exit() returns true.
 */
class SystemThread {
 public:
  void start(std::function<void(SystemThreadInterface&)> f);
  void join();
  void stop();
  SystemThread() = default;

 private:
  friend class SystemThreadInterface;
  friend class SystemThreadManager;
  friend void* bootstrap_thread_func(void* thd);

  std::string name = "invalid";
  std::thread thread;
  SystemThreadManager* manager;
  std::function<void(SystemThreadInterface&)> function;
  bool initialization_complete = false;
  std::mutex initialization_mutex;
  std::condition_variable initialization_cv;
  Timer stats_timer;
  Timer stat_diff_timer;
  double cpu_user = 0, cpu_kernel = 0;
  int id = -1;
  bool want_exit = false;
  bool running = false;
};

/*!
 * The interface used by a thread in the runtime.
 */
class SystemThreadInterface {
 public:
  SystemThreadInterface(SystemThread* p) : thread(*p) {}
  void initialization_complete();
  bool get_want_exit() const;
  void trigger_shutdown();

 private:
  SystemThread& thread;
};

/*!
 * A manager of all threads in the runtime.
 */
class SystemThreadManager {
 public:
  SystemThread& create_thread(const std::string& name);
  void print_stats();
  void shutdown();
  void join();
  bool all_threads_exiting();

 private:
  std::array<SystemThread, MAX_SYSTEM_THREADS> threads;
  int thread_count = 0;
};

#endif  // RUNTIME_SYSTEMTHREAD_H
