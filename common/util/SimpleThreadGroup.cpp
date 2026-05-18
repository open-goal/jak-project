#include "SimpleThreadGroup.h"

#include "common/util/Assert.h"

void SimpleThreadGroup::run(const std::function<void(int)>& func, int num_runs) {
  int num_workers = std::min(num_runs, (int)std::thread::hardware_concurrency());
  run(func, num_runs, num_workers);
}

void SimpleThreadGroup::run(const std::function<void(int)>& func, int num_runs, int num_workers) {
  ASSERT(m_joined);

  // copy the function, in case the user gives us a temporary function.
  m_func = func;

  int start_idx = 0;
  for (int thread_idx = 0; thread_idx < num_workers; thread_idx++) {
    int end_idx = ((thread_idx + 1) * num_runs) / num_workers;
    if (thread_idx == (num_workers - 1)) {
      end_idx = num_runs;
    }

    m_threads.emplace_back([&, start_idx, end_idx]() {
      for (int i = start_idx; i < end_idx; i++) {
        m_func(i);
      }
    });

    start_idx = end_idx;
  }

  m_joined = false;
}

void SimpleThreadGroup::join() {
  ASSERT(!m_joined);
  for (auto& t : m_threads) {
    t.join();
  }
  m_threads.clear();
  m_joined = true;
}