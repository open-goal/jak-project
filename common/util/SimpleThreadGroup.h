#include <functional>
#include <thread>
#include <vector>

/*!
 * Very simple group of threads.
 * Does:
 *  for (int i = 0; i < num_runs; i++) {
 *    func(i);
 *  }
 * but in parallel.
 *
 * Two things to watch out for:
 * - you must call join before this object is destroyed. The pattern of "join in the destructor"
 *   can cause confusing issues where resources used by threads are destroyed before the threads
 *   are joined, if you aren't careful about the order you declare variables.
 * - the function is copied (once)
 */
class SimpleThreadGroup {
 public:
  void run(const std::function<void(int)>& func, int num_runs, int num_workers);
  void run(const std::function<void(int)>& func, int num_runs);
  void join();

 private:
  bool m_joined = true;
  std::vector<std::thread> m_threads;
  std::function<void(int)> m_func;
};