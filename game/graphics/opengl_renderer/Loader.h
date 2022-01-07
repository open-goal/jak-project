#pragma once

#include <thread>
#include <mutex>
#include <condition_variable>

#include "common/custom_data/Tfrag3Data.h"

class Loader {
 public:
  Loader();
  ~Loader();
  tfrag3::Level* get_tfrag3_level(const std::string& level_name);

 private:
  struct Level {
    std::unique_ptr<tfrag3::Level> level;
    int frames_since_last_used = 0;
  };

  void loader_thread();

  std::unordered_map<std::string, Level> m_tfrag3_levels;
  std::string m_level_to_load;

  std::thread m_loader_thread;
  std::mutex m_loader_mutex;
  std::condition_variable m_loader_cv;
  bool m_want_shutdown = false;
};
