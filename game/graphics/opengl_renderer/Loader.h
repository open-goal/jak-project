#pragma once

#include <thread>
#include <mutex>
#include <condition_variable>

#include "game/graphics/pipelines/opengl.h"

#include "common/custom_data/Tfrag3Data.h"

class Loader {
 public:
  Loader();
  ~Loader();
  void update();

  struct LevelData {
    std::unique_ptr<tfrag3::Level> level;
    std::vector<GLuint> textures;
    u64 load_id = 0;
  };

  const LevelData* get_tfrag3_level(const std::string& level_name);
  void hack_scramble_textures();

 private:
  struct Level {
    LevelData data;
    int frames_since_last_used = 0;
  };

  void loader_thread();

  std::unordered_map<std::string, Level> m_loaded_tfrag3_levels;
  std::unordered_map<std::string, Level> m_initializing_tfrag3_levels;

  std::string m_level_to_load;

  std::thread m_loader_thread;
  std::mutex m_loader_mutex;
  std::condition_variable m_loader_cv;
  bool m_want_shutdown = false;
  uint64_t m_id = 0;
};
