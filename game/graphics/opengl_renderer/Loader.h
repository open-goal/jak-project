#pragma once

#include <thread>
#include <mutex>
#include <condition_variable>

#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/texture/TexturePool.h"
#include "common/custom_data/Tfrag3Data.h"

class Loader {
 public:
  static constexpr float TIE_LOAD_BUDGET = 1.5f;
  static constexpr float SHARED_TEXTURE_LOAD_BUDGET = 3.f;
  Loader();
  ~Loader();
  void update(std::string& status_out, TexturePool& tex_pool);

  struct LevelData {
    std::unique_ptr<tfrag3::Level> level;
    std::vector<GLuint> textures;
    u64 load_id = 0;
  };

  const LevelData* get_tfrag3_level(const std::string& level_name);
  void hack_scramble_textures();
  void load_common(TexturePool& tex_pool, const std::string& name);

  void set_want_levels(const std::vector<std::string>& levels);

 private:
  struct Level {
    LevelData data;
    int frames_since_last_used = 0;
  };

  void loader_thread();
  u64 add_texture(TexturePool& pool, const tfrag3::Texture& tex, bool is_common);

  std::unordered_map<std::string, Level> m_loaded_tfrag3_levels;
  std::unordered_map<std::string, Level> m_initializing_tfrag3_levels;

  tfrag3::Level m_common_level;

  std::string m_level_to_load;

  std::thread m_loader_thread;
  std::mutex m_loader_mutex;
  std::condition_variable m_loader_cv;
  bool m_want_shutdown = false;
  uint64_t m_id = 0;
};
