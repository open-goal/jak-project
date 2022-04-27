#pragma once

#include <thread>
#include <mutex>
#include <condition_variable>

#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/texture/TexturePool.h"
#include "common/custom_data/Tfrag3Data.h"
#include "common/util/Timer.h"

class Loader {
 public:
  static constexpr float TIE_LOAD_BUDGET = 1.5f;
  static constexpr float SHARED_TEXTURE_LOAD_BUDGET = 3.f;
  Loader();
  ~Loader();
  void update(TexturePool& tex_pool);
  void update_blocking(TexturePool& tex_pool);

  struct LevelData {
    std::unique_ptr<tfrag3::Level> level;
    std::vector<GLuint> textures;
    u64 load_id = 0;

    struct TieOpenGL {
      GLuint vertex_buffer;
      bool has_wind = false;
      GLuint wind_indices;
    };
    std::array<std::vector<TieOpenGL>, tfrag3::TIE_GEOS> tie_data;
    std::array<std::vector<GLuint>, tfrag3::TIE_GEOS> tfrag_vertex_data;
    std::vector<GLuint> shrub_vertex_data;
    GLuint collide_vertices;

    // internal load state
    bool tie_opengl_created = false;
    bool tie_verts_done = false;
    bool tie_wind_indices_done = false;
    bool tie_load_done = false;
    u32 tie_next_geo = 0;
    u32 tie_next_tree = 0;
    u32 tie_next_vert = 0;

    bool tfrag_opengl_created = false;
    bool tfrag_load_done = false;
    u32 tfrag_next_geo = 0;
    u32 tfrag_next_tree = 0;
    u32 tfrag_next_vert = 0;

    bool shrub_opengl_created = false;
    bool shrub_load_done = false;
    u32 shrub_next_tree = 0;
    u32 shrub_next_vert = 0;
  };

  const LevelData* get_tfrag3_level(const std::string& level_name);
  void load_common(TexturePool& tex_pool, const std::string& name);
  void set_want_levels(const std::vector<std::string>& levels);
  std::vector<LevelData*> get_in_use_levels();

 private:
  struct Level {
    LevelData data;
    int frames_since_last_used = 0;
  };

  void loader_thread();
  u64 add_texture(TexturePool& pool, const tfrag3::Texture& tex, bool is_common);

  bool upload_textures(Timer& timer, LevelData& data, TexturePool& texture_pool);
  bool init_tie(Timer& timer, LevelData& data);
  bool init_tfrag(Timer& timer, LevelData& data);
  bool init_shrub(Timer& timer, LevelData& data);
  bool init_collide(Timer& timer, LevelData& data);

  // used by game and loader thread
  std::unordered_map<std::string, Level> m_initializing_tfrag3_levels;

  tfrag3::Level m_common_level;

  std::string m_level_to_load;

  std::thread m_loader_thread;
  std::mutex m_loader_mutex;
  std::condition_variable m_loader_cv;
  std::condition_variable m_file_load_done_cv;
  bool m_want_shutdown = false;
  uint64_t m_id = 0;

  // used only by game thread
  std::unordered_map<std::string, Level> m_loaded_tfrag3_levels;

  std::vector<std::string> m_desired_levels;
};
