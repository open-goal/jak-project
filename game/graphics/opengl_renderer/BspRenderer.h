#pragma once

#include "common/versions/versions.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class BspRenderer {
public:
  BspRenderer(GameVersion version);
  void render(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();
  ~BspRenderer();

private:
  int min_leaf = 0;
  int max_leaf = UINT16_MAX;
  struct LevelCache {
    std::string name;
    GLuint index_buffer = -1;
    GLuint vertex_buffer = -1;
    int index_count = 0;
  };

  void unload_cached_for_name(const std::string& name);
  void load_level(LevelData* level, LevelCache* lc);
  void unload_level(LevelCache* lc);
  void render_level(SharedRenderState* render_state, ScopedProfilerNode& prof, LevelCache* lc);


  std::map<uint64_t, LevelCache> m_level_cache;

  GLuint m_vao;
};
