#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"

class Tfrag3 {
 public:
  struct RenderSettings {
    math::Matrix4f math_camera;
    math::Vector4f hvdf_offset;
    float fog_x;
    const u8* rgba_data;
    int tree_idx;
    // todo culling planes
    // todo occlusion culling string.
  };

  Tfrag3();
  ~Tfrag3();

  void debug_render_all_trees(const RenderSettings& settings,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof);
  void render_tree(const RenderSettings& settings,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof);
  void setup_for_level(const std::string& level, SharedRenderState* render_state);
  void discard_tree_cache();

 private:
  void first_draw_setup(const RenderSettings& settings, SharedRenderState* render_state);
  void setup_shader(const RenderSettings& settings, SharedRenderState* render_state, DrawMode mode);

  std::string m_level_name;

  struct TreeCache {
    tfrag3::TFragmentTreeKind kind;
    GLuint vertex_buffer = -1;
    GLuint vao;
    u32 vert_count = 0;
    const std::vector<tfrag3::Draw>* draws = nullptr;
  };

  std::vector<GLuint> m_textures;
  std::vector<TreeCache> m_cached_trees;

  bool m_has_index_buffer = false;
  GLuint m_index_buffer = -1;
};
