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
    float time_of_day_weights[8] = {0};
    // todo culling planes
    // todo occlusion culling string.
  };

  Tfrag3();
  ~Tfrag3();

  void debug_render_all_trees_nolores(const RenderSettings& settings,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof);

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
  void interp_time_of_day_slow(const float weights[8],
                               const std::vector<tfrag3::TimeOfDayColor>& in,
                               math::Vector<u8, 4>* out);

  struct TreeCache {
    tfrag3::TFragmentTreeKind kind;
    GLuint vertex_buffer = -1;
    GLuint vao;
    u32 vert_count = 0;
    const std::vector<tfrag3::Draw>* draws = nullptr;
    const std::vector<tfrag3::TimeOfDayColor>* colors = nullptr;
  };

  std::string m_level_name;

  std::vector<GLuint> m_textures;
  std::vector<TreeCache> m_cached_trees;
  GLuint m_time_of_day_texture = -1;
  bool m_has_time_of_day_texture = false;

  std::vector<math::Vector<u8, 4>> m_color_result;

  bool m_has_index_buffer = false;
  GLuint m_index_buffer = -1;

  // in theory could be up to 4096, I think, but we don't see that many...
  // should be easy to increase (will require a shader change too for indexing)
  static constexpr int TIME_OF_DAY_COLOR_COUNT = 2048;
};
