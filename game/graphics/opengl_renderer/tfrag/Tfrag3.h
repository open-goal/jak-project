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
    math::Vector4f planes[4];
    bool do_culling = false;
    bool debug_culling = false;
    // todo culling planes
    // todo occlusion culling string.
  };

  Tfrag3();
  ~Tfrag3();

  void debug_render_all_trees_nolores(const RenderSettings& settings,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof);

  void render_all_trees(const RenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  void render_matching_trees(const std::vector<tfrag3::TFragmentTreeKind>& trees,
                             const RenderSettings& settings,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof);

  void render_tree(const RenderSettings& settings,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof,
                   bool use_vis);

  void setup_for_level(const std::string& level, SharedRenderState* render_state);
  void discard_tree_cache();

  void render_tree_cull_debug(const RenderSettings& settings,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof);

  void draw_debug_window();
  struct DebugVertex {
    math::Vector3f position;
    math::Vector4f rgba;
  };

 private:
  void first_draw_setup(const RenderSettings& settings, SharedRenderState* render_state);
  enum class DoubleDrawKind { NONE, AFAIL_NO_DEPTH_WRITE };
  struct DoubleDraw {
    DoubleDrawKind kind = DoubleDrawKind::NONE;
    float aref = 0.;
  };

  DoubleDraw setup_shader(const RenderSettings& settings,
                          SharedRenderState* render_state,
                          DrawMode mode);
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
    const std::vector<tfrag3::VisNode>* vis = nullptr;

    std::vector<u8> vis_temp;
    std::vector<u32> culled_indices;
    int num_vis_tree_roots = 0;
    int vis_tree_root = 0;
    int first_vis_leaf = 0;

    void reset_stats() {
      rendered_this_frame = false;
      tris_this_frame = 0;
      draws_this_frame = 0;
    }
    bool rendered_this_frame = false;
    int tris_this_frame = 0;
    int draws_this_frame = 0;
    bool allowed = true;
    bool forced = false;
    bool cull_debug = false;
  };

  std::string m_level_name;

  std::vector<GLuint> m_textures;
  std::vector<TreeCache> m_cached_trees;
  GLuint m_time_of_day_texture = -1;
  bool m_has_time_of_day_texture = false;

  std::vector<math::Vector<u8, 4>> m_color_result;

  bool m_has_index_buffer = false;
  GLuint m_index_buffer = -1;

  GLuint m_debug_vao = -1;
  GLuint m_debug_verts = -1;

  // in theory could be up to 4096, I think, but we don't see that many...
  // should be easy to increase (will require a shader change too for indexing)
  static constexpr int TIME_OF_DAY_COLOR_COUNT = 2048;

  static constexpr int DEBUG_TRI_COUNT = 4096;
  std::vector<DebugVertex> m_debug_vert_data;
};
