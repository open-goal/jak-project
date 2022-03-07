#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"
#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/opengl_renderer/background/background_common.h"

class Tfrag3 {
 public:
  Tfrag3();
  ~Tfrag3();

  void debug_render_all_trees_nolores(int geom,
                                      const TfragRenderSettings& settings,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof);

  void render_all_trees(int geom,
                        const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  void render_matching_trees(int geom,
                             const std::vector<tfrag3::TFragmentTreeKind>& trees,
                             const TfragRenderSettings& settings,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof);

  void render_tree(int geom,
                   const TfragRenderSettings& settings,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof);

  bool setup_for_level(const std::vector<tfrag3::TFragmentTreeKind>& tree_kinds,
                       const std::string& level,
                       SharedRenderState* render_state);
  void discard_tree_cache();

  void render_tree_cull_debug(const TfragRenderSettings& settings,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof);

  void draw_debug_window();
  struct DebugVertex {
    math::Vector3f position;
    math::Vector4f rgba;
  };

  bool update_load(const std::vector<tfrag3::TFragmentTreeKind>& tree_kinds,
                   const tfrag3::Level* lev_data,
                   std::string& status_out);

  int lod() const { return Gfx::g_global_settings.lod_tfrag; }

 private:
  static constexpr int GEOM_MAX = 3;

  struct TreeCache {
    tfrag3::TFragmentTreeKind kind;
    GLuint vertex_buffer = -1;
    GLuint index_buffer = -1;
    std::vector<u32> index_list;
    GLuint time_of_day_texture;
    GLuint vao;
    u32 vert_count = 0;
    const std::vector<tfrag3::StripDraw>* draws = nullptr;
    const std::vector<tfrag3::TimeOfDayColor>* colors = nullptr;
    const tfrag3::BVH* vis = nullptr;
    SwizzledTimeOfDay tod_cache;

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

  struct Cache {
    std::vector<u8> vis_temp;
    std::vector<std::pair<int, int>> draw_idx_temp;
  } m_cache;

  std::string m_level_name;

  const std::vector<GLuint>* m_textures = nullptr;
  std::array<std::vector<TreeCache>, GEOM_MAX> m_cached_trees;

  std::vector<math::Vector<u8, 4>> m_color_result;

  GLuint m_debug_vao = -1;
  GLuint m_debug_verts = -1;

  u64 m_load_id = -1;

  // in theory could be up to 4096, I think, but we don't see that many...
  // should be easy to increase
  static constexpr int TIME_OF_DAY_COLOR_COUNT = 8192;

  static constexpr int DEBUG_TRI_COUNT = 4096;
  std::vector<DebugVertex> m_debug_vert_data;

  bool m_has_level = false;
  bool m_use_fast_time_of_day = true;

  enum State : u32 {
    FIRST = 0,
    DISCARD_TREE = 0,
    FREE_OLD_TREES = 1,
    INIT_NEW_TREES = 2,
    UPLOAD_VERTS = 3,
  };

  struct {
    bool loading = false;
    State state;
    u32 vert = 0;
    u32 vert_geo = 0;
    u32 vert_tree = 0;
    u32 vert_debug_bytes = 0;
  } m_load_state;
  static constexpr int MAX_TEX_PER_FRAME = 4;
};
