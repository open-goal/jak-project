#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"

#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/pipelines/opengl.h"

class Tfrag3 {
 public:
  Tfrag3();
  ~Tfrag3();

  void init_shaders(ShaderLibrary& shaders);
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

  void update_load(const std::vector<tfrag3::TFragmentTreeKind>& tree_kinds,
                   const LevelData* loader_data);

  int lod() const { return Gfx::g_global_settings.lod_tfrag; }

 private:
  static constexpr int GEOM_MAX = 3;

  struct TreeCache {
    tfrag3::TFragmentTreeKind kind = tfrag3::TFragmentTreeKind::INVALID;
    GLuint vertex_buffer = -1;
    GLuint index_buffer = -1;
    GLuint single_draw_index_buffer = -1;
    GLuint time_of_day_texture = -1;
    GLuint vao = -1;
    u32 vert_count = 0;
    const std::vector<tfrag3::StripDraw>* draws = nullptr;
    const std::vector<tfrag3::TimeOfDayColor>* colors = nullptr;
    const tfrag3::BVH* vis = nullptr;
    const u32* index_data = nullptr;
    SwizzledTimeOfDay tod_cache;
    u64 draw_mode = 0;

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

    bool freeze_itimes = false;
    math::Vector<s32, 4> itimes_debug[4];
  };

  struct {
    GLuint decal;
  } m_uniforms;

  struct Cache {
    std::vector<u8> vis_temp;
    std::vector<std::pair<int, int>> draw_idx_temp;
    std::vector<u32> index_temp;
    std::vector<std::pair<int, int>> multidraw_offset_per_stripdraw;
    std::vector<GLsizei> multidraw_count_buffer;
    std::vector<void*> multidraw_index_offset_buffer;
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
};
