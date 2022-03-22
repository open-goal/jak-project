#pragma once

#include <optional>

#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"
#include "common/util/FilteredValue.h"

class Tie3 : public BucketRenderer {
 public:
  Tie3(const std::string& name, BucketId my_id, int level_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  ~Tie3();

  void render_all_trees(int geom,
                        const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void render_tree(int idx,
                   int geom,
                   const TfragRenderSettings& settings,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof);
  bool setup_for_level(const std::string& str, SharedRenderState* render_state);

  struct WindWork {
    u32 paused;
    u32 pad[3];
    math::Vector4f wind_array[64];
    math::Vector4f wind_normal;
    math::Vector4f wind_temp;
    float wind_force[64];
    u32 wind_time;
    u32 pad2[3];
  } m_wind_data;

  int lod() const { return Gfx::g_global_settings.lod_tie; }

 private:
  bool update_load(const tfrag3::Level* lev_data, std::string& status_out);
  void discard_tree_cache();
  void render_tree_wind(int idx,
                        int geom,
                        const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  struct Tree {
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint time_of_day_texture;
    std::vector<u32> index_list;
    GLuint vao;
    u32 vert_count;
    const std::vector<tfrag3::StripDraw>* draws = nullptr;
    const std::vector<tfrag3::InstancedStripDraw>* wind_draws = nullptr;
    const std::vector<tfrag3::TieWindInstance>* instance_info = nullptr;
    const std::vector<tfrag3::TimeOfDayColor>* colors = nullptr;
    const tfrag3::BVH* vis = nullptr;
    SwizzledTimeOfDay tod_cache;

    std::vector<std::array<math::Vector4f, 4>> wind_matrix_cache;

    bool has_wind = false;
    GLuint wind_vertex_index_buffer;
    std::vector<u32> wind_vertex_index_offsets;

    struct {
      u32 index_upload = 0;
      u32 verts = 0;
      u32 draws = 0;
      u32 full_draws = 0;  // ones that have all visible
      u32 wind_draws = 0;
      Filtered<float> cull_time;
      Filtered<float> index_time;
      Filtered<float> tod_time;
      Filtered<float> setup_time;
      Filtered<float> draw_time;
      Filtered<float> tree_time;
    } perf;
  };

  std::array<std::vector<Tree>, 4> m_trees;  // includes 4 lods!
  std::string m_level_name;
  const std::vector<GLuint>* m_textures;
  u64 m_load_id = -1;

  struct Cache {
    std::vector<u8> vis_temp;
    std::vector<std::pair<int, int>> draw_idx_temp;
  } m_cache;

  std::vector<math::Vector<u8, 4>> m_color_result;

  static constexpr int TIME_OF_DAY_COLOR_COUNT = 8192;

  bool m_has_level = false;
  char m_user_level[255] = "vi1";
  std::optional<std::string> m_pending_user_level = std::nullopt;
  bool m_override_level = false;
  bool m_use_fast_time_of_day = true;
  bool m_debug_wireframe = false;
  bool m_debug_all_visible = false;
  bool m_hide_wind = false;
  Filtered<float> m_all_tree_time;

  TfragPcPortData m_pc_port_data;

  std::vector<float> m_wind_vectors;  // note: I suspect these are shared with shrub.

  float m_wind_multiplier = 1.f;

  int m_level_id;

  static_assert(sizeof(WindWork) == 84 * 16);

  enum State : u32 {
    FIRST = 0,
    DISCARD_TREE = 0,
    INIT_NEW_TREES = 1,
    UPLOAD_VERTS = 2,
    UPLOAD_WIND_INDEX = 3,
  };

  struct {
    bool loading = false;
    State state;
    u32 tex = 0;

    u32 vert_geo = 0;
    u32 vert_tree = 0;
    u32 vert = 0;

    u32 vert_debug_bytes = 0;

    size_t time_of_day_count = 0;
    size_t vis_temp_len = 0;
    size_t max_draw = 0;
    size_t max_idx_per_draw = 0;
    u16 max_wind_idx = 0;

  } m_load_state;
};
