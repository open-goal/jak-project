#pragma once

#include <optional>

#include "game/graphics/opengl_renderer/tfrag/tfrag_common.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"
#include "common/util/FilteredValue.h"

class Tie3 : public BucketRenderer {
 public:
  Tie3(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  ~Tie3();

  void render_all_trees(const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void render_tree(int idx,
                   const TfragRenderSettings& settings,
                   SharedRenderState* render_state,
                   ScopedProfilerNode& prof);
  void setup_for_level(const std::string& str, SharedRenderState* render_state);

 private:
  void discard_tree_cache();
  struct Tree {
    GLuint vertex_buffer;
    GLuint vao;
    u32 vert_count;
    const std::vector<tfrag3::StripDraw>* draws = nullptr;
    const std::vector<tfrag3::TimeOfDayColor>* colors = nullptr;
    const tfrag3::BVH* vis = nullptr;
    SwizzledTimeOfDay tod_cache;

    struct {
      u32 index_upload = 0;
      u32 verts = 0;
      u32 draws = 0;
      u32 full_draws = 0;  // ones that have all visible
      Filtered<float> cull_time;
      Filtered<float> index_time;
      Filtered<float> tod_time;
      Filtered<float> setup_time;
      Filtered<float> draw_time;
      Filtered<float> tree_time;
    } perf;
  };

  std::vector<Tree> m_trees;
  std::string m_level_name;
  std::vector<GLuint> m_textures;  // todo, can we share with tfrag in some cases?

  struct Cache {
    std::vector<u8> vis_temp;
    std::vector<std::pair<int, int>> draw_idx_temp;
    std::vector<u32> index_list;
  } m_cache;

  GLuint m_time_of_day_texture = -1;
  bool m_has_time_of_day_texture = false;

  std::vector<math::Vector<u8, 4>> m_color_result;

  bool m_has_index_buffer = false;
  GLuint m_index_buffer = -1;

  static constexpr int TIME_OF_DAY_COLOR_COUNT = 8192;

  char m_user_level[255] = "vi1";
  std::optional<std::string> m_pending_user_level = std::nullopt;
  bool m_override_level = false;
  bool m_use_fast_time_of_day = true;
  bool m_debug_wireframe = false;
  bool m_debug_all_visible = false;
  Filtered<float> m_all_tree_time;

  TfragPcPortData m_pc_port_data;
};
