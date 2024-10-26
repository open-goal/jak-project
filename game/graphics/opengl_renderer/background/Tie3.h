#pragma once

#include <optional>

#include "common/util/FilteredValue.h"

#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/pipelines/opengl.h"

struct TieProtoVisibility {
  void init(const std::vector<std::string>& names);
  void update(const u8* data, size_t size);

  std::vector<u8> vis_flags;
  std::unordered_map<std::string, std::vector<u32>> name_to_idx;

  bool all_visible = true;
};

struct EtieUniforms {
  GLuint persp0, persp1, cam_no_persp, envmap_tod_tint, decal;
};

class Tie3 : public BucketRenderer {
 public:
  // by default, only render the specified category on the call to render.
  // to render the other categories, use the Tie3AnotherCategory renderer below.
  Tie3(const std::string& name,
       int my_id,
       int level_id,
       const std::vector<GLuint>* anim_slot_array,
       tfrag3::TieCategory category = tfrag3::TieCategory::NORMAL);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;
  ~Tie3();

  bool set_up_common_data_from_dma(DmaFollower& dma, SharedRenderState* render_state);

  void setup_all_trees(int geom,
                       const TfragRenderSettings& settings,
                       const u8* proto_vis_data,
                       size_t proto_vis_data_size,
                       bool use_multidraw,
                       ScopedProfilerNode& prof);

  void setup_tree(int idx,
                  int geom,
                  const TfragRenderSettings& settings,
                  const u8* proto_vis_data,
                  size_t proto_vis_data_size,
                  bool use_multidraw,
                  ScopedProfilerNode& prof);

  void draw_matching_draws_for_all_trees(int geom,
                                         const TfragRenderSettings& settings,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& prof,
                                         tfrag3::TieCategory category);

  void draw_matching_draws_for_tree(int idx,
                                    int geom,
                                    const TfragRenderSettings& settings,
                                    SharedRenderState* render_state,
                                    ScopedProfilerNode& prof,
                                    tfrag3::TieCategory category);

  bool try_loading_level(const std::string& str, SharedRenderState* render_state);

  void render_from_another(SharedRenderState* render_state,
                           ScopedProfilerNode& prof,
                           tfrag3::TieCategory category);

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
  void load_from_fr3_data(const LevelData* loader_data);
  void discard_tree_cache();
  void render_tree_wind(int idx,
                        int geom,
                        const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  struct CommonData {
    // data that the AnotherCategory renderers can use.
    TfragRenderSettings settings;
    const u8* proto_vis_data = nullptr;
    u32 proto_vis_data_size = 0;
    math::Vector4f envmap_color = math::Vector4f{2.f, 2.f, 2.f, 2.f};
    u64 frame_idx = -1;
  } m_common_data;

  float m_envmap_strength = 1.f;

  struct Tree {
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint single_draw_index_buffer;
    GLuint time_of_day_texture;
    GLuint vao;
    std::array<u32, tfrag3::kNumTieCategories + 1> category_draw_indices;
    const std::vector<tfrag3::StripDraw>* draws = nullptr;
    const std::vector<tfrag3::InstancedStripDraw>* wind_draws = nullptr;
    const std::vector<tfrag3::TieWindInstance>* instance_info = nullptr;
    const tfrag3::PackedTimeOfDay* colors = nullptr;
    const tfrag3::BVH* vis = nullptr;
    const u32* index_data = nullptr;
    std::vector<std::array<math::Vector4f, 4>> wind_matrix_cache;
    GLuint wind_vertex_index_buffer;
    std::vector<u32> wind_vertex_index_offsets;
    bool has_proto_visibility = false;
    TieProtoVisibility proto_visibility;

    std::vector<std::pair<int, int>> draw_idx_temp;
    std::vector<u32> index_temp;
    std::vector<u8> vis_temp;
    std::vector<std::pair<int, int>> multidraw_offset_per_stripdraw;
    std::vector<GLsizei> multidraw_count_buffer;
    std::vector<void*> multidraw_index_offset_buffer;
    u64 draw_mode = 0;  // strip or not, GL enum
  };

  void envmap_second_pass_draw(const Tree& tree,
                               const TfragRenderSettings& settings,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               tfrag3::TieCategory category);

  std::array<std::vector<Tree>, 4> m_trees;  // includes 4 lods!
  std::string m_level_name;
  const std::vector<GLuint>* m_textures;
  u64 m_load_id = -1;

  std::vector<math::Vector<u8, 4>> m_color_result;

  static constexpr int TIME_OF_DAY_COLOR_COUNT = 8192;

  bool m_has_level = false;
  bool m_use_fast_time_of_day = true;
  bool m_debug_all_visible = false;
  bool m_hide_wind = false;
  bool m_draw_envmap_second_draw = true;

  TfragPcPortData m_pc_port_data;

  std::vector<float> m_wind_vectors;  // note: I suspect these are shared with shrub.

  float m_wind_multiplier = 1.f;

  int m_level_id;

  tfrag3::TieCategory m_default_category;

  struct {
    GLuint decal;
  } m_uniforms;

  EtieUniforms m_etie_uniforms, m_etie_base_uniforms;
  const std::vector<GLuint>* m_anim_slot_array;
  static_assert(sizeof(WindWork) == 84 * 16);
};

class Tie3AnotherCategory : public BucketRenderer {
 public:
  Tie3AnotherCategory(const std::string& name,
                      int my_id,
                      Tie3* parent,
                      tfrag3::TieCategory category);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  Tie3* m_parent;
  tfrag3::TieCategory m_category;
};

/*!
 * Jak 1 - specific renderer that does TIE and TIE envmap in one.
 */
class Tie3WithEnvmapJak1 : public Tie3 {
 public:
  Tie3WithEnvmapJak1(const std::string& name, int my_id, int level_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  bool m_enable_envmap = true;
};