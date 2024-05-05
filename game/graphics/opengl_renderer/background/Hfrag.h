#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/background/background_common.h"

class Hfrag : public BucketRenderer {
 public:
  Hfrag(const std::string& name, int my_id);

  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary&) override;

  struct HfragLevel {
    bool in_use = false;
    std::string name;
    u64 load_id = 0;
    GLuint vertex_buffer;
    GLuint index_buffer;
    GLuint time_of_day_texture;
    GLuint vao;
    tfrag3::Hfragment* hfrag = nullptr;
    u64 num_colors = 0;
    SwizzledTimeOfDay tod_cache;
    u64 last_used_frame = 0;
    struct {
      int total_corners = 0;
      int corners_in_view = 0;
      int corners_in_view_and_not_occluded = 0;
    } stats;
  };

 private:
  /*!
   * Try to get a HfragLevel for the given level name. May return nullptr.
   */
  HfragLevel* get_hfrag_level(const std::string& name, SharedRenderState* render_state);
  void unload_hfrag_level(HfragLevel* lev);
  void load_hfrag_level(const std::string& load_name, HfragLevel* lev, const LevelData* data);
  void render_hfrag_level(HfragLevel* lev,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof,
                          const TfragPcPortData& pc_data,
                          const u8* occlusion_data);

  static constexpr int kMaxLevels = 2;
  std::array<HfragLevel, kMaxLevels> m_levels;
  static constexpr int TIME_OF_DAY_COLOR_COUNT = 8192;
  std::vector<math::Vector<u8, 4>> m_color_result;
};