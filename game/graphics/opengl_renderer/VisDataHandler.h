#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

/*!
 * The VisDataHandler copies visibility data from add-pc-port-background-data for the background
 * renderers.
 */
class VisDataHandler : public BucketRenderer {
 public:
  VisDataHandler(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  struct LevelStats {
    bool has_vis = false;
    int num_visible = 0;
  };
  static constexpr int kMaxLevels = 10;
  LevelStats m_stats[kMaxLevels];
  bool m_count_vis = false;
};
