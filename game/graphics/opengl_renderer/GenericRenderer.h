#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class GenericRenderer: public BucketRenderer {
 public:
  GenericRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
};
