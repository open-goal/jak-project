#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class MercRenderer : public BucketRenderer {
 public:
  MercRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  std::string m_debug_string;
};
