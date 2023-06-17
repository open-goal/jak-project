#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"

class Generic2BucketRenderer : public BucketRenderer {
 public:
  Generic2BucketRenderer(const std::string& name,
                         int id,
                         std::shared_ptr<Generic2> renderer,
                         Generic2::Mode mode);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  std::shared_ptr<Generic2> m_generic;
  Generic2::Mode m_mode;
};
