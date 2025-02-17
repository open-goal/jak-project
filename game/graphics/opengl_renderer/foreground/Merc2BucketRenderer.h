#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Merc2.h"

class Merc2BucketRenderer : public BucketRenderer {
 public:
  Merc2BucketRenderer(const std::string& name, int my_id, std::shared_ptr<Merc2> merc);
  void draw_debug_window() override;
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  bool empty() const override;

 private:
  bool m_empty = false;
  std::shared_ptr<Merc2> m_renderer;
  MercDebugStats m_debug_stats;
};
