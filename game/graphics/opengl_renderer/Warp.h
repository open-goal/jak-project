#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"

class Warp : public BucketRenderer {
 public:
  Warp(const std::string& name, int id);
  ~Warp();
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;

 private:
  Generic2 m_generic;
};
