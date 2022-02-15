#pragma once

#include <string>
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"


class EyeRenderer : public BucketRenderer {
 public:
  EyeRenderer(const std::string& name, BucketId id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

  void handle_eye_dma(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  std::string m_debug;
  bool m_dump_to_file = false;
  float m_average_time_ms = 0;
};
