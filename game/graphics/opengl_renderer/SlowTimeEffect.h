#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class SlowTimeEffect {
 public:
  SlowTimeEffect();
  ~SlowTimeEffect();
  SlowTimeEffect(const SlowTimeEffect&) = delete;
  SlowTimeEffect& operator=(const SlowTimeEffect&) = delete;
  void draw(float amount, SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  GLuint m_vao;
  GLuint m_vertex_buffer;
};