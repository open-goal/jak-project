#pragma once
#include "common/versions/versions.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"

struct PatColors {
  math::Vector4f pat_mode_colors[0x8];
  math::Vector4f pat_material_colors[0x40];
  math::Vector4f pat_event_colors[0x40];
};

class CollideMeshRenderer {
 public:
  CollideMeshRenderer(GameVersion version);
  void render(SharedRenderState* render_state, ScopedProfilerNode& prof);
  ~CollideMeshRenderer();

 private:
  void init_pat_colors(GameVersion version);

  GLuint m_vao;
  GLuint m_ubo;

  PatColors m_colors;
};
