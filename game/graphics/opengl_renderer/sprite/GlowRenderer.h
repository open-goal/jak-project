#pragma once

#include "game/graphics/opengl_renderer/sprite/sprite_common.h"

class GlowRenderer {
 public:
  void add_sprite(const SpriteGlowOutput& data);
  void flush(SharedRenderState* render_state, ScopedProfilerNode& prof);
};
