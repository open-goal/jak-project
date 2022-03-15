#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/ocean/OceanTexture.h"

class OceanNear : public BucketRenderer {
 public:
  OceanNear(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_textures(TexturePool& pool) override;

 private:
  OceanTexture m_texture_renderer;
};
