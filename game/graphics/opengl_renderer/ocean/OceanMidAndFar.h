#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/opengl_renderer/ocean/OceanTexture.h"

/*!
 * OceanMidAndFar is the handler for the first ocean bucket.
 *
 */
class OceanMidAndFar : public BucketRenderer {
 public:
  OceanMidAndFar(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_textures(TexturePool& pool) override;

 private:
  void handle_ocean_far(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  DirectRenderer m_direct;
  OceanTexture m_texture_renderer;
};
