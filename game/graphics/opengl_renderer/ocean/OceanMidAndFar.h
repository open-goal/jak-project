#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/ocean/OceanMid.h"
#include "game/graphics/opengl_renderer/ocean/OceanTexture.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

/*!
 * OceanMidAndFar is the handler for the first ocean bucket.
 * This bucket runs three renderers:
 * - ocean-texture (handled by the OceanTexture C++ class)
 * - ocean-far (handled by this class, it's very simple)
 * - ocean-mid (handled by the C++ OceanMid class)
 */
class OceanMidAndFar : public BucketRenderer {
 public:
  OceanMidAndFar(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void render_jak1(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void render_jak2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window() override;
  void init_textures(TexturePool& pool, GameVersion version) override;

 private:
  void handle_ocean_far(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void handle_ocean_mid(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);
  void handle_ocean_89_jak2(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof);
  void handle_ocean_79_jak2(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof);

  DirectRenderer m_direct;
  OceanTexture m_texture_renderer;
  OceanMid m_mid_renderer;
};
