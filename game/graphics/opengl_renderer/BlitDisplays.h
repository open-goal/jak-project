#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/texture/TexturePool.h"

/*!
 * The BlitDisplays renderer does various blitting and effects on the previous frame
 */
class BlitDisplays : public BucketRenderer {
 public:
  BlitDisplays(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void init_textures(TexturePool& texture_pool, GameVersion) override;
  void draw_debug_window() override;

 private:
  GpuTexture* m_gpu_tex;
  u32 m_tbp;
};
