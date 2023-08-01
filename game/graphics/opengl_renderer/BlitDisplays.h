#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
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
  void do_copy_back(SharedRenderState* render_state);

 private:
  std::unique_ptr<FramebufferCopier> m_copier;
  GpuTexture* m_gpu_tex;
  u32 m_tbp;
  bool m_copy_back_pending = false;
};
