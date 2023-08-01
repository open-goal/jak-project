#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/Warp.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

class Warp : public BucketRenderer {
 public:
  Warp(const std::string& name, int id, std::shared_ptr<Generic2> generic);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_textures(TexturePool& tex_pool, GameVersion version) override;

 private:
  std::shared_ptr<Generic2> m_generic;
  FramebufferCopier m_fb_copier;
  GpuTexture* m_warp_src_tex = nullptr;
  u32 m_tbp = 1216;  // hack, jak 2
};
