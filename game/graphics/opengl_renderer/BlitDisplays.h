#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/texture/TexturePool.h"

struct PcZoomBlur {
  math::Vector4f pos;
  math::Vector4<int32_t> color;
  uint32_t is_2d;
  int32_t texels;
  float alpha_current;
};

/*!
 * The BlitDisplays renderer does various blitting and effects on the previous frame
 */
class BlitDisplays : public BucketRenderer {
 public:
  BlitDisplays(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void init_textures(TexturePool& texture_pool, GameVersion) override;
  void draw_debug_window() override;
  void do_copy_back(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void do_zoom_blur(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void apply_color_filter(SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  std::unique_ptr<FramebufferCopier> m_copier;
  FramebufferCopier m_blur_old_copier;
  FramebufferCopier m_blur_new_copier;
  GpuTexture* m_gpu_tex;
  u32 m_tbp;
  bool m_copy_back_pending = false;
  bool m_zoom_blur_pending = false;
  bool m_color_filter_pending = false;
  math::Vector4f m_color_filter;
  PcZoomBlur m_zoom_blur;
  FullScreenTexDraw m_fullscreen_tex_draw;
  FullScreenDraw m_color_draw;
};
