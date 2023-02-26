#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

/*!
 * Renderer for the "Progress Bucket" of Jak 2.
 */
// class ProgressRenderer : public BucketRenderer {
//  public:
//   ProgressRenderer(const std::string& name, int my_id, int batch_size);
//   ~ProgressRenderer();
//   void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof)
//   override; void draw_debug_window() override;
//
//   void render_vif(u32 vif0,
//                   u32 vif1,
//                   const u8* data,
//                   u32 size,
//                   SharedRenderState* render_state,
//                   ScopedProfilerNode& prof);
//   void render_gif(const u8* data,
//                   u32 size,
//                   SharedRenderState* render_state,
//                   ScopedProfilerNode& prof);
//
//  private:
//   DirectRenderer m_direct;
//   bool m_using_direct = true;
// };

class ProgressRenderer : public DirectRenderer {
 public:
  static constexpr int kMinimapVramAddr = 4032;
  static constexpr int kMinimapWidth = 128;
  static constexpr int kMinimapHeight = 128;
  static constexpr int kScreenFbp = 408;
  static constexpr int kMinimapFbp = 126;
  ProgressRenderer(const std::string& name, int my_id, int batch_size);
  void init_textures(TexturePool& texture_pool, GameVersion) override;
  void handle_frame(u64 val, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void pre_render() override;
  void post_render() override;
 private:
  GpuTexture* m_minimap_gpu_tex = nullptr;
  FramebufferTexturePair m_minimap_fb;
  std::optional<FramebufferTexturePairContext> m_fb_ctxt;
  u32 m_current_fbp = kScreenFbp;
};