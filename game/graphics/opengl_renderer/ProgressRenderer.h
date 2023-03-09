#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

/*!
 * Renderer for the "Progress Bucket" of Jak 2.
 */
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