#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/TextureAnimator.h"
#include "game/graphics/texture/TexturePool.h"

/*!
 * The TextureUploadHandler receives textures uploads in the DMA chain and updates the TexturePool.
 * The actual textures are preconverted and provided by the loader, so this just updates tables that
 * tell the renderers which OpenGL texture goes with PS2 VRAM addresses.
 */
class TextureUploadHandler : public BucketRenderer {
 public:
  TextureUploadHandler(const std::string& name,
                       int my_id,
                       std::shared_ptr<TextureAnimator> texture_animator,
                       bool add_direct = false);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  bool empty() const override;

 private:
  struct TextureUpload {
    u64 page;
    s64 mode;
  };
  void flush_uploads(std::vector<TextureUpload>& uploads, SharedRenderState* render_state);
  bool m_fake_uploads = false;
  int m_upload_count = 0;
  std::shared_ptr<TextureAnimator> m_texture_animator;
  std::unique_ptr<DirectRenderer> m_direct;
};
