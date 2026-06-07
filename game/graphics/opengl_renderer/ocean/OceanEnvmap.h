#pragma once

#include <optional>

#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

/*!
 * This class generates the ocean envmap texture using the sky + time of day (ocean-method-89).
 */
class OceanEnvmap : public DirectRenderer {
 public:
  // (-> *ocean-envmap-texture-base* vram-block)
  static constexpr int ENVMAP_VRAM_ADDR = 0xf80;
  static constexpr int ENVMAP_WIDTH = 64;
  static constexpr int ENVMAP_HEIGHT = 64;

  OceanEnvmap(const std::string& name, int my_id, int batch_size);
  void init_textures(TexturePool& pool, GameVersion version) override;
  void handle_ocean_envmap_jak2(DmaFollower& dma,
                                SharedRenderState* render_state,
                                ScopedProfilerNode& prof);
  void draw_debug_window() override;

 private:
  FramebufferTexturePair m_first_pass_fb;
  FramebufferTexturePair m_envmap_fb;
  std::optional<FramebufferTexturePairContext> m_fb_ctxt;
  GpuTexture* m_envmap_gpu_tex = nullptr;

  // ocean-method-85
  GLuint m_haze_vao = 0;
  GLuint m_haze_vbo = 0;
  void render_haze(const u8* gif_data, u32 size, SharedRenderState* render_state);

  // ocean-method-83
  GLuint m_radial_vao = 0;
  GLuint m_radial_vbo = 0;
  void render_envmap(SharedRenderState* render_state);
};
