
#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

class SkyTextureHandler : public BucketRenderer {
 public:
  SkyTextureHandler(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
  ~SkyTextureHandler();
 private:
  void handle_sky_copies(DmaFollower& dma, SharedRenderState* render_state);
  bool m_print_debug_dma = true;
  std::string m_debug_dma_str;
  GLuint m_framebuffers[2]; // sky, clouds
  GLuint m_textures[2]; // sky, clouds
  int m_sizes[2] = {32, 64};
  GLuint m_gl_vertex_buffer;

  struct Vertex {
    float x = 0;
    float y = 0;
    float intensity = 0;
  };

  Vertex m_vertex_data[6];
};


class SkyRenderer : public BucketRenderer {
 public:
  SkyRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
  void render_gif(const u8* data, u32 size, SharedRenderState* render_state);
 private:
  bool m_print_debug_dma = true;
  DirectRenderer m_direct_renderer;
  std::string m_debug_dma_str;
};