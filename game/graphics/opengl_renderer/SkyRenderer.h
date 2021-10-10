
#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

/*!
 * Handles texture blending for the sky.
 * Will insert the result texture into the texture pool.
 */
class SkyTextureHandler : public BucketRenderer {
 public:
  SkyTextureHandler(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  ~SkyTextureHandler();

 private:
  void handle_sky_copies(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);
  GLuint m_framebuffers[2];  // sky, clouds
  GLuint m_textures[2];      // sky, clouds
  int m_sizes[2] = {32, 64};
  GLuint m_gl_vertex_buffer;

  struct Vertex {
    float x = 0;
    float y = 0;
    float intensity = 0;
  };

  struct FrameStats {
    int sky_draws = 0;
    int cloud_draws = 0;
    int sky_blends = 0;
    int cloud_blends = 0;
  } m_stats;

  Vertex m_vertex_data[6];
};

/*!
 * Handles sky drawing.
 */
class SkyRenderer : public BucketRenderer {
 public:
  SkyRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  DirectRenderer m_direct_renderer;

  struct FrameStats {
    int gif_packets = 0;
  } m_frame_stats;
};