
#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

class SkyBlender {
 public:
  SkyBlender();
  ~SkyBlender();

  struct Stats {
    int sky_draws = 0;
    int cloud_draws = 0;
    int sky_blends = 0;
    int cloud_blends = 0;
  };

  Stats do_sky_blends(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  GLuint m_framebuffers[2];  // sky, clouds
  GLuint m_textures[2];      // sky, clouds
  int m_sizes[2] = {32, 64};
  GLuint m_gl_vertex_buffer;

  struct Vertex {
    float x = 0;
    float y = 0;
    float intensity = 0;
  };

  Vertex m_vertex_data[6];
};

/*!
 * Handles texture blending for the sky.
 * Will insert the result texture into the texture pool.
 */
class SkyBlendHandler : public BucketRenderer {
 public:
  SkyBlendHandler(const std::string& name,
                  BucketId my_id,
                  std::shared_ptr<SkyBlender> shared_blender);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  void handle_sky_copies(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

  std::shared_ptr<SkyBlender> m_shared_blender;
  SkyBlender::Stats m_stats;
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