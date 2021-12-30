
#include "common/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/pipelines/opengl.h"

class SkyBlendGPU {
 public:
  SkyBlendGPU();
  ~SkyBlendGPU();

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