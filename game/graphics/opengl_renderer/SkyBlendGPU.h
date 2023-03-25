
#include "common/dma/dma_chain_read.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/SkyBlendCommon.h"
#include "game/graphics/pipelines/opengl.h"

class SkyBlendGPU {
 public:
  SkyBlendGPU();
  ~SkyBlendGPU();
  void init_textures(TexturePool& tex_pool, GameVersion version);
  SkyBlendStats do_sky_blends(DmaFollower& dma,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof);

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

  struct TexInfo {
    GpuTexture* tex;
    u32 tbp;
  } m_tex_info[2];
};