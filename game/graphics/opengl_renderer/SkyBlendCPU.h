#pragma once

#include "common/dma/dma_chain_read.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/SkyBlendCommon.h"
#include "game/graphics/pipelines/opengl.h"

class SkyBlendCPU {
 public:
  SkyBlendCPU();
  ~SkyBlendCPU();

  SkyBlendStats do_sky_blends(DmaFollower& dma,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof);
  void init_textures(TexturePool& tex_pool, GameVersion version);

 private:
  static constexpr int m_sizes[2] = {32, 64};
  std::vector<u8> m_texture_data[2];

  struct TexInfo {
    GLuint gl;
    u32 tbp;
    GpuTexture* tex;
  } m_textures[2];
};
