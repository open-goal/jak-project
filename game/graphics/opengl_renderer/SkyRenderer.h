
#pragma once
#include "game/graphics//opengl_renderer/SkyBlendCPU.h"
#include "game/graphics//opengl_renderer/SkyBlendGPU.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/background/TFragment.h"

/*!
 * Handles texture blending for the sky.
 * Will insert the result texture into the texture pool.
 */
class SkyBlendHandler : public BucketRenderer {
 public:
  SkyBlendHandler(const std::string& name,
                  int my_id,
                  int level_id,
                  std::shared_ptr<SkyBlendGPU> shared_gpu_blender,
                  std::shared_ptr<SkyBlendCPU> shared_cpu_blender);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;

 private:
  void handle_sky_copies(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);

  std::shared_ptr<SkyBlendGPU> m_shared_gpu_blender;
  std::shared_ptr<SkyBlendCPU> m_shared_cpu_blender;
  SkyBlendStats m_gpu_stats;
  TFragment m_tfrag_renderer;
};

/*!
 * Handles sky drawing.
 */
class SkyRenderer : public BucketRenderer {
 public:
  SkyRenderer(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  DirectRenderer m_direct_renderer;

  struct FrameStats {
    int gif_packets = 0;
  } m_frame_stats;
};