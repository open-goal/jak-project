#pragma once

#include <array>
#include <memory>

#include "game/graphics/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"

class OpenGLRenderer {
 public:
  OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool);
  void render(DmaFollower dma, int window_width_px, int window_height_px);

 private:
  void setup_frame(int window_width_px, int window_height_px);
  void draw_test_triangle();
  void dispatch_buckets(DmaFollower dma);
  void init_bucket_renderers();

  template <typename T, class... Args>
  void init_bucket_renderer(const std::string& name, BucketId id, Args&&... args) {
    m_bucket_renderers.at((int)id) = std::make_unique<T>(name, id, std::forward<Args>(args)...);
  }

  SharedRenderState m_render_state;

  std::array<std::unique_ptr<BucketRenderer>, (int)BucketId::MAX_BUCKETS> m_bucket_renderers;
};
