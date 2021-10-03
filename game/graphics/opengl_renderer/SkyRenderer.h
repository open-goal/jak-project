
#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

class SkyTextureHandler : public BucketRenderer {
 public:
  SkyTextureHandler(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
 private:
  bool m_print_debug_dma = true;
  std::string m_debug_dma_str;
};


class SkyRenderer : public BucketRenderer {
 public:
  SkyRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state) override;
  void draw_debug_window() override;
 private:
  bool m_print_debug_dma = true;
  std::string m_debug_dma_str;
};