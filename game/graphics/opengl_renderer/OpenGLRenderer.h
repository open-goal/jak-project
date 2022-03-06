#pragma once

#include <array>
#include <memory>

#include "common/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/Profiler.h"

struct RenderOptions {
  int window_height_px = 0;
  int window_width_px = 0;
  int lbox_height_px = 0;
  int lbox_width_px = 0;
  bool draw_render_debug_window = false;
  bool draw_profiler_window = false;
  bool draw_small_profiler_window = false;

  bool save_screenshot = false;
  std::string screenshot_path;
};

class OpenGLRenderer {
 public:
  OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool, std::shared_ptr<Loader> loader);
  void render(DmaFollower dma, const RenderOptions& settings);

 private:
  void setup_frame(int window_width_px, int window_height_px, int offset_x, int offset_y);
  void draw_test_triangle();
  void dispatch_buckets(DmaFollower dma, ScopedProfilerNode& prof);
  void init_bucket_renderers();
  void draw_renderer_selection_window();

  void finish_screenshot(const std::string& output_name, int px, int py, int x, int y);

  template <typename T, class... Args>
  T* init_bucket_renderer(const std::string& name,
                          BucketCategory cat,
                          BucketId id,
                          Args&&... args) {
    auto renderer = std::make_unique<T>(name, id, std::forward<Args>(args)...);
    T* ret = renderer.get();
    m_bucket_renderers.at((int)id) = std::move(renderer);
    m_bucket_categories.at((int)id) = cat;
    return ret;
  }

  SharedRenderState m_render_state;
  Profiler m_profiler;
  SmallProfiler m_small_profiler;

  std::array<std::unique_ptr<BucketRenderer>, (int)BucketId::MAX_BUCKETS> m_bucket_renderers;
  std::array<BucketCategory, (int)BucketId::MAX_BUCKETS> m_bucket_categories;

  std::array<float, (int)BucketCategory::MAX_CATEGORIES> m_category_times;
};
