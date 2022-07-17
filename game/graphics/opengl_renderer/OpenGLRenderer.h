#pragma once

#include <array>
#include <memory>

#include "common/dma/dma_chain_read.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/CollideMeshRenderer.h"
#include "game/graphics/opengl_renderer/Profiler.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/tools/subtitles/subtitle_editor.h"

struct RenderOptions {
  int window_height_px = 0;
  int window_width_px = 0;
  int lbox_height_px = 0;
  int lbox_width_px = 0;
  bool draw_render_debug_window = false;
  bool draw_profiler_window = false;
  bool draw_small_profiler_window = false;
  bool draw_subtitle_editor_window = false;

  int msaa_samples = 4;
  int game_res_w = 640;
  int game_res_h = 480;

  bool save_screenshot = false;
  std::string screenshot_path;

  float pmode_alp_register = 0.f;
};

/*!
 * Main OpenGL renderer.
 * This handles the glClear and all game rendering, but not actual setup, synchronization or imgui
 * stuff.
 *
 * It is simply a collection of bucket renderers, and a few special case ones.
 */
class OpenGLRenderer {
 public:
  OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool, std::shared_ptr<Loader> loader);

  // rendering interface: takes the dma chain from the game, and some size/debug settings from
  // the graphics system.
  void render(DmaFollower dma, const RenderOptions& settings);

 private:
  void setup_frame(const RenderOptions& settings);
  void dispatch_buckets(DmaFollower dma, ScopedProfilerNode& prof);
  void do_pcrtc_effects(float alp, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void init_bucket_renderers();
  void draw_renderer_selection_window();
  void finish_screenshot(const std::string& output_name, int px, int py, int x, int y, GLuint fbo);
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
  SubtitleEditor m_subtitle_editor;

  std::array<std::unique_ptr<BucketRenderer>, (int)BucketId::MAX_BUCKETS> m_bucket_renderers;
  std::array<BucketCategory, (int)BucketId::MAX_BUCKETS> m_bucket_categories;

  std::array<float, (int)BucketCategory::MAX_CATEGORIES> m_category_times;
  FullScreenDraw m_blackout_renderer;
  CollideMeshRenderer m_collide_renderer;

  float m_last_pmode_alp = 1.;
  bool m_enable_fast_blackout_loads = true;
};
