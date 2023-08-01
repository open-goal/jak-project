#pragma once

#include <array>
#include <memory>

#include "common/dma/dma_chain_read.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/CollideMeshRenderer.h"
#include "game/graphics/opengl_renderer/Fbo.h"
#include "game/graphics/opengl_renderer/Profiler.h"
#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/TextureAnimator.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"
#include "game/graphics/opengl_renderer/foreground/Merc2.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/tools/filter_menu/filter_menu.h"
#include "game/tools/subtitle_editor/subtitle_editor.h"

struct RenderOptions {
  bool draw_render_debug_window = false;
  bool draw_profiler_window = false;
  bool draw_loader_window = false;
  bool draw_small_profiler_window = false;
  bool draw_subtitle_editor_window = false;
  bool draw_filters_window = false;

  // internal rendering settings - The OpenGLRenderer will internally use this resolution/format.
  int msaa_samples = 2;
  int game_res_w = 640;
  int game_res_h = 480;

  // size of the window's framebuffer (framebuffer 0)
  // The renderer needs to know this to do an optimization to render directly to the window's
  // framebuffer when possible.
  int window_framebuffer_height = 0;
  int window_framebuffer_width = 0;

  // the part of the window that we should draw to. The rest is black. This value is determined by
  // logic inside of the game - it needs to know the desired aspect ratio.
  int draw_region_height = 0;
  int draw_region_width = 0;

  bool save_screenshot = false;
  bool quick_screenshot = false;
  bool internal_res_screenshot = false;
  std::string screenshot_path;

  float pmode_alp_register = 0.f;

  // when enabled, does a `glFinish()` after each major rendering pass. This blocks until the GPU
  // is done working, making it easier to profile GPU utilization.
  bool gpu_sync = false;
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
  OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool,
                 std::shared_ptr<Loader> loader,
                 GameVersion version);

  // rendering interface: takes the dma chain from the game, and some size/debug settings from
  // the graphics system.
  void render(DmaFollower dma, const RenderOptions& settings);

 private:
  void setup_frame(const RenderOptions& settings);
  void dispatch_buckets(DmaFollower dma, ScopedProfilerNode& prof, bool sync_after_buckets);
  void dispatch_buckets_jak1(DmaFollower dma, ScopedProfilerNode& prof, bool sync_after_buckets);
  void dispatch_buckets_jak2(DmaFollower dma, ScopedProfilerNode& prof, bool sync_after_buckets);

  void do_pcrtc_effects(float alp, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void blit_display();
  void init_bucket_renderers_jak1();
  void init_bucket_renderers_jak2();
  void draw_renderer_selection_window();
  void finish_screenshot(const std::string& output_name,
                         int px,
                         int py,
                         int x,
                         int y,
                         GLuint fbo,
                         int read_buffer,
                         bool quick_screenshot);
  template <typename T, typename U, class... Args>
  T* init_bucket_renderer(const std::string& name, BucketCategory cat, U id, Args&&... args) {
    auto renderer = std::make_unique<T>(name, (int)id, std::forward<Args>(args)...);
    T* ret = renderer.get();
    m_bucket_renderers.at((int)id) = std::move(renderer);
    m_bucket_categories.at((int)id) = cat;
    return ret;
  }

  const std::vector<GLuint>* anim_slot_array() {
    return m_texture_animator ? m_texture_animator->slots() : nullptr;
  }

  SharedRenderState m_render_state;
  Profiler m_profiler;
  SmallProfiler m_small_profiler;
  SubtitleEditor* m_subtitle_editor = nullptr;
  FiltersMenu m_filters_menu;

  std::shared_ptr<Merc2> m_merc2;
  std::shared_ptr<Generic2> m_generic2;
  std::shared_ptr<TextureAnimator> m_texture_animator;
  std::vector<std::unique_ptr<BucketRenderer>> m_bucket_renderers;
  std::vector<BucketCategory> m_bucket_categories;
  class BlitDisplays* m_blit_displays = nullptr;

  std::array<float, (int)BucketCategory::MAX_CATEGORIES> m_category_times;
  FullScreenDraw m_blackout_renderer;
  CollideMeshRenderer m_collide_renderer;

  float m_last_pmode_alp = 1.;
  bool m_enable_fast_blackout_loads = true;

  struct FboState {
    struct {
      Fbo window;          // provided by glfw
      Fbo render_buffer;   // temporary buffer to render to
      Fbo resolve_buffer;  // temporary buffer to resolve to
    } resources;

    Fbo* render_fbo = nullptr;  // the selected fbo from the three above to use for rendering
  } m_fbo_state;

  std::unique_ptr<BucketRenderer> m_jak2_eye_renderer;
  GameVersion m_version;
};
