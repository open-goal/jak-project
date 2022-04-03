/*!
 * @file opengl.cpp
 * Lower-level OpenGL implementation.
 */

#include <memory>
#include <mutex>
#include <condition_variable>

#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_impl_glfw.h"
#include "third-party/imgui/imgui_impl_opengl3.h"

#include "opengl.h"

#include "game/graphics/gfx.h"
#include "game/graphics/display.h"
#include "game/graphics/opengl_renderer/OpenGLRenderer.h"
#include "game/graphics/texture/TexturePool.h"
#include "common/dma/dma_copy.h"
#include "game/system/newpad.h"
#include "common/log/log.h"
#include "common/goal_constants.h"
#include "common/util/image_loading.h"
#include "game/runtime.h"
#include "common/util/Timer.h"
#include "game/graphics/opengl_renderer/debug_gui.h"
#include "common/util/FileUtil.h"
#include "common/util/compress.h"
#include "common/util/FrameLimiter.h"

namespace {

constexpr bool run_dma_copy = false;

struct GraphicsData {
  // vsync
  std::mutex sync_mutex;
  std::condition_variable sync_cv;
  bool vsync_enabled = true;

  // dma chain transfer
  std::mutex dma_mutex;
  std::condition_variable dma_cv;
  u64 frame_idx = 0;
  u64 frame_idx_of_input_data = 0;
  bool has_data_to_render = false;
  FixedChunkDmaCopier dma_copier;

  // texture pool
  std::shared_ptr<TexturePool> texture_pool;

  std::shared_ptr<Loader> loader;

  // temporary opengl renderer
  OpenGLRenderer ogl_renderer;

  OpenGlDebugGui debug_gui;

  FrameLimiter frame_limiter;
  Timer engine_timer;
  double last_engine_time = 1. / 60.;
  float pmode_alp = 0.f;

  GraphicsData()
      : dma_copier(EE_MAIN_MEM_SIZE),
        texture_pool(std::make_shared<TexturePool>()),
        loader(std::make_shared<Loader>()),
        ogl_renderer(texture_pool, loader) {}
};

std::unique_ptr<GraphicsData> g_gfx_data;

void SetDisplayCallbacks(GLFWwindow* d) {
  glfwSetKeyCallback(
      d, [](GLFWwindow* /*window*/, int key, int /*scancode*/, int action, int /*mods*/) {
        if (action == GlfwKeyAction::Press) {
          // lg::debug("KEY PRESS:   key: {} scancode: {} mods: {:X}", key, scancode, mods);
          Pad::OnKeyPress(key);
        } else if (action == GlfwKeyAction::Release) {
          // lg::debug("KEY RELEASE: key: {} scancode: {} mods: {:X}", key, scancode, mods);
          Pad::OnKeyRelease(key);
        }
      });
}

void ErrorCallback(int err, const char* msg) {
  lg::error("GLFW ERR {}: {}", err, std::string(msg));
}

bool HasError() {
  const char* ptr;
  if (glfwGetError(&ptr) != GLFW_NO_ERROR) {
    lg::error("glfw error: {}", ptr);
    return true;
  } else {
    return false;
  }
}

void FocusCallback(GLFWwindow* window, int focused) {
  glfwSetWindowAttrib(window, GLFW_FLOATING, focused);
}

}  // namespace

static bool gl_inited = false;
static int gl_init(GfxSettings& settings) {
  if (glfwSetErrorCallback(ErrorCallback) != NULL) {
    lg::warn("glfwSetErrorCallback has been re-set!");
  }

  if (glfwInit() == GLFW_FALSE) {
    lg::error("glfwInit error");
    return 1;
  }

  // request an OpenGL 4.3 Core context
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);  // 4.3
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);  // core profile, not compat
  // debug check
  if (settings.debug) {
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE);
  } else {
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_FALSE);
  }
  glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);
  glfwWindowHint(GLFW_SAMPLES, 4);

  return 0;
}

static void gl_exit() {
  g_gfx_data.reset();
  glfwTerminate();
  glfwSetErrorCallback(NULL);
  gl_inited = false;
}

static std::shared_ptr<GfxDisplay> gl_make_main_display(int width,
                                                        int height,
                                                        const char* title,
                                                        GfxSettings& settings) {
  GLFWwindow* window = glfwCreateWindow(width, height, title, NULL, NULL);

  if (!window) {
    lg::error("gl_make_main_display failed - Could not create display window");
    return NULL;
  }

  glfwMakeContextCurrent(window);
  gladLoadGLLoader((GLADloadproc)glfwGetProcAddress);
  if (!gl_inited && !gladLoadGL()) {
    lg::error("GL init fail");
    return NULL;
  }

  std::string image_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "appicon.png").string();

  GLFWimage images[1];
  images[0].pixels =
      stbi_load(image_path.c_str(), &images[0].width, &images[0].height, 0, 4);  // rgba channels
  glfwSetWindowIcon(window, 1, images);
  stbi_image_free(images[0].pixels);
  g_gfx_data = std::make_unique<GraphicsData>();
  gl_inited = true;

  // enable vsync by default
  // glfwSwapInterval(1);
  glfwSwapInterval(settings.vsync);

  SetDisplayCallbacks(window);
  Pad::initialize();

  if (HasError()) {
    lg::error("gl_make_main_display error");
    return NULL;
  }

  std::shared_ptr<GfxDisplay> display = std::make_shared<GfxDisplay>(window);
  // lg::debug("init display #x{:x}", (uintptr_t)display);

  // setup imgui

  // check that version of the library is okay
  IMGUI_CHECKVERSION();

  // this does initialization for stuff like the font data
  ImGui::CreateContext();

  // set up to get inputs for this window
  ImGui_ImplGlfw_InitForOpenGL(window, true);

  // NOTE: imgui's setup calls functions that may fail intentionally, and attempts to disable error
  // reporting so these errors are invisible. But it does not work, and some weird X11 default
  // cursor error is set here that we clear.
  glfwGetError(NULL);

  // set up the renderer
  ImGui_ImplOpenGL3_Init("#version 130");

  return display;
}

static void gl_kill_display(GfxDisplay* display) {
  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplGlfw_Shutdown();
  ImGui::DestroyContext();
  glfwDestroyWindow(display->window_glfw);
}

namespace {
std::string make_output_file_name(const std::string& file_name) {
  file_util::create_dir_if_needed(file_util::get_file_path({"gfx_dumps"}));
  return file_util::get_file_path({"gfx_dumps", file_name});
}
}  // namespace

void render_game_frame(int width, int height, int lbox_width, int lbox_height) {
  // wait for a copied chain.
  bool got_chain = false;
  {
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    // note: there's a timeout here. If the engine is messed up and not sending us frames,
    // we still want to run the glfw loop.
    got_chain = g_gfx_data->dma_cv.wait_for(lock, std::chrono::milliseconds(50),
                                            [=] { return g_gfx_data->has_data_to_render; });
  }
  // render that chain.
  if (got_chain) {
    g_gfx_data->frame_idx_of_input_data = g_gfx_data->frame_idx;
    RenderOptions options;
    options.window_height_px = height;
    options.window_width_px = width;
    options.lbox_height_px = lbox_height;
    options.lbox_width_px = lbox_width;
    options.draw_render_debug_window = g_gfx_data->debug_gui.should_draw_render_debug();
    options.draw_profiler_window = g_gfx_data->debug_gui.should_draw_profiler();
    options.save_screenshot = g_gfx_data->debug_gui.get_screenshot_flag();
    options.draw_small_profiler_window = g_gfx_data->debug_gui.small_profiler;
    options.pmode_alp_register = g_gfx_data->pmode_alp;
    if (options.save_screenshot) {
      options.screenshot_path = make_output_file_name(g_gfx_data->debug_gui.screenshot_name());
    }
    if constexpr (run_dma_copy) {
      auto& chain = g_gfx_data->dma_copier.get_last_result();
      g_gfx_data->ogl_renderer.render(DmaFollower(chain.data.data(), chain.start_offset), options);
    } else {
      g_gfx_data->ogl_renderer.render(DmaFollower(g_gfx_data->dma_copier.get_last_input_data(),
                                                  g_gfx_data->dma_copier.get_last_input_offset()),
                                      options);
    }
  }

  // before vsync, mark the chain as rendered.
  {
    // should be fine to remove this mutex if the game actually waits for vsync to call
    // send_chain again. but let's be safe for now.
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    g_gfx_data->engine_timer.start();
    g_gfx_data->has_data_to_render = false;
    g_gfx_data->sync_cv.notify_all();
  }
}

static void gl_display_position(GfxDisplay* display, int* x, int* y) {
  glfwGetWindowPos(display->window_glfw, x, y);
}

static void gl_display_size(GfxDisplay* display, int* width, int* height) {
  glfwGetFramebufferSize(display->window_glfw, width, height);
}

static void gl_display_set_size(GfxDisplay* display, int width, int height) {
  glfwSetWindowSize(display->window_glfw, width, height);
}

static void gl_display_scale(GfxDisplay* display, float* xs, float* ys) {
  glfwGetWindowContentScale(display->window_glfw, xs, ys);
}

static void gl_set_fullscreen(GfxDisplay* display, int mode, int /*screen*/) {
  GLFWmonitor* monitor = glfwGetPrimaryMonitor();  // todo
  auto window = display->window_glfw;
  switch (mode) {
    case 0: {
      // windowed
      glfwSetWindowAttrib(window, GLFW_DECORATED, GLFW_TRUE);
      glfwSetWindowFocusCallback(window, NULL);
      glfwSetWindowAttrib(window, GLFW_FLOATING, GLFW_FALSE);
      glfwSetWindowMonitor(window, NULL, display->xpos_backup(), display->ypos_backup(),
                           display->width_backup(), display->height_backup(), GLFW_DONT_CARE);
    } break;
    case 1: {
      // fullscreen
      if (display->fullscreen_mode() == 0) {
        display->backup_params();
      }
      const GLFWvidmode* vmode = glfwGetVideoMode(monitor);
      glfwSetWindowMonitor(window, monitor, 0, 0, vmode->width, vmode->height, 60);
      glfwSetWindowFocusCallback(window, FocusCallback);
    } break;
    case 2: {
      // borderless fullscreen
      if (display->fullscreen_mode() == 0) {
        display->backup_params();
      }
      int x, y;
      glfwGetMonitorPos(monitor, &x, &y);
      const GLFWvidmode* vmode = glfwGetVideoMode(monitor);
      glfwSetWindowAttrib(window, GLFW_DECORATED, GLFW_FALSE);
      glfwSetWindowAttrib(window, GLFW_FLOATING, GLFW_TRUE);
      glfwSetWindowFocusCallback(window, FocusCallback);
#ifdef _WIN32
      glfwSetWindowMonitor(window, NULL, x, y, vmode->width, vmode->height + 1, GLFW_DONT_CARE);
#else
      glfwSetWindowMonitor(window, NULL, x, y, vmode->width, vmode->height, GLFW_DONT_CARE);
#endif
    } break;
  }
}

static void gl_render_display(GfxDisplay* display) {
  GLFWwindow* window = display->window_glfw;

  // poll events
  glfwPollEvents();
  glfwMakeContextCurrent(window);
  Pad::update_gamepads();

  // imgui start of frame
  ImGui_ImplOpenGL3_NewFrame();
  ImGui_ImplGlfw_NewFrame();
  ImGui::NewFrame();

  // window size
  int width = Gfx::g_global_settings.lbox_w;
  int height = Gfx::g_global_settings.lbox_h;
  int fbuf_w, fbuf_h;
  glfwGetFramebufferSize(window, &fbuf_w, &fbuf_h);
#ifdef _WIN32
  if (display->fullscreen_mode() == 2) {
    // pretend the framebuffer is 1 pixel shorter on borderless. fullscreen issues!
    fbuf_h--;
  }
#endif
  // horizontal letterbox size
  int lbox_w = (fbuf_w - width) / 2;
  // vertical letterbox size
  int lbox_h = (fbuf_h - height) / 2;
#ifdef _WIN32
  if (display->fullscreen_mode() == 2) {
    // add one pixel of vertical letterbox on borderless to make up for extra line
    lbox_h++;
  }
#endif

  // render game!
  if (g_gfx_data->debug_gui.should_advance_frame()) {
    render_game_frame(width, height, lbox_w, lbox_h);
  }

  if (g_gfx_data->debug_gui.should_gl_finish()) {
    glFinish();
  }

  // render imgui
  g_gfx_data->debug_gui.draw(g_gfx_data->dma_copier.get_last_result().stats);
  ImGui::Render();
  ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

  // switch vsync modes, if requested
  bool req_vsync = g_gfx_data->debug_gui.get_vsync_flag();
  if (req_vsync != g_gfx_data->vsync_enabled) {
    g_gfx_data->vsync_enabled = req_vsync;
    glfwSwapInterval(req_vsync);
  }

  // actual vsync
  g_gfx_data->debug_gui.finish_frame();
  glfwSwapBuffers(window);
  if (g_gfx_data->debug_gui.framelimiter) {
    g_gfx_data->frame_limiter.run(
        g_gfx_data->debug_gui.target_fps, g_gfx_data->debug_gui.experimental_accurate_lag,
        g_gfx_data->debug_gui.sleep_in_frame_limiter, g_gfx_data->last_engine_time);
  }
  g_gfx_data->debug_gui.start_frame();

  if (display->fullscreen_pending()) {
    display->fullscreen_flush();
  }

  // toggle even odd and wake up engine waiting on vsync.
  {
    std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
    g_gfx_data->frame_idx++;
    g_gfx_data->sync_cv.notify_all();
  }

  // exit if display window was closed
  if (glfwWindowShouldClose(window)) {
    std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
    MasterExit = 2;
    g_gfx_data->sync_cv.notify_all();
  }
}

/*!
 * Wait for the next vsync. Returns 0 or 1 depending on if frame is even or odd.
 * Called from the game thread, on a GOAL stack.
 */
u32 gl_vsync() {
  if (!g_gfx_data) {
    return 0;
  }
  std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
  auto init_frame = g_gfx_data->frame_idx_of_input_data;
  g_gfx_data->sync_cv.wait(lock, [=] { return MasterExit || g_gfx_data->frame_idx > init_frame; });
  return g_gfx_data->frame_idx & 1;
}

u32 gl_sync_path() {
  if (!g_gfx_data) {
    return 0;
  }
  std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
  g_gfx_data->last_engine_time = g_gfx_data->engine_timer.getSeconds();
  if (!g_gfx_data->has_data_to_render) {
    return 0;
  }
  g_gfx_data->sync_cv.wait(lock, [=] { return !g_gfx_data->has_data_to_render; });
  return 0;
}

/*!
 * Send DMA to the renderer.
 * Called from the game thread, on a GOAL stack.
 */
void gl_send_chain(const void* data, u32 offset) {
  if (g_gfx_data) {
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    if (g_gfx_data->has_data_to_render) {
      lg::error(
          "Gfx::send_chain called when the graphics renderer has pending data. Was this called "
          "multiple times per frame?");
      return;
    }

    // we copy the dma data and give a copy of it to the render.
    // the copy has a few advantages:
    // - if the game code has a bug and corrupts the DMA buffer, the renderer won't see it.
    // - the copied DMA is much smaller than the entire game memory, so it can be dumped to a file
    //    separate of the entire RAM.
    // - it verifies the DMA data is valid early on.
    // but it may also be pretty expensive. Both the renderer and the game wait on this to complete.

    // The renderers should just operate on DMA chains, so eliminating this step in the future may
    // be easy.

    g_gfx_data->dma_copier.set_input_data(data, offset, run_dma_copy);

    g_gfx_data->has_data_to_render = true;
    g_gfx_data->dma_cv.notify_all();
  }
}

void gl_texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  // block
  if (g_gfx_data) {
    // just pass it to the texture pool.
    // the texture pool will take care of locking.
    // we don't want to lock here for the entire duration of the conversion.
    g_gfx_data->texture_pool->handle_upload_now(tpage, mode, g_ee_main_mem, s7_ptr);
  }
}

void gl_texture_relocate(u32 destination, u32 source, u32 format) {
  if (g_gfx_data) {
    g_gfx_data->texture_pool->relocate(destination, source, format);
  }
}

void gl_poll_events() {
  glfwPollEvents();
}

void gl_set_levels(const std::vector<std::string>& levels) {
  g_gfx_data->loader->set_want_levels(levels);
}

void gl_set_pmode_alp(float val) {
  g_gfx_data->pmode_alp = val;
}

const GfxRendererModule moduleOpenGL = {
    gl_init,                // init
    gl_make_main_display,   // make_main_display
    gl_kill_display,        // kill_display
    gl_render_display,      // render_display
    gl_display_position,    // display_position
    gl_display_size,        // display_size
    gl_display_set_size,    // display_set_size
    gl_display_scale,       // display_scale
    gl_set_fullscreen,      // set_fullscreen
    gl_exit,                // exit
    gl_vsync,               // vsync
    gl_sync_path,           // sync_path
    gl_send_chain,          // send_chain
    gl_texture_upload_now,  // texture_upload_now
    gl_texture_relocate,    // texture_relocate
    gl_poll_events,         // poll_events
    gl_set_levels,          // set_levels
    gl_set_pmode_alp,       // set_pmode_alp
    GfxPipeline::OpenGL,    // pipeline
    "OpenGL 4.3"            // name
};
