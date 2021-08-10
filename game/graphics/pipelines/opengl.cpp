/*!
 * @file opengl.cpp
 * Lower-level OpenGL implementation.
 */

#include <memory>
#include <mutex>
#include <condition_variable>

#include "opengl.h"

#include "game/graphics/gfx.h"
#include "game/graphics/display.h"
#include "game/graphics/opengl_renderer/OpenGLRenderer.h"
#include "game/graphics/texture/TexturePool.h"
#include "game/graphics/dma/dma_copy.h"
#include "common/log/log.h"
#include "common/goal_constants.h"
#include "game/runtime.h"

namespace {

struct GraphicsData {
  // vsync
  std::mutex sync_mutex;
  std::condition_variable sync_cv;

  // dma chain transfer
  std::mutex dma_mutex;
  std::condition_variable dma_cv;
  u64 frame_idx = 0;
  bool has_data_to_render = false;
  FixedChunkDmaCopier dma_copier;

  // texture pool
  std::shared_ptr<TexturePool> texture_pool;

  // temporary opengl renderer
  OpenGLRenderer ogl_renderer;

  GraphicsData()
      : dma_copier(EE_MAIN_MEM_SIZE),
        texture_pool(std::make_shared<TexturePool>()),
        ogl_renderer(texture_pool) {}
};

std::unique_ptr<GraphicsData> g_gfx_data;

void SetDisplayCallbacks(GLFWwindow* d) {
  glfwSetKeyCallback(d, [](GLFWwindow* /*window*/, int key, int scancode, int action, int mods) {
    if (action == GlfwKeyAction::Press) {
      lg::debug("KEY PRESS:   key: {} scancode: {} mods: {:X}", key, scancode, mods);
    } else if (action == GlfwKeyAction::Release) {
      lg::debug("KEY RELEASE: key: {} scancode: {} mods: {:X}", key, scancode, mods);
    }
  });
}

void ErrorCallback(int err, const char* msg) {
  lg::error("GLFW ERR {}: " + std::string(msg), err);
}

bool HasError() {
  return glfwGetError(NULL) != GLFW_NO_ERROR;
}

}  // namespace

static bool gl_inited = false;
static int gl_init() {
  if (glfwSetErrorCallback(ErrorCallback) != NULL) {
    lg::warn("glfwSetErrorCallback has been re-set!");
  }

  if (glfwInit() == GLFW_FALSE) {
    lg::error("glfwInit error");
    return 1;
  }

  // request OpenGL 3.0 (placeholder)
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

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

  if (!gl_inited && !gladLoadGL()) {
    lg::error("GL init fail");
    return NULL;
  }
  g_gfx_data = std::make_unique<GraphicsData>();
  gl_inited = true;

  // enable vsync by default
  // glfwSwapInterval(1);
  glfwSwapInterval(settings.vsync);

  SetDisplayCallbacks(window);

  if (HasError()) {
    lg::error("gl_make_main_display error");
    return NULL;
  }

  std::shared_ptr<GfxDisplay> display = std::make_shared<GfxDisplay>(window);
  // lg::debug("init display #x{:x}", (uintptr_t)display);

  return display;
}

static void gl_kill_display(GfxDisplay* display) {
  glfwDestroyWindow(display->window_glfw);
}

static void gl_render_display(GfxDisplay* display) {
  GLFWwindow* window = display->window_glfw;

  glfwMakeContextCurrent(window);

  // wait for a copied chain.
  bool got_chain = false;
  {
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    // note: there's a timeout here. If the engine is messed up and not sending us frames,
    // we still want to run the glfw loop.
    got_chain = g_gfx_data->dma_cv.wait_for(lock, std::chrono::milliseconds(20),
                                            [=] { return g_gfx_data->has_data_to_render; });
  }

  // render that chain.
  if (got_chain) {
    auto& chain = g_gfx_data->dma_copier.get_last_result();
    int width, height;
    glfwGetFramebufferSize(window, &width, &height);
    g_gfx_data->ogl_renderer.render(DmaFollower(chain.data.data(), chain.start_offset), width,
                                    height);
  }

  // before vsync, mark the chain as rendered.
  {
    // should be fine to remove this mutex if the game actually waits for vsync to call
    // send_chain again. but let's be safe for now.
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    g_gfx_data->has_data_to_render = false;
  }

  // actual vsync
  glfwSwapBuffers(window);

  // toggle even odd and wake up engine waiting on vsync.
  {
    std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
    g_gfx_data->frame_idx++;
    g_gfx_data->sync_cv.notify_all();
  }
  // poll events TODO integrate input with cpad
  glfwPollEvents();

  // exit if display window was closed
  if (glfwWindowShouldClose(window)) {
    // Display::KillDisplay(window);
    MasterExit = 2;
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
  auto init_frame = g_gfx_data->frame_idx;
  g_gfx_data->sync_cv.wait(lock, [=] { return g_gfx_data->frame_idx > init_frame; });

  return g_gfx_data->frame_idx & 1;
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

    // Timer copy_timer;
    g_gfx_data->dma_copier.run(data, offset);
    // fmt::print("copy took {:.3f}ms\n", copy_timer.getMs());

    g_gfx_data->has_data_to_render = true;
    g_gfx_data->dma_cv.notify_all();
  }
}

void gl_texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  if (g_gfx_data) {
    // just pass it to the texture pool.
    // the texture pool will take care of locking.
    // we don't want to lock here for the entire duration of the conversion.
    g_gfx_data->texture_pool->handle_upload_now(tpage, mode, g_ee_main_mem, s7_ptr);
  }
}

void gl_texture_relocate(u32 destination, u32 source) {
  if (g_gfx_data) {
    g_gfx_data->texture_pool->relocate(destination, source);
  }
}

const GfxRendererModule moduleOpenGL = {
    gl_init,                // init
    gl_make_main_display,   // make_main_display
    gl_kill_display,        // kill_display
    gl_render_display,      // render_display
    gl_exit,                // exit
    gl_vsync,               // vsync
    gl_send_chain,          // send_chain
    gl_texture_upload_now,  // texture_upload_now
    gl_texture_relocate,    // texture_relocate
    GfxPipeline::OpenGL,    // pipeline
    "OpenGL 3.0"            // name
};
