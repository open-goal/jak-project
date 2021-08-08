/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Handles some low-level routines.
 */

#include "gfx.h"
#include <functional>
#include <memory>
#include <mutex>
#include <chrono>
#include <condition_variable>
#include "common/log/log.h"
#include "game/runtime.h"
#include "game/graphics/dma/dma_copy.h"
#include "display.h"
#include "common/goal_constants.h"
#include "common/util/Timer.h"
#include "game/graphics/opengl_renderer/OpenGLRenderer.h"
#include "game/graphics/texture/TexturePool.h"

namespace Gfx {

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

bool g_is_initialized = false;

void GlfwErrorCallback(int err, const char* msg) {
  lg::error("GLFW ERR {}: " + std::string(msg), err);
}

u32 Init() {
  if (glfwSetErrorCallback(GlfwErrorCallback) != NULL) {
    lg::warn("glfwSetErrorCallback has been re-set!");
  }

  if (!glfwInit()) {
    lg::error("glfwInit error");
    return 1;
  }

  if (g_main_thread_id != std::this_thread::get_id()) {
    lg::warn("ran Gfx::Init outside main thread. Init display elsewhere?");
  } else {
    Display::InitDisplay(640, 480, "testy", Display::display);
  }

  g_gfx_data = std::make_unique<GraphicsData>();
  g_is_initialized = true;
  return 0;
}

void Loop(std::function<bool()> f) {
  while (f()) {
    // run display-specific things
    if (Display::display) {
      // pre-render setup
      glfwMakeContextCurrent(Display::display);

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
        glfwGetFramebufferSize(Display::display, &width, &height);
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
      glfwSwapBuffers(Display::display);

      // toggle even odd and wake up engine waiting on vsync.
      {
        std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
        g_gfx_data->frame_idx++;
        g_gfx_data->sync_cv.notify_all();
      }

      // poll events TODO integrate input with cpad
      glfwPollEvents();

      // exit if display window was closed
      if (glfwWindowShouldClose(Display::display)) {
        // Display::KillDisplay(Display::display);
        MasterExit = 2;
      }
    }
  }
}

u32 Exit() {
  g_is_initialized = false;
  g_gfx_data.reset();
  lg::debug("gfx exit");
  Display::KillDisplay(Display::display);
  glfwTerminate();
  glfwSetErrorCallback(NULL);
  return 0;
}

bool is_initialized() {
  return g_is_initialized;
}

/*!
 * Wait for the next vsync. Returns 0 or 1 depending on if frame is even or odd.
 * Called from the game thread, on a GOAL stack.
 */
u32 vsync() {
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
void send_chain(const void* data, u32 offset) {
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

void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  if (g_gfx_data) {
    // just pass it to the texture pool.
    // the texture pool will take care of locking.
    // we don't want to lock here for the entire duration of the conversion.
    g_gfx_data->texture_pool->handle_upload_now(tpage, mode, g_ee_main_mem, s7_ptr);
  }
}

}  // namespace Gfx
