/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Handles some low-level routines.
 */

#include "gfx.h"
#include <functional>
#include <memory>
#include <mutex>
#include <condition_variable>
#include "common/log/log.h"
#include "game/runtime.h"
#include "display.h"

#include "opengl.h"

namespace Gfx {

struct GraphicsData {
  std::mutex sync_mutex;
  std::condition_variable sync_cv;
  u64 frame_idx = 0;
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
      // lg::debug("run display");
      glfwMakeContextCurrent(Display::display);

      // render graphics
      glClear(GL_COLOR_BUFFER_BIT);

      glfwSwapBuffers(Display::display);

      // toggle even odd and wake up anybody waiting on vsync.
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

}  // namespace Gfx
