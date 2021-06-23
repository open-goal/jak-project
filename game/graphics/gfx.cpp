/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Handles some low-level routines.
 */

#include "gfx.h"
#include <functional>
#include "common/log/log.h"
#include "game/runtime.h"
#include "display.h"

#include "opengl.h"

namespace Gfx {

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

      // poll events TODO integrate input with cpad
      glfwPollEvents();

      // exit if display window was closed
      if (glfwWindowShouldClose(Display::display)) {
        // Display::KillDisplay(Display::display);
        MasterExit = 1;
      }
    }
  }
}

u32 Exit() {
  lg::debug("gfx exit");
  Display::KillDisplay(Display::display);
  glfwTerminate();
  glfwSetErrorCallback(NULL);
  return 0;
}

}  // namespace Gfx
