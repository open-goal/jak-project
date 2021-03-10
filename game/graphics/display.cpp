/*!
 * @file display.cpp
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "display.h"

#include "common/log/log.h"

namespace Display {

GLFWwindow* display = NULL;

void InitDisplay(int width, int height, char* title, GLFWwindow*& d) {
  if (d) {
    lg::warn("InitDisplay has already created a display window");
    return;
  }

  // request OpenGL 3.0 (placeholder)
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);
  d = glfwCreateWindow(width, height, title, NULL, NULL);

  if (!d) {
    lg::error("InitDisplay failed - Could not create display window");
    return;
  }

  glfwMakeContextCurrent(d);
  if (!gladLoadGL()) {
    lg::error("GL init fail");
    KillDisplay(d);
    return;
  }

  // enable vsync by default
  glfwSwapInterval(1);

  lg::debug("init display #x{}", (uintptr_t)d);
}

void KillDisplay(GLFWwindow*& d) {
  lg::debug("kill display #x{}", (uintptr_t)d);
  if (!d) {
    lg::warn("KillDisplay called when no display was available");
    return;
  }

  glfwDestroyWindow(d);
  d = NULL;
}

}  // namespace Display
