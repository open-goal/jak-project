/*!
 * @file opengl.cpp
 * Lower-level OpenGL implementation.
 */

#include "opengl.h"

#include "game/graphics/gfx.h"
#include "game/graphics/display.h"

#include "common/log/log.h"
#include <memory>

namespace {

void SetDisplayCallbacks(GLFWwindow* d) {
  glfwSetKeyCallback(d, [](GLFWwindow* window, int key, int scancode, int action, int mods) {
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
  glfwTerminate();
  glfwSetErrorCallback(NULL);
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

  // render graphics
  glClear(GL_COLOR_BUFFER_BIT);

  glfwSwapBuffers(window);

  // poll events TODO integrate input with cpad
  glfwPollEvents();

  // exit if display window was closed
  if (glfwWindowShouldClose(window)) {
    // Display::KillDisplay(window);
    MasterExit = 2;
  }
}

const GfxRendererModule moduleOpenGL = {
    gl_init,               // init
    gl_make_main_display,  // make_main_display
    gl_kill_display,       // kill_display
    gl_render_display,     // render_display
    gl_exit,               // exit
    GfxPipeline::OpenGL,   // pipeline
    "OpenGL 3.0"           // name
};
