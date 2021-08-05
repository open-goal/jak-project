#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "pipelines/opengl.h"
#include "gfx.h"
#include <vector>

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
class GfxDisplay {
  const char* title;

  const GfxRendererModule* renderer = nullptr;

  public:
  GfxDisplay(GLFWwindow* a_window);  // OpenGL window constructor
  ~GfxDisplay();  // destructor - this calls the renderer's function for getting rid of a window,
                  // and we can then get rid of the GfxDisplay itself

  // all kinds of windows for the display
  union {
    void* window_generic_ptr = nullptr;
    GLFWwindow* window_glfw;
  };

  bool is_active() const { return window_generic_ptr != nullptr; }
  void set_renderer(GfxPipeline pipeline);
  void set_window(GLFWwindow* window);
  void set_title(const char* title);
  const char* get_title() const { return title; }

  void render_graphics();
};

namespace Display {

// a list of displays. the first one is the "main" display, all others are spectator-like extra
// views.
extern std::vector<GfxDisplay*> displays;

int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings);
void KillDisplay(GfxDisplay* display);

GfxDisplay* GetMainDisplay();

}  // namespace Display
