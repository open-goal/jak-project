#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "pipelines/opengl.h"
#include "gfx.h"
#include <vector>
#include <memory>

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
class GfxDisplay {
  const char* m_title;

  const GfxRendererModule* m_renderer = nullptr;

  // NOT actual size! just backups
  int m_width;
  int m_height;
  // same here
  int m_xpos;
  int m_ypos;

  int m_fullscreen_mode = 0;
  int m_fullscreen_screen;
  int m_fullscreen_target_mode;
  int m_fullscreen_target_screen;

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
  void set_size(int w, int h);
  void get_scale(float* w, float* h);
  const char* title() const { return m_title; }

  bool fullscreen_pending() { return m_fullscreen_mode != m_fullscreen_target_mode; }
  void fullscreen_flush() {
    m_renderer->set_fullscreen(this, m_fullscreen_target_mode, m_fullscreen_target_screen);
    m_fullscreen_mode = m_fullscreen_target_mode;
    m_fullscreen_screen = m_fullscreen_target_screen;
  }
  void set_fullscreen(int mode, int screen) {
    m_fullscreen_target_mode = mode;
    m_fullscreen_target_screen = screen;
  }
  int fullscreen_mode() { return m_fullscreen_mode; }
  int fullscreen_screen() { return m_fullscreen_screen; }
  void backup_params();
  int width_backup() { return m_width; }
  int height_backup() { return m_height; }
  int xpos_backup() { return m_xpos; }
  int ypos_backup() { return m_ypos; }

  int width();
  int height();

  void render_graphics();
};

namespace Display {

// a list of displays. the first one is the "main" display, all others are spectator-like extra
// views.
extern std::vector<std::shared_ptr<GfxDisplay>> g_displays;

int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings);
void KillDisplay(std::shared_ptr<GfxDisplay> display);
void KillMainDisplay();

std::shared_ptr<GfxDisplay> GetMainDisplay();

}  // namespace Display
