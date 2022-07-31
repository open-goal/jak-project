#pragma once

/*!
 * @file opengl.h
 * OpenGL includes.
 */

#define GLFW_INCLUDE_NONE
#include "game/graphics/display.h"
#include "game/graphics/gfx.h"

#include "third-party/glad/include/glad/glad.h"
#include "third-party/glfw/include/GLFW/glfw3.h"

enum GlfwKeyAction {
  Release = GLFW_RELEASE,  // falling edge of key press
  Press = GLFW_PRESS,      // rising edge of key press
  Repeat = GLFW_REPEAT     // repeated input on hold e.g. when typing something
};

class GLDisplay : public GfxDisplay {
 public:
  GLDisplay(GLFWwindow* window, bool is_main);
  virtual ~GLDisplay();

  void* get_window() const { return m_window; }
  void get_position(int* x, int* y);
  void get_size(int* w, int* h);
  void get_scale(float* x, float* y);
  void get_screen_size(int vmode_idx, s32* w, s32* h);
  int get_screen_rate(int vmode_idx);
  int get_screen_vmode_count();
  int get_monitor_count();
  void set_size(int w, int h);
  void update_fullscreen(GfxDisplayMode mode, int screen);
  void render();
  bool minimized();
  bool fullscreen_pending() override;
  void fullscreen_flush() override;
  void set_lock(bool lock);
  void on_key(GLFWwindow* window, int key, int scancode, int action, int mods);
  void on_window_pos(GLFWwindow* window, int xpos, int ypos);
  void on_window_size(GLFWwindow* window, int width, int height);
  void on_iconify(GLFWwindow* window, int iconified);

 private:
  GLFWwindow* m_window;
  bool m_minimized = false;
  GLFWvidmode m_last_video_mode = {0, 0, 0, 0, 0, 0};

  GLFWmonitor* get_monitor(int index);
};

extern const GfxRendererModule gRendererOpenGL;
