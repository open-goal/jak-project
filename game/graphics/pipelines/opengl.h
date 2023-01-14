#pragma once

/*!
 * @file opengl.h
 * OpenGL includes.
 */

#define GLFW_INCLUDE_NONE
#include <mutex>

#include "game/graphics/display.h"
#include "game/graphics/gfx.h"

#include "third-party/glad/include/glad/glad.h"
#include "third-party/glfw/include/GLFW/glfw3.h"

enum GlfwKeyAction {
  Release = GLFW_RELEASE,  // falling edge of key press
  Press = GLFW_PRESS,      // rising edge of key press
  Repeat = GLFW_REPEAT     // repeated input on hold e.g. when typing something
};

enum GlfwKeyCustomAxis {
  CURSOR_X_AXIS = GLFW_GAMEPAD_AXIS_LAST + 1,
  CURSOR_Y_AXIS = GLFW_GAMEPAD_AXIS_LAST + 2
};

class GLDisplay : public GfxDisplay {
 public:
  GLDisplay(GLFWwindow* window, bool is_main);
  virtual ~GLDisplay();

  void* get_window() const override { return m_window; }
  void get_position(int* x, int* y) override;
  void get_size(int* w, int* h) override;
  void get_scale(float* x, float* y) override;
  void get_screen_size(int vmode_idx, s32* w, s32* h) override;
  int get_screen_rate(int vmode_idx) override;
  int get_screen_vmode_count() override;
  int get_monitor_count() override;
  std::tuple<double, double> get_mouse_pos() override;
  void set_size(int w, int h) override;
  void update_fullscreen(GfxDisplayMode mode, int screen) override;
  void render() override;
  bool minimized() override;
  bool fullscreen_pending() override;
  void fullscreen_flush() override;
  void set_lock(bool lock) override;
  void on_key(GLFWwindow* window, int key, int scancode, int action, int mods);
  void on_window_pos(GLFWwindow* window, int xpos, int ypos);
  void on_window_size(GLFWwindow* window, int width, int height);
  void on_iconify(GLFWwindow* window, int iconified);
  void on_mouse_key(GLFWwindow* window, int button, int action, int mode);
  void on_cursor_position(GLFWwindow* window, double xposition, double yposition);
  void update_cursor_visibility(GLFWwindow* window, bool is_visible);

 private:
  void update_glfw();

  GLFWwindow* m_window;
  bool m_minimized = false;
  GLFWvidmode m_last_video_mode = {0, 0, 0, 0, 0, 0};

  bool is_cursor_position_valid = false;
  double last_cursor_x_position = 0;
  double last_cursor_y_position = 0;

  static constexpr int MAX_VMODES = 256;

  struct VMode {
    void set(const GLFWvidmode* vmode);
    int width = 640, height = 480;
    int refresh_rate = 60;
  };

  struct DisplayState {
    // move it a bit away from the top by default
    s32 window_pos_x = 50;
    s32 window_pos_y = 50;
    int window_size_width = 640, window_size_height = 480;
    float window_scale_x = 1.f, window_scale_y = 1.f;

    bool pending_size_change = false;
    s32 requested_size_width = 0;
    s32 requested_size_height = 0;

    int num_vmodes = 0;
    VMode vmodes[MAX_VMODES];
    int largest_vmode_width = 640, largest_vmode_height = 480;
    int largest_vmode_refresh_rate = 60;
    VMode current_vmode;
  } m_display_state, m_display_state_copy;
  std::mutex m_lock;

  struct {
    bool pending = false;
    int width = 0;
    int height = 0;
  } m_pending_size;

  GLFWmonitor* get_monitor(int index);
};

extern const GfxRendererModule gRendererOpenGL;
namespace glfw {
static const int NUM_KEYS = GLFW_KEY_LAST + GLFW_MOUSE_BUTTON_LAST + 1;
}
