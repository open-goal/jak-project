#pragma once

/*!
 * @file opengl.h
 * OpenGL includes.
 */

#define GLFW_INCLUDE_NONE
#include <mutex>

#include "game/graphics/display.h"
#include "game/graphics/gfx.h"

#include "third-party/SDL/include/SDL.h"
#include "third-party/glad/include/glad/glad.h"

class GLDisplay : public GfxDisplay {
 public:
  GLDisplay(SDL_Window* window, SDL_GLContext gl_context, bool is_main);
  virtual ~GLDisplay();

  // Overrides
  std::shared_ptr<DisplayManager> get_display_manager() const override { return m_display_manager; }
  std::shared_ptr<InputManager> get_input_manager() const override { return m_input_manager; }

  void render() override;

 private:
  SDL_Window* m_window;
  SDL_GLContext m_gl_context;

  std::shared_ptr<DisplayManager> m_display_manager;
  std::shared_ptr<InputManager> m_input_manager;

  bool m_should_quit = false;
  bool m_take_screenshot_next_frame = false;
  void process_sdl_events();

  struct DisplayState {
    // move it a bit away from the top by default
    s32 window_pos_x = 50;
    s32 window_pos_y = 50;
    int window_size_width = 640;
    int window_size_height = 480;
    float window_scale_x = 1.f;
    float window_scale_y = 1.f;

    bool pending_size_change = false;
    s32 requested_size_width = 0;
    s32 requested_size_height = 0;
  } m_display_state, m_display_state_copy;
  std::mutex m_lock;

  struct {
    bool pending = false;
    int width = 0;
    int height = 0;
  } m_pending_size;
};

extern const GfxRendererModule gRendererOpenGL;
