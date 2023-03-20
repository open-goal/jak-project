#pragma once

#include <optional>
#include <string>
#include <unordered_map>

#include "third-party/SDL/include/SDL.h"

/*
TODO:
  - Fix MSAA OFF, something to do with `make_fbo`
    - Also hide options beyond the user's MSAA setting (ie. my GPU only supports up to x8, but the menu has x16)
  - Show/Hide cursor based on if mouse controls are enabled
  - Window position and set it on startup properly
  handle edge case of window going outside bounds 
  - scale testing
  - selecting monitor

  - hiDPI support
    - see https://wiki.libsdl.org/SDL2/SDL_GetRendererOutputSize
*/

enum WindowDisplayMode { Windowed = 0, Fullscreen = 1, Borderless = 2 };

/// https://wiki.libsdl.org/SDL2/SDL_DisplayMode
struct DisplayMode {
  /// https://wiki.libsdl.org/SDL2/SDL_PixelFormatEnum
  uint32_t sdl_pixel_format;
  int screen_width;
  int screen_height;
  /// refresh rate (in Hz), or 0 for unspecified
  int refresh_rate;
};

/// Monitors and handles all SDL events related to monitors and the window position
/// Stores related info for other parts of the application to use
/// Manages display related operations and querying
/// TODO - change to manager?
class DisplayMonitor {
 public:
  DisplayMonitor(SDL_Window* window);

  /// Propagate and handle the SDL event, ignoring it if it's not relevant
  void process_sdl_event(const SDL_Event& event);

  // Accessors
  bool is_window_active() { return m_window != nullptr; }
  bool is_minimized() { return m_is_minimized; }
  int get_window_width() { return m_window_width; }
  int get_window_height() { return m_window_height; }
  float get_window_scale_x() { return m_window_scale_x; }
  float get_window_scale_y() { return m_window_scale_y; }
  int num_connected_displays() { return m_display_modes.size(); }
  std::string get_connected_display_name(int id);
  int get_active_display_mode_count() {
    if (m_display_modes.find(m_active_display_id) != m_display_modes.end()) {
      return m_display_modes.at(m_active_display_id).size();
    }
    return 0;
  }
  int get_active_display_refresh_rate() {
    if (m_active_display_mode) {
      return m_active_display_mode->refresh_rate;
    }
    return 0;
  }
  int get_screen_width() {
    if (m_active_display_mode) {
      return m_active_display_mode->screen_width;
    }
    // TODO - good idea to return 0?
    return 0;
  }
  int get_screen_height() {
    if (m_active_display_mode) {
      return m_active_display_mode->screen_height;
    }
    // TODO - good idea to return 0?
    return 0;
  }
  WindowDisplayMode get_window_display_mode() { return m_window_display_mode; }

  // Mutators
  void set_window_resizable(bool resizable) {
    if (m_window) {
      SDL_SetWindowResizable(m_window, resizable ? SDL_TRUE : SDL_FALSE);
    }
  }
  void set_window_size(int width, int height);
  void set_window_display_mode(WindowDisplayMode mode);

 private:
  SDL_Window* m_window;
  int m_active_display_id;
  std::optional<DisplayMode> m_active_display_mode;
  WindowDisplayMode m_window_display_mode = Windowed;

  int m_curr_window_xpos;
  int m_curr_window_ypos;
  int m_window_width;
  int m_window_height;
  float m_window_scale_x;
  float m_window_scale_y;

  bool m_is_minimized;

  /// A map of all video modes for each display ID
  std::unordered_map<int, std::vector<DisplayMode>> m_display_modes;

  void update_curr_display_info();
  void update_curr_window_scaling();
  void update_video_modes();
};
