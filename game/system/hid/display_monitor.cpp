#include "display_monitor.h"

#include "sdl_util.h"

#include "third-party/fmt/core.h"

DisplayMonitor::DisplayMonitor(SDL_Window* window) : m_window(window) {
  update_curr_display_info();
  update_video_modes();
}

void DisplayMonitor::process_sdl_event(const SDL_Event& event) {
  const auto event_type = event.type;
  if (event_type == SDL_WINDOWEVENT) {
    // https://wiki.libsdl.org/SDL2/SDL_WindowEvent
    // https://wiki.libsdl.org/SDL2/SDL_WindowEventID
    switch (event.window.event) {
      case SDL_WINDOWEVENT_MINIMIZED:
        m_is_minimized = true;
        break;
      case SDL_WINDOWEVENT_RESTORED:
        m_is_minimized = false;
        break;
      case SDL_WINDOWEVENT_MOVED:
        m_curr_window_xpos = event.window.data1;
        m_curr_window_ypos = event.window.data2;
        break;
      case SDL_WINDOWEVENT_RESIZED:
      case SDL_WINDOWEVENT_SIZE_CHANGED:
        m_window_width = event.window.data1;
        m_window_height = event.window.data2;
        update_curr_window_scaling();
        break;
      case SDL_WINDOWEVENT_DISPLAY_CHANGED:
        m_active_display_id = event.window.data1;
    }
  } else if (event_type == SDL_DISPLAYEVENT) {
    // https://wiki.libsdl.org/SDL2/SDL_DisplayEventID
    switch (event.display.event) {
      case SDL_DISPLAYEVENT_CONNECTED:
      case SDL_DISPLAYEVENT_DISCONNECTED:
        update_curr_display_info();
        update_video_modes();
        break;
      case SDL_DISPLAYEVENT_ORIENTATION:
        // TODO - do i have to invert width/height?
        break;
    }
  }
}

void DisplayMonitor::update_curr_display_info() {
  m_active_display_id = SDL_GetWindowDisplayIndex(m_window);
  if (m_active_display_id < 0) {
    sdl_util::log_error("could not retrieve current window's display index");
  }
  SDL_GL_GetDrawableSize(m_window, &m_window_width, &m_window_height);
  SDL_DisplayMode curr_mode;
  SDL_GetCurrentDisplayMode(m_active_display_id, &curr_mode);
  DisplayMode new_mode = {};
  new_mode.screen_width = curr_mode.w;
  new_mode.screen_height = curr_mode.h;
  new_mode.refresh_rate = curr_mode.refresh_rate;
  new_mode.sdl_pixel_format = curr_mode.format;
  m_active_display_mode = new_mode;
  // Update the scale of the display as well
  update_curr_window_scaling();
}

void DisplayMonitor::update_curr_window_scaling() {
  // TODO - figure out how to do this on SDL
  // https://github.com/libsdl-org/SDL/commit/ab81a559f43abc0858c96788f8e00bbb352287e8
  m_window_scale_x = 1.0;
  m_window_scale_y = 1.0;
}

void DisplayMonitor::update_video_modes() {
  // TODO - poll for the current video mode too!

  const auto num_displays = SDL_GetNumVideoDisplays();
  if (num_displays < 0) {
    sdl_util::log_error("could not retrieve number of displays");
    return;
  }

  m_display_modes.clear();

  for (int display_id = 0; display_id < num_displays; display_id++) {
    const auto num_display_modes = SDL_GetNumDisplayModes(display_id);
    if (num_display_modes < 0) {
      sdl_util::log_error(fmt::format(
          "could not retrieve number of display modes for display idx: {}", display_id));
      continue;
    }
    m_display_modes[display_id] = {};
    for (int mode_id = 0; mode_id < num_display_modes; mode_id++) {
      SDL_DisplayMode curr_mode;
      const auto ok = SDL_GetDisplayMode(display_id, mode_id, &curr_mode);
      if (ok != 0) {
        sdl_util::log_error(
            fmt::format("could not retrieve display mode info, window: {} | display_idx: {}",
                        display_id, mode_id));
        continue;
      }
      DisplayMode new_mode = {};
      new_mode.screen_width = curr_mode.w;
      new_mode.screen_height = curr_mode.h;
      new_mode.refresh_rate = curr_mode.refresh_rate;
      new_mode.sdl_pixel_format = curr_mode.format;
      m_display_modes.at(display_id).push_back(new_mode);
    }
  }
}
