#include "display_manager.h"

#include "sdl_util.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/format.h"

DisplayManager::DisplayManager(SDL_Window* window)
    : m_window(window), m_selected_fullscreen_display_id(0) {
#ifdef _WIN32
  // Windows hint to disable OS level forced scaling and allow native resolution at non 100% scales
  SetProcessDPIAware();
#endif
  update_curr_display_info();
  update_video_modes();
  // Load display settings from a file
  m_display_settings = game_settings::DisplaySettings();
  // Adjust window / monitor position
  initialize_window_position_from_settings();
}

DisplayManager::~DisplayManager() {
  if (m_window_display_mode == WindowDisplayMode::Windowed) {
    m_display_settings.display_id = m_active_display_id;
    m_display_settings.window_xpos = m_window_xpos;
    m_display_settings.window_ypos = m_window_ypos;
  }
  m_display_settings.save_settings();
}

void DisplayManager::initialize_window_position_from_settings() {
  // Check that the display id is still valid
  if (m_current_display_modes.find(m_display_settings.display_id) ==
      m_current_display_modes.end()) {
    m_display_settings.display_id = 0;
    m_display_settings.window_xpos = 50;
    m_display_settings.window_ypos = 50;
  }

  SDL_Rect rect;
  const auto ok = SDL_GetDisplayBounds(m_display_settings.display_id, &rect);
  if (ok < 0) {
    sdl_util::log_error(fmt::format("unable to get display bounds for display id {}",
                                    m_display_settings.display_id));
  } else {
    // Adjust the settings if they are out of bounds
    if (m_display_settings.window_xpos <= rect.x ||
        m_display_settings.window_xpos + 50 >= rect.x + rect.w) {
      m_display_settings.window_xpos = rect.x + 50;
      m_display_settings.save_settings();
    }
    if (m_display_settings.window_ypos <= rect.y ||
        m_display_settings.window_ypos + 50 > rect.y + rect.h) {
      m_display_settings.window_ypos = rect.y + 50;
      m_display_settings.save_settings();
    }
  }

  SDL_SetWindowPosition(m_window, m_display_settings.window_xpos, m_display_settings.window_ypos);
}

void DisplayManager::process_sdl_event(const SDL_Event& event) {
  const auto event_type = event.type;
  if (event_type == SDL_WINDOWEVENT) {
    // https://wiki.libsdl.org/SDL2/SDL_WindowEvent
    // https://wiki.libsdl.org/SDL2/SDL_WindowEventID
    switch (event.window.event) {
      case SDL_WINDOWEVENT_MINIMIZED:
        m_window_state = WindowState::Minimized;
        break;
      case SDL_WINDOWEVENT_MAXIMIZED:
        m_window_state = WindowState::Maximized;
        break;
      case SDL_WINDOWEVENT_RESTORED:
        m_window_state = WindowState::Restored;
        break;
      case SDL_WINDOWEVENT_MOVED:
        m_window_xpos = event.window.data1;
        m_window_ypos = event.window.data2;
        break;
      case SDL_WINDOWEVENT_RESIZED:
      case SDL_WINDOWEVENT_SIZE_CHANGED:
        m_window_width = event.window.data1;
        m_window_height = event.window.data2;
        break;
      case SDL_WINDOWEVENT_DISPLAY_CHANGED:
        // NOTE - if the user changes the window to a display that doesn't support the same
        // framerate we don't handle that
        update_curr_display_info();
        break;
      case SDL_WINDOWEVENT_ENTER:
        if (m_input_manager && m_input_manager.value()->auto_hiding_cursor()) {
          m_input_manager.value()->hide_cursor(true);
        }
        break;
      case SDL_WINDOWEVENT_LEAVE:
        m_input_manager.value()->hide_cursor(false);
        break;
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
        update_video_modes();
        break;
    }
  }
}

void DisplayManager::process_ee_events() {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  // Fully process any events from the EE
  while (!ee_event_queue.empty()) {
    const auto& evt = ee_event_queue.front();
    switch (evt.type) {
      case EEDisplayEventType::SET_WINDOW_SIZE:
        set_window_size(std::get<int>(evt.param1), std::get<int>(evt.param2));
        break;
      case EEDisplayEventType::SET_WINDOW_DISPLAY_MODE:
        set_window_display_mode(std::get<WindowDisplayMode>(evt.param1));
        break;
      case EEDisplayEventType::SET_FULLSCREEN_DISPLAY_ID:
        set_fullscreen_display_id(std::get<int>(evt.param1));
        break;
    }
    ee_event_queue.pop();
  }
}

std::string DisplayManager::get_connected_display_name(int id) {
  if (m_current_display_modes.find(id) != m_current_display_modes.end()) {
    return m_current_display_modes.at(id).display_name;
  }
  return "";
}

int DisplayManager::get_active_display_refresh_rate() {
  if (m_active_display_id >= 0 &&
      m_current_display_modes.find(m_active_display_id) != m_current_display_modes.end()) {
    return m_current_display_modes.at(m_active_display_id).refresh_rate;
  }
  return 0;
}

int DisplayManager::get_screen_width() {
  if (m_active_display_id >= 0 &&
      m_current_display_modes.find(m_active_display_id) != m_current_display_modes.end()) {
    return m_current_display_modes.at(m_active_display_id).screen_width;
  }
  return 640;
}

int DisplayManager::get_screen_height() {
  if (m_active_display_id >= 0 &&
      m_current_display_modes.find(m_active_display_id) != m_current_display_modes.end()) {
    return m_current_display_modes.at(m_active_display_id).screen_height;
  }
  return 480;
}

Resolution DisplayManager::get_resolution(int id) {
  if (id < (int)m_available_resolutions.size()) {
    return m_available_resolutions.at(id);
  }
  return {0, 0, 0.0};
}

void DisplayManager::enqueue_set_window_size(int width, int height) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_WINDOW_SIZE, width, height});
}

void DisplayManager::set_window_size(int width, int height) {
  SDL_SetWindowSize(m_window, width, height);
}

void DisplayManager::enqueue_set_window_display_mode(WindowDisplayMode mode) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_WINDOW_DISPLAY_MODE, mode, {}});
}

void DisplayManager::set_window_display_mode(WindowDisplayMode mode) {
  // https://wiki.libsdl.org/SDL2/SDL_SetWindowFullscreen
  int result = 0;
  switch (mode) {
    case WindowDisplayMode::Windowed:
      result = SDL_SetWindowFullscreen(m_window, 0);
      if (result == 0) {
        SDL_SetWindowSize(m_window, m_window_width, m_window_height);
      } else {
        sdl_util::log_error("unable to change window to windowed mode");
      }
      break;
    case WindowDisplayMode::Fullscreen:
    case WindowDisplayMode::Borderless:
      // 1. exit fullscreen
      result = SDL_SetWindowFullscreen(m_window, 0);
      if (result == 0) {
        SDL_Rect display_bounds;
        result = SDL_GetDisplayBounds(m_selected_fullscreen_display_id, &display_bounds);
        if (result < 0) {
          sdl_util::log_error(fmt::format("unable to get display bounds for display id {}",
                                          m_selected_fullscreen_display_id));
        } else {
          // 2. move it to the right monitor
          SDL_SetWindowPosition(m_window, display_bounds.x + 50, display_bounds.y + 50);
          if (mode == WindowDisplayMode::Fullscreen) {
            update_video_modes();
            // If fullscreen, we have to resize the window to take up the full resolution
            //
            // Some people are weird and don't use the monitor's maximum supported resolution
            // in which case, we use what the user actually has selected.
            const auto& display_res = m_current_display_modes.at(m_selected_fullscreen_display_id);
            set_window_size(display_res.screen_width, display_res.screen_height);
          }
          // 3. fullscreen it!
          result = SDL_SetWindowFullscreen(m_window, mode == WindowDisplayMode::Fullscreen
                                                         ? SDL_WINDOW_FULLSCREEN
                                                         : SDL_WINDOW_FULLSCREEN_DESKTOP);
          if (result < 0) {
            sdl_util::log_error("unable to switch window fullscreen or borderless fullscreen");
          }
        }
      } else {
        sdl_util::log_error(
            "unable to switch window to windowed mode in order to change fullscreen modes");
      }
      break;
  }
  if (result != 0) {
    sdl_util::log_error(
        fmt::format("unable to change window display mode to {}", fmt::underlying(mode)));
  } else {
    // Set the mode, now that we've been successful
    m_window_display_mode = mode;
  }
}

void DisplayManager::enqueue_set_fullscreen_display_id(int display_id) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_FULLSCREEN_DISPLAY_ID, display_id, {}});
}

void DisplayManager::set_fullscreen_display_id(int display_id) {
  if (display_id >= (int)m_current_display_modes.size()) {
    display_id = 0;
  }
  bool update_fullscreen = m_window_display_mode != WindowDisplayMode::Windowed &&
                           m_selected_fullscreen_display_id != display_id;
  m_selected_fullscreen_display_id = display_id;
  if (update_fullscreen) {
    set_window_display_mode(m_window_display_mode);
  }
}

void DisplayManager::update_curr_display_info() {
  m_active_display_id = SDL_GetWindowDisplayIndex(m_window);
  if (m_active_display_id < 0) {
    sdl_util::log_error("could not retrieve current window's display index");
  }
  SDL_GL_GetDrawableSize(m_window, &m_window_width, &m_window_height);
  SDL_GetWindowPosition(m_window, &m_window_xpos, &m_window_ypos);
  // Update the scale of the display as well
  // TODO - figure out how to do this on SDL
  // https://github.com/libsdl-org/SDL/commit/ab81a559f43abc0858c96788f8e00bbb352287e8
  m_window_scale_x = 1.0;
  m_window_scale_y = 1.0;
}

void DisplayManager::update_video_modes() {
  const auto num_displays = SDL_GetNumVideoDisplays();
  if (num_displays < 0) {
    sdl_util::log_error("could not retrieve number of displays");
    return;
  }

  m_current_display_modes.clear();

  SDL_DisplayMode curr_mode;
  for (int display_id = 0; display_id < num_displays; display_id++) {
    const auto success = SDL_GetCurrentDisplayMode(display_id, &curr_mode);
    if (success != 0) {
      sdl_util::log_error(
          fmt::format("couldn't retrieve current display mode for display id {}", display_id));
      continue;
    }
    auto display_orient = SDL_GetDisplayOrientation(display_id);
    Orientation orient = Orientation::Unknown;
    switch (display_orient) {
      case SDL_ORIENTATION_LANDSCAPE:
        orient = Orientation::Landscape;
        break;
      case SDL_ORIENTATION_LANDSCAPE_FLIPPED:
        orient = Orientation::LandscapeFlipped;
        break;
      case SDL_ORIENTATION_PORTRAIT:
        orient = Orientation::Portrait;
        break;
      case SDL_ORIENTATION_PORTRAIT_FLIPPED:
        orient = Orientation::PortraitFlipped;
        break;
      default:
        orient = Orientation::Unknown;
    }

    std::string display_name_str = "";
    const auto display_name = SDL_GetDisplayName(display_id);
    if (display_name == NULL) {
      sdl_util::log_error(fmt::format("couldn't retrieve display name with id {}", display_id));
    } else {
      display_name_str = display_name;
    }

    DisplayMode new_mode = {display_name_str, curr_mode.format,       curr_mode.w,
                            curr_mode.h,      curr_mode.refresh_rate, orient};
    m_current_display_modes[display_id] = new_mode;
  }
  update_resolutions();
}

void DisplayManager::update_resolutions() {
  // Enumerate display's display modes to get the resolutions
  auto num_display_modes = SDL_GetNumDisplayModes(m_active_display_id);
  SDL_DisplayMode curr_mode;
  for (int i = 0; i < num_display_modes; i++) {
    auto ok = SDL_GetDisplayMode(m_active_display_id, i, &curr_mode);
    if (ok != 0) {
      sdl_util::log_error(fmt::format("unable to get display mode for display {}, index {}",
                                      m_active_display_id, i));
      continue;
    }
    Resolution new_res = {curr_mode.w, curr_mode.h,
                          static_cast<float>(curr_mode.w) / static_cast<float>(curr_mode.h)};
    m_available_resolutions.push_back(new_res);
  }
  // Add original NTSC/PAL options for weird people that want to act like they are on a PS2?
  m_available_resolutions.push_back({512, 448, static_cast<float>(512) / static_cast<float>(448)});
  m_available_resolutions.push_back({512, 224, static_cast<float>(512) / static_cast<float>(224)});

  // Sort by area
  std::sort(m_available_resolutions.begin(), m_available_resolutions.end(),
            [](const Resolution& a, const Resolution& b) -> bool {
              return a.width * a.height > b.width * b.height;
            });

  // Remove duplicate resolutions
  auto last = std::unique(m_available_resolutions.begin(), m_available_resolutions.end(),
                          [](const Resolution& a, const Resolution& b) -> bool {
                            return (a.width == b.width && a.height == b.height);
                          });
  m_available_resolutions.erase(last, m_available_resolutions.end());
}
