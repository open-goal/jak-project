#include "display_manager.h"

#include "sdl_util.h"

#include "common/global_profiler/GlobalProfiler.h"

#include "fmt/core.h"
#include "fmt/format.h"

DisplayManager::DisplayManager(SDL_Window* window) : m_window(window) {
  prof().instant_event("ROOT");
  {
    auto p = scoped_prof("display_manager::init");
    // initialize settings
    m_display_settings = game_settings::DisplaySettings();
#ifdef _WIN32
    // Windows hint to disable OS level forced scaling and allow native resolution at non 100%
    // scales
    SetProcessDPIAware();
#endif
    update_curr_display_info();
    update_video_modes();
    // Load from file now (after initializing current window settings)
    m_display_settings.load_settings();
    // Adjust window / monitor position
    initialize_window_position_from_settings();
    set_display_mode(m_display_settings.display_mode);
  }
}

DisplayManager::~DisplayManager() {
  prof().instant_event("ROOT");
  {
    auto p = scoped_prof("display_manager::destroy");
    if (m_display_settings.display_mode == game_settings::DisplaySettings::DisplayMode::Windowed) {
      m_display_settings.window_xpos = m_window_xpos;
      m_display_settings.window_ypos = m_window_ypos;
    }
    m_display_settings.save_settings();
  }
}

void DisplayManager::initialize_window_position_from_settings() {
  // Check that the display id is still valid
  if (m_current_display_modes.find(m_display_settings.display_id) ==
      m_current_display_modes.end()) {
    lg::warn("[DISPLAY] Saved display ID is no longer valid, resetting to display 0");
    m_display_settings.display_id = 0;
    m_display_settings.window_xpos = 50;
    m_display_settings.window_ypos = 50;
    m_display_settings.save_settings();
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
      lg::warn("[DISPLAY] Saved xpos is out of bounds, resetting");
      m_display_settings.window_xpos = rect.x + 50;
      m_display_settings.save_settings();
    }
    if (m_display_settings.window_ypos <= rect.y ||
        m_display_settings.window_ypos + 50 > rect.y + rect.h) {
      lg::warn("[DISPLAY] Saved ypos is out of bounds, resetting");
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
      case EEDisplayEventType::SET_DISPLAY_MODE:
        set_display_mode(std::get<game_settings::DisplaySettings::DisplayMode>(evt.param1));
        break;
      case EEDisplayEventType::SET_DISPLAY_ID:
        set_display_id(std::get<int>(evt.param1));
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
  if (get_active_display_id() >= 0 &&
      m_current_display_modes.find(get_active_display_id()) != m_current_display_modes.end()) {
    return m_current_display_modes.at(get_active_display_id()).refresh_rate;
  }
  return 0;
}

int DisplayManager::get_screen_width() {
  if (get_active_display_id() >= 0 &&
      m_current_display_modes.find(get_active_display_id()) != m_current_display_modes.end()) {
    return m_current_display_modes.at(get_active_display_id()).screen_width;
  }
  return 640;
}

int DisplayManager::get_screen_height() {
  if (get_active_display_id() >= 0 &&
      m_current_display_modes.find(get_active_display_id()) != m_current_display_modes.end()) {
    return m_current_display_modes.at(get_active_display_id()).screen_height;
  }
  return 480;
}

int DisplayManager::get_num_resolutions(bool for_window_size) {
  if (for_window_size) {
    return m_available_window_sizes.size();
  }
  return m_available_resolutions.size();
}

Resolution DisplayManager::get_resolution(int id, bool for_window_size) {
  if (for_window_size && id < (int)m_available_window_sizes.size()) {
    return m_available_window_sizes.at(id);
  } else if (id < (int)m_available_resolutions.size()) {
    return m_available_resolutions.at(id);
  }
  return {0, 0, 0.0};
}

bool DisplayManager::is_supported_resolution(int width, int height) {
  for (const auto& resolution : m_available_resolutions) {
    if (resolution.width == width && resolution.height == height) {
      return true;
    }
  }
  lg::warn("[DISPLAY] {}x{} is not a supported resolution", width, height);
  return false;
}

void DisplayManager::enqueue_set_window_size(int width, int height) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_WINDOW_SIZE, width, height});
}

void DisplayManager::set_window_size(int width, int height) {
  SDL_SetWindowSize(m_window, width, height);
}

void DisplayManager::enqueue_set_window_display_mode(
    game_settings::DisplaySettings::DisplayMode mode) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_DISPLAY_MODE, mode, {}});
}

void DisplayManager::set_display_mode(game_settings::DisplaySettings::DisplayMode mode) {
  lg::info("[DISPLAY] Setting to display mode: {}", static_cast<int>(mode));
  // https://wiki.libsdl.org/SDL2/SDL_SetWindowFullscreen
  int result = 0;
  switch (mode) {
    case game_settings::DisplaySettings::DisplayMode::Windowed:
      result = SDL_SetWindowFullscreen(m_window, 0);
      if (result == 0) {
        lg::info("[DISPLAY] windowed mode - resizing window to {}x{}", m_window_width,
                 m_window_height);
        SDL_SetWindowSize(m_window, m_window_width, m_window_height);
      } else {
        sdl_util::log_error("unable to change window to windowed mode");
      }
      break;
    case game_settings::DisplaySettings::DisplayMode::Fullscreen:
    case game_settings::DisplaySettings::DisplayMode::Borderless:
      // 1. exit fullscreen
      result = SDL_SetWindowFullscreen(m_window, 0);
      if (result == 0) {
        SDL_Rect display_bounds;
        result = SDL_GetDisplayBounds(get_active_display_id(), &display_bounds);
        if (result < 0) {
          sdl_util::log_error(fmt::format("unable to get display bounds for display id {}",
                                          get_active_display_id()));
        } else {
          // 2. move it to the right monitor
          lg::info("[DISPLAY] preparing fullscreen - moving window to {},{} on display id {}",
                   display_bounds.x + 50, display_bounds.y + 50, get_active_display_id());
          SDL_SetWindowPosition(m_window, display_bounds.x + 50, display_bounds.y + 50);
          if (mode == game_settings::DisplaySettings::DisplayMode::Fullscreen) {
            update_video_modes();
            // If fullscreen, we have to resize the window to take up the full resolution
            //
            // Some people are weird and don't use the monitor's maximum supported resolution
            // in which case, we use what the user actually has selected.
            const auto& display_res = m_current_display_modes.at(get_active_display_id());
            lg::info("[DISPLAY] preparing fullscreen - setting window resolution to {}x{}",
                     display_res.screen_width, display_res.screen_height);
            set_window_size(display_res.screen_width, display_res.screen_height);
          }
          // 3. fullscreen it!
          result = SDL_SetWindowFullscreen(
              m_window, mode == game_settings::DisplaySettings::DisplayMode::Fullscreen
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
    m_display_settings.display_mode = mode;
    m_display_settings.save_settings();
  }
}

void DisplayManager::enqueue_set_display_id(int display_id) {
  const std::lock_guard<std::mutex> lock(event_queue_mtx);
  ee_event_queue.push({EEDisplayEventType::SET_DISPLAY_ID, display_id, {}});
}

void DisplayManager::set_display_id(int display_id) {
  lg::info("[DISPLAY] Setting display id to: {}", display_id);
  if (display_id >= (int)m_current_display_modes.size()) {
    display_id = 0;
  }
  m_display_settings.display_id = display_id;
  if (get_display_mode() != game_settings::DisplaySettings::DisplayMode::Windowed) {
    set_display_mode((game_settings::DisplaySettings::DisplayMode)m_display_settings.display_mode);
  }
  m_display_settings.save_settings();
}

void DisplayManager::update_curr_display_info() {
  lg::info("[DISPLAY] Updating current display info");
  m_display_settings.display_id = SDL_GetWindowDisplayIndex(m_window);
  if (get_active_display_id() < 0) {
    sdl_util::log_error("could not retrieve current window's display index");
  }
  lg::info("[DISPLAY] current display id is {}", m_display_settings.display_id);
  SDL_GL_GetDrawableSize(m_window, &m_window_width, &m_window_height);
  SDL_GetWindowPosition(m_window, &m_window_xpos, &m_window_ypos);
  // Update the scale of the display as well
  // TODO - figure out how to do this on SDL
  // https://github.com/libsdl-org/SDL/commit/ab81a559f43abc0858c96788f8e00bbb352287e8
  m_window_scale_x = 1.0;
  m_window_scale_y = 1.0;
}

void DisplayManager::update_video_modes() {
  lg::info("[DISPLAY] Enumerating video modes");
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
    lg::info(
        "[DISPLAY]: Found monitor {}, currently set to {}x{}@{}hz. Format: {}, Orientation: {}",
        new_mode.display_name, new_mode.screen_width, new_mode.screen_height, new_mode.refresh_rate,
        new_mode.sdl_pixel_format, static_cast<int>(new_mode.orientation));
  }
  update_resolutions();
}

void DisplayManager::update_resolutions() {
  lg::info("[DISPLAY] Enumerating resolutions");
  // Enumerate display's display modes to get the resolutions
  const auto active_display_id = get_active_display_id();
  const auto active_refresh_rate = m_current_display_modes[active_display_id].refresh_rate;
  auto num_display_modes = SDL_GetNumDisplayModes(active_display_id);
  SDL_DisplayMode curr_mode;
  for (int i = 0; i < num_display_modes; i++) {
    auto ok = SDL_GetDisplayMode(active_display_id, i, &curr_mode);
    if (ok != 0) {
      sdl_util::log_error(
          fmt::format("unable to get display mode for display {}, index {}", active_display_id, i));
      continue;
    }
    Resolution new_res = {curr_mode.w, curr_mode.h,
                          static_cast<float>(curr_mode.w) / static_cast<float>(curr_mode.h)};
    // Skip resolutions that aren't using the current refresh rate, they won't work.
    // For example if your monitor is currently set to `60hz` and the monitor _could_ support
    // resolution X but only at `30hz`...then there's no reason for us to consider it as an option.
    if (curr_mode.refresh_rate != active_refresh_rate) {
      lg::debug(
          "[DISPLAY]: Skipping {}x{} as it requires {}hz but the monitor is currently set to {}hz",
          curr_mode.w, curr_mode.h, curr_mode.refresh_rate, active_refresh_rate);
      // Allow it for windowed mode though
      m_available_window_sizes.push_back(new_res);
      continue;
    }
    lg::info("[DISPLAY]: {}x{} is supported", new_res.width, new_res.height);
    m_available_resolutions.push_back(new_res);
    m_available_window_sizes.push_back(new_res);
  }

  // Sort by area
  std::sort(m_available_resolutions.begin(), m_available_resolutions.end(),
            [](const Resolution& a, const Resolution& b) -> bool {
              return a.width * a.height > b.width * b.height;
            });
  std::sort(m_available_window_sizes.begin(), m_available_window_sizes.end(),
            [](const Resolution& a, const Resolution& b) -> bool {
              return a.width * a.height > b.width * b.height;
            });

  // Remove duplicate resolutions
  m_available_resolutions.erase(
      std::unique(m_available_resolutions.begin(), m_available_resolutions.end(),
                  [](const Resolution& a, const Resolution& b) -> bool {
                    return (a.width == b.width && a.height == b.height);
                  }),
      m_available_resolutions.end());
  m_available_window_sizes.erase(
      std::unique(m_available_window_sizes.begin(), m_available_window_sizes.end(),
                  [](const Resolution& a, const Resolution& b) -> bool {
                    return (a.width == b.width && a.height == b.height);
                  }),
      m_available_window_sizes.end());
}
