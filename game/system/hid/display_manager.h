#pragma once

#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <unordered_map>
#include <variant>

#include "game/settings/settings.h"
#include "game/system/hid/input_manager.h"

#include "third-party/SDL/include/SDL.h"

/*
TODO:
  - hiDPI support, see https://wiki.libsdl.org/SDL2/SDL_GetRendererOutputSize
*/

enum class WindowState { Minimized, Maximized, Restored };

enum class Orientation { Landscape, LandscapeFlipped, Portrait, PortraitFlipped, Unknown };

/// https://wiki.libsdl.org/SDL2/SDL_DisplayMode
struct DisplayMode {
  std::string display_name;
  /// https://wiki.libsdl.org/SDL2/SDL_PixelFormatEnum
  uint32_t sdl_pixel_format;
  int screen_width;
  int screen_height;
  /// refresh rate (in Hz), or 0 for unspecified
  int refresh_rate;
  Orientation orientation;
};

// Describes an available resolution
struct Resolution {
  int width;
  int height;
  float aspect_ratio;
};

/// Monitors and handles all SDL events related to monitors and the window position
/// Stores related info for other parts of the application to use
/// Manages display related operations and querying
class DisplayManager {
 private:
  enum class EEDisplayEventType { SET_WINDOW_SIZE, SET_DISPLAY_MODE, SET_DISPLAY_ID };

  struct EEDisplayEvent {
    EEDisplayEventType type;
    std::variant<bool, int, game_settings::DisplaySettings::DisplayMode> param1;
    std::variant<bool, int, game_settings::DisplaySettings::DisplayMode> param2;
  };

 public:
  DisplayManager(SDL_Window* window);
  ~DisplayManager();

  /// Propagate and handle the SDL event, ignoring it if it's not relevant
  void process_sdl_event(const SDL_Event& event);
  /// Any event coming from the EE thread that interacts directly with SDL should be enqueued as an
  /// event so it can be ran from the proper thread context (the graphics thread)
  void process_ee_events();

  int get_active_display_id() { return m_display_settings.display_id; }
  bool is_window_active() { return m_window != nullptr; }
  bool is_minimized() { return m_window_state == WindowState::Minimized; }
  int get_window_width() { return m_window_width; }
  int get_window_height() { return m_window_height; }
  int get_window_game_width() { return m_window_game_width; }
  int get_window_game_height() { return m_window_game_height; }
  float get_window_scale_x() { return m_window_scale_x; }
  float get_window_scale_y() { return m_window_scale_y; }
  int num_connected_displays() { return m_current_display_modes.size(); }
  std::string get_connected_display_name(int id);
  int get_active_display_refresh_rate();
  int get_screen_width();
  int get_screen_height();
  game_settings::DisplaySettings::DisplayMode get_display_mode() {
    return m_display_settings.display_mode;
  }
  int get_num_resolutions(bool for_window_size);
  Resolution get_resolution(int id, bool for_window_size);
  bool is_supported_resolution(int width, int height);

  // Mutators
  void enqueue_set_window_size(int width, int height);
  void enqueue_set_window_display_mode(game_settings::DisplaySettings::DisplayMode mode);
  void enqueue_set_display_id(int display_id);

  void set_game_size(int width, int height) {
    m_window_game_width = width;
    m_window_game_height = height;
  }

  void set_input_manager(std::shared_ptr<InputManager> input_manager) {
    m_input_manager = input_manager;
  }

 private:
  SDL_Window* m_window;
  game_settings::DisplaySettings m_display_settings;
  std::optional<std::shared_ptr<InputManager>> m_input_manager;

  std::mutex event_queue_mtx;
  std::queue<EEDisplayEvent> ee_event_queue;

  int m_window_xpos = 0;
  int m_window_ypos = 0;
  int m_window_width = 0;
  int m_window_height = 0;
  int m_window_game_width = 0;
  int m_window_game_height = 0;
  float m_window_scale_x = 1.0;
  float m_window_scale_y = 1.0;
  WindowState m_window_state = WindowState::Restored;

  // The currently set display mode for each display
  // There is no reason to keep track of all display modes for all monitors
  // the only one that matters is the one the user _currently_ has configured
  //
  // ie. allowing someone to set 150fps on a monitor set to 60hz is not correct
  std::unordered_map<int, DisplayMode> m_current_display_modes;
  std::vector<Resolution> m_available_resolutions;
  std::vector<Resolution> m_available_window_sizes;

  void initialize_window_position_from_settings();
  void update_curr_display_info();
  void update_video_modes();
  void update_resolutions();

  void set_window_size(int width, int height);
  void set_display_mode(game_settings::DisplaySettings::DisplayMode mode);
  void set_display_id(int display_id);
};
