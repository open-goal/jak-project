#pragma once

#include <array>
#include <memory>
#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <unordered_map>
#include <variant>

#include "common/common_types.h"

#include "devices/game_controller.h"
#include "devices/keyboard.h"
#include "devices/mouse.h"
#include "game/settings/settings.h"
#include "game/system/hid/input_bindings.h"

#include "third-party/SDL/include/SDL.h"

/// Central class that:
/// - keeps track of available input devices
/// - polls data from the input devices considered active
/// - fetches said data to be sent to the game
class InputManager {
 private:
  enum class EEInputEventType {
    IGNORE_BACKGROUND_CONTROLLER_EVENTS,
    UPDATE_RUMBLE,
    SET_CONTROLLER_LED,
    UPDATE_MOUSE_OPTIONS,
    SET_AUTO_HIDE_MOUSE
  };

  struct EEInputEvent {
    EEInputEventType type;
    std::variant<bool, int> param1;
    std::variant<bool, u8> param2;
    std::variant<bool, u8> param3;
    std::variant<u8> param4;
  };

 public:
  InputManager();
  ~InputManager();

  // Propagate and handle the SDL event, ignored it if it's not relevant
  void process_sdl_event(const SDL_Event& event);
  void poll_keyboard_data();
  void clear_keyboard_actions();
  void poll_mouse_data();
  void clear_mouse_actions();
  // Any cleanup that should happen after polling has completed for this frame
  void finish_polling();
  /// Any event coming from the EE thread that interacts directly with SDL should be enqueued as an
  /// event so it can be ran from the proper thread context (the graphics thread)
  void process_ee_events();
  void register_command(const CommandBinding::Source source, const CommandBinding bind);

  std::optional<std::shared_ptr<PadData>> get_current_data(const int port) const;
  std::pair<int, int> get_mouse_pos() const { return m_mouse.get_mouse_pos(); }
  MouseDevice::MouseButtonStatus get_mouse_button_status() const {
    return m_mouse.get_mouse_button_status();
  }

  // These functions can be called from the EE and interact with SDL directly
  // These should be enqueued in the event that for example, you try to set
  // the controller LED while the controller is disconnecting
  void enqueue_ignore_background_controller_events(const bool ignore);
  void enqueue_update_rumble(const int port, const u8 low_intensity, const u8 high_intensity);
  void enqueue_set_controller_led(const int port, const u8 red, const u8 green, const u8 blue);
  void enqueue_update_mouse_options(const bool enabled,
                                    const bool control_camera,
                                    const bool control_movement);
  void enqueue_set_auto_hide_mouse(const bool auto_hide_mouse);

  // These functions can be called from the EE but they only interact with
  // the InputManager, so it shouldn't hurt (they don't need to be enqueued)
  // Also these are generally waiting for a response and...OpenGOAL does not have an async/await
  // pattern!
  int get_num_controllers() const { return m_available_controllers.size(); }
  std::string get_controller_name(const int controller_id);
  std::string get_current_bind(const int port,
                               const InputDeviceType device_type,
                               const bool buttons,
                               const int input_idx,
                               const bool analog_for_minimum);
  int get_controller_index(const int port);
  void set_controller_for_port(const int controller_id, const int port);
  bool controller_has_led(const int port);
  bool controller_has_rumble(const int port);
  void enable_keyboard(const bool enabled);
  bool get_waiting_for_bind() const { return m_waiting_for_bind.has_value(); }
  void set_wait_for_bind(const InputDeviceType device_type,
                         const bool for_analog,
                         const bool for_minimum_analog,
                         const int input_idx);
  void stop_waiting_for_bind() { m_waiting_for_bind = std::nullopt; }
  void set_camera_sens(const float xsens, const float ysens);
  void reset_input_bindings_to_defaults(const int port, const InputDeviceType device_type);
  bool auto_hiding_cursor() { return m_auto_hide_mouse || m_mouse.is_camera_being_controlled(); }
  void hide_cursor(const bool hide_cursor);
  bool is_keyboard_enabled() {
    return m_settings->keyboard_enabled || m_settings->keyboard_temp_enabled;
  }

 private:
  std::mutex m_event_queue_mtx;
  std::queue<EEInputEvent> ee_event_queue;

  std::shared_ptr<game_settings::InputSettings> m_settings;

  /// This data can be shared throughout the runtime, it is the current state of the
  /// aggregate of all input sources for the given port
  std::unordered_map<int, std::shared_ptr<PadData>> m_data;
  /// A list of all the currently connected controllers that we can potentially read data
  /// from if they are mapped to a given port
  std::vector<std::shared_ptr<GameController>> m_available_controllers;
  /// You can have many keyboards plugged into a computer, but we do not differentiate
  /// between them it's all aggregated under one device.
  KeyboardDevice m_keyboard;
  /// You can have many mice plugged into a computer, but we do not differentiate between
  /// them it's all aggregated under one device.
  MouseDevice m_mouse;
  /// A mapping between port numbers and the controller index. Connect as many controllers as
  /// you want.
  std::unordered_map<int, int> m_controller_port_mapping;
  /// The port that the keyboard and mouse will be used for PadData
  int m_keyboard_and_mouse_port = 0;
  /// Collection of arbitrary commands to run on user actions
  CommandBindingGroups m_command_binds;

  bool m_mouse_enabled = false;
  int m_skip_polling_for_n_frames = 0;
  bool m_auto_hide_mouse = true;
  bool m_mouse_currently_hidden = false;
  bool m_ignore_background_controller_events = false;

  /// No inputs will be processed while in this mode the first input detected from the relevant
  /// device type will be used to set the bind and clear the flag
  ///
  /// The game will poll for the status of this flag to know if a bind has been assigned
  std::optional<InputBindAssignmentMeta> m_waiting_for_bind = std::nullopt;

  void refresh_device_list();
  void clear_inputs();

  void ignore_background_controller_events(const bool ignore);
  int update_rumble(const int port, const u8 low_intensity, const u8 high_intensity);
  void set_controller_led(const int port, const u8 red, const u8 green, const u8 blue);
  void update_mouse_options(const bool enabled,
                            const bool control_camera,
                            const bool control_movement);
  void set_auto_hide_mouse(const bool auto_hide_mouse);
};
