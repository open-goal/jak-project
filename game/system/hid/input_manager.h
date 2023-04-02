#pragma once

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

#include "common/common_types.h"

#include "game/system/hid/input_bindings.h"
#include <game/settings/settings.h>

#include "third-party/SDL/include/SDL.h"

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
 protected:
  bool m_loaded = false;
  std::shared_ptr<game_settings::InputSettings> m_settings;

 public:
  virtual ~InputDevice(){};

  virtual void process_event(const SDL_Event& event,
                             const CommandBindingGroups& commands,
                             std::shared_ptr<PadData> data,
                             std::optional<InputBindAssignmentMeta>& bind_assignment) = 0;
  virtual void close_device() = 0;
  bool is_loaded() const { return m_loaded; };
};

// https://wiki.libsdl.org/SDL2/CategoryGameController
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id, std::shared_ptr<game_settings::InputSettings> settings);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override;
  int update_rumble(const u8 low_rumble, const u8 high_rumble);
  std::string get_name() const { return m_device_name; }
  bool has_led() { return m_has_led; }
  void set_led(const u8 red, const u8 green, const u8 blue);
  std::string get_guid() { return m_guid; }

 private:
  int m_sdl_instance_id = -1;
  SDL_GameController* m_device_handle;
  std::string m_device_name = "";
  bool m_has_led;
  std::string m_guid = "";
};

class KeyboardDevice : public InputDevice {
 public:
  KeyboardDevice(){};
  KeyboardDevice(std::shared_ptr<game_settings::InputSettings> settings);
  ~KeyboardDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override{
      // there is nothing to close
  };
};

class MouseDevice : public InputDevice {
 public:
  MouseDevice(){};
  MouseDevice(std::shared_ptr<game_settings::InputSettings> settings);
  ~MouseDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override{
      // there is nothing to close
  };

  void enable_mouse_motion_controls(bool enable) { m_enable_mouse_motion_controls = enable; }

  std::pair<int, int> get_mouse_pos() const { return {m_xcoord, m_ycoord}; }

 private:
  int m_xcoord = 0;
  int m_ycoord = 0;

  bool m_enable_mouse_motion_controls = false;
  bool m_was_moving_with_mouse = false;
  float m_ysens = 10.0;
  float m_xsens = -15.0;
};

/// Central class that:
/// - keeps track of available input devices
/// - polls data from the input devices considered active
/// - fetches said data to be sent to the game
class InputManager {
 public:
  InputManager();
  ~InputManager();

  // Propagate and handle the SDL event, ignored it if it's not relevant
  void process_sdl_event(const SDL_Event& event, const bool ignore_kb_mouse);
  void refresh_device_list();
  void ignore_background_controller_events(const bool ignore);

  std::optional<std::shared_ptr<PadData>> get_current_data(const int port) const;
  int update_rumble(const int port, const u8 low_intensity, const u8 high_intensity);
  std::pair<int, int> get_mouse_pos() const { return m_mouse.get_mouse_pos(); }

  void register_command(const CommandBinding::Source source, const CommandBinding bind);

  int get_num_controllers() const { return m_available_controllers.size(); }
  std::string get_controller_name(const int controller_id);
  std::string get_current_bind(const int port,
                               const InputDeviceType device_type,
                               const bool buttons,
                               const int input_idx);
  void set_controller_for_port(const int controller_id, const int port);
  void set_controller_led(const int port, const u8 red, const u8 green, const u8 blue);
  void enable_keyboard(const bool enabled);
  void enable_mouse(const bool enabled);
  bool get_waiting_for_bind() const { return m_waiting_for_bind.has_value(); }
  void set_wait_for_bind(const InputDeviceType device_type,
                         const bool for_analog,
                         const bool for_minimum_analog,
                         const int input_idx);
  void stop_waiting_for_bind() { m_waiting_for_bind = std::nullopt; }

 private:
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
  /// TODO - when saving controller settings, it'd be wise to use the device GUID or w/e -- instead
  /// of this id that way if they are connected in a different order, we can still reconstruct the
  /// correct mapping
  std::unordered_map<int, int> m_controller_port_mapping;
  /// The port that the keyboard and mouse will be used for PadData
  int m_keyboard_and_mouse_port = 0;
  /// Collection of arbitrary commands to run on user actions
  CommandBindingGroups m_command_binds;

  bool m_keyboard_enabled = true;
  bool m_mouse_enabled = false;

  bool m_ignore_background_controller_events = false;

  /// No inputs will be processed while in this mode the first input detected from the relevant
  /// device type will be used to set the bind and clear the flag
  ///
  /// The game will poll for the status of this flag to know if a bind has been assigned
  std::optional<InputBindAssignmentMeta> m_waiting_for_bind = std::nullopt;
};
