#pragma once

/*!
 * @file newpad.h
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

#include "common/common_types.h"

#include "game/system/hid/input_bindings.h"

#include "third-party/SDL/include/SDL.h"

/*
TODO:
  - Hotswapping (crash when connecting sometimes, out of bounds somewhere)
  - Fix keyboard for analog keys
  - check for modifiers
  - mouse movement for analog sticks
  - Controller Selection (and names like with monitors)
  - Custom Binds from JSON

  - PS4/PS5 color customizing
*/

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
 protected:
  bool m_loaded = false;
  std::string m_device_name;

  // Each binding is a mapping from an SDL input to one or more InputBindings
  InputBindingGroups m_binds;

 public:
  virtual ~InputDevice(){};

  virtual void process_event(const SDL_Event& event,
                             const CommandBindingGroups& commands,
                             std::shared_ptr<PadData> data) = 0;
  virtual void close_device() = 0;

  bool is_loaded() const { return m_loaded; };
};

// https://wiki.libsdl.org/SDL2/CategoryGameController
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data) override;
  void close_device() override;
  int update_rumble(const u8 low_rumble, const u8 high_rumble);

 private:
  int m_sdl_instance_id = -1;
  SDL_GameController* m_device_handle;
};

class KeyboardDevice : public InputDevice {
 public:
  // TODO - load user binds
  KeyboardDevice() { m_binds = DEFAULT_KEYBOARD_BINDS; };
  ~KeyboardDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data) override;
  void close_device() override{
      // there is nothing to close
  };
};

class MouseDevice : public InputDevice {
 public:
  // TODO - load user binds
  MouseDevice() { m_binds = DEFAULT_MOUSE_BINDS; };
  ~MouseDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data) override;
  void close_device() override{
      // there is nothing to close
  };

  std::pair<int, int> get_mouse_pos() const { return {m_xcoord, m_ycoord}; }

  // TODO - mouse sensitivity

 private:
  int m_xcoord = 0;
  int m_ycoord = 0;
};

// Central class that:
// - keeps track of available input devices
// - polls data from the input devices considered active
// - fetches said data to be sent to the game
class InputMonitor {
 public:
  InputMonitor();
  ~InputMonitor();

  // Propagate and handle the SDL event, ignored it if it's not relevant
  void process_sdl_event(const SDL_Event& event);
  void refresh_device_list();
  std::optional<std::shared_ptr<PadData>> get_current_data(const int port) const;
  int update_rumble(const int port, const u8 low_intensity, const u8 high_intensity);
  void change_active_controller(const int controller_id);
  std::pair<int, int> get_mouse_pos() const { return m_mouse.get_mouse_pos(); }
  void register_command(const CommandBinding::Source source, const CommandBinding bind);

 private:
  /// <summary>
  /// This data can be shared throughout the runtime, it is the current state of the aggregate of
  /// all input sources for the given port
  /// </summary>
  std::unordered_map<int, std::shared_ptr<PadData>> m_data;
  /// <summary>
  /// A list of all the currently connected controllers that we can potentially read data from
  /// if they are mapped to a given port
  /// </summary>
  std::vector<std::shared_ptr<GameController>> m_available_controllers;
  /// <summary>
  /// You can have many keyboards plugged into a computer, but we do not differentiate between them
  /// it's all aggregated under one device.
  /// </summary>
  KeyboardDevice m_keyboard;
  /// <summary>
  /// You can have many mice plugged into a computer, but we do not differentiate between them
  /// it's all aggregated under one device.
  /// </summary>
  MouseDevice m_mouse;
  /// <summary>
  /// A mapping between port numbers and the controller index. Connect as many controllers as you
  /// want.
  /// TODO - when saving controller settings, it'd be wise to use the device GUID or w/e -- instead
  /// of this id that way if they are connected in a different order, we can still reconstruct the
  /// correct mapping
  /// </summary>
  std::unordered_map<int, int> m_controller_port_mapping;
  /// <summary>
  /// The port that the keyboard and mouse will be used for PadData
  /// </summary>
  int m_keyboard_and_mouse_port = 0;
  /// <summary>
  /// Collection of arbitrary commands to run on user actions
  /// </summary>
  CommandBindingGroups m_command_binds;
};
