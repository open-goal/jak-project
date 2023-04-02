#include "input_manager.h"

#include <atomic>
#include <cmath>

#include "input_manager.h"
#include "sdl_util.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"

GameController::GameController(int sdl_device_id,
                               std::shared_ptr<game_settings::InputSettings> settings)
    : m_sdl_instance_id(sdl_device_id) {
  m_settings = settings;
  m_loaded = false;
  m_device_handle = SDL_GameControllerOpen(sdl_device_id);
  if (!m_device_handle) {
    sdl_util::log_error(fmt::format("Could not read data from device index: {}", sdl_device_id));
    return;
  }

  auto joystick = SDL_GameControllerGetJoystick(m_device_handle);
  if (!joystick) {
    sdl_util::log_error(
        fmt::format("Could not get underlying joystick for gamecontroller: id {}", sdl_device_id));
    return;
  }
  m_sdl_instance_id = SDL_JoystickInstanceID(joystick);
  if (m_sdl_instance_id < 0) {
    sdl_util::log_error(
        fmt::format("Could not get instance id for gamecontroller: id {}", sdl_device_id));
    return;
  }
  const auto controller_guid = SDL_JoystickGetGUID(joystick);
  if (controller_guid.data == 0) {
    sdl_util::log_error(fmt::format("Could not get contoller guid with id: {}", sdl_device_id));
    return;
  }
  char guidStr[33];
  SDL_JoystickGetGUIDString(controller_guid, guidStr, sizeof(guidStr));
  m_guid = guidStr;

  auto name = SDL_GameControllerName(m_device_handle);
  if (!name) {
    sdl_util::log_error(fmt::format("Could not get device name with id: {}", sdl_device_id));
    m_device_name = "";
  } else {
    m_device_name = name;
  }
  m_has_led = sdl_util::from_sdl_bool(SDL_GameControllerHasLED(m_device_handle));

  if (m_settings->controller_binds.find(m_guid) == m_settings->controller_binds.end()) {
    m_settings->controller_binds[m_guid] = DEFAULT_CONTROLLER_BINDS;
  }

  m_loaded = true;
}

void GameController::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data,
                                   std::optional<InputBindAssignmentMeta>& bind_assignment) {
  if (event.type == SDL_CONTROLLERAXISMOTION && event.caxis.which == m_sdl_instance_id) {
    // https://wiki.libsdl.org/SDL2/SDL_GameControllerAxis
    if (event.caxis.axis <= SDL_CONTROLLER_AXIS_INVALID ||
        event.caxis.axis >= SDL_CONTROLLER_AXIS_MAX) {
      return;
    }

    auto& binds = m_settings->controller_binds.at(m_guid);

    // Handle analog stick binds
    if (event.caxis.axis >= SDL_CONTROLLER_AXIS_LEFTX &&
        event.caxis.axis <= SDL_CONTROLLER_AXIS_RIGHTY &&
        binds.analog_axii.find(event.caxis.axis) != binds.analog_axii.end()) {
      for (const auto& bind : binds.analog_axii.at(event.caxis.axis)) {
        // Adjust the value range to 0-255 (127 being neutral)
        // Values come out of SDL as -32,768 + 32,767
        int axis_val = ((event.caxis.value + 32768) * 256) / 65536;
        data->analog_data.at(bind.pad_data_index) = axis_val;
      }
    } else if (event.caxis.axis >= SDL_CONTROLLER_AXIS_TRIGGERLEFT &&
               event.caxis.axis <= SDL_CONTROLLER_AXIS_TRIGGERRIGHT &&
               binds.button_axii.find(event.caxis.axis) != binds.button_axii.end()) {
      // Binding re-assignment
      if (bind_assignment) {
        if (bind_assignment->device_type == InputDeviceType::CONTROLLER &&
            !bind_assignment->for_analog) {
          binds.assign_button_bind(event.caxis.axis, bind_assignment.value(), true);
        }
        return;
      }

      // and analog triggers
      for (const auto& bind : binds.button_axii.at(event.caxis.axis)) {
        // TODO - probably want some kind of a deadzone here too so the slightest touch doesn't
        // trigger it (though how does that work for controllers with buttons instead!)
        //
        // TODO - test with switch controller
        data->button_data.at(bind.pad_data_index) = event.caxis.value > 0;
      }
    }
  } else if ((event.type == SDL_CONTROLLERBUTTONDOWN || event.type == SDL_CONTROLLERBUTTONUP) &&
             event.cbutton.which == m_sdl_instance_id) {
    auto& binds = m_settings->controller_binds.at(m_guid);

    // https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
    if (event.cbutton.button <= SDL_CONTROLLER_BUTTON_INVALID ||
        event.cbutton.button >= SDL_CONTROLLER_BUTTON_MAX ||
        binds.buttons.find(event.cbutton.button) == binds.buttons.end()) {
      return;
    }

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_CONTROLLERBUTTONDOWN) {
      if (bind_assignment->device_type == InputDeviceType::CONTROLLER &&
          !bind_assignment->for_analog) {
        binds.assign_button_bind(event.cbutton.button, bind_assignment.value());
      }
      return;
    }

    // Iterate the binds, and apply all of them
    for (const auto& bind : binds.buttons.at(event.cbutton.button)) {
      data->button_data.at(bind.pad_data_index) = event.type == SDL_CONTROLLERBUTTONDOWN;
    }

    // Check for commands
    if (event.type == SDL_CONTROLLERBUTTONDOWN &&
        commands.controller_binds.find(event.cbutton.button) != commands.controller_binds.end()) {
      for (const auto& command : commands.controller_binds.at(event.cbutton.button)) {
        command.command();
      }
    }
  }
}

void GameController::close_device() {
  if (m_device_handle) {
    SDL_GameControllerClose(m_device_handle);
  }
}

int GameController::update_rumble(const u8 low_rumble, const u8 high_rumble) {
  if (m_device_handle) {
    // https://wiki.libsdl.org/SDL2/SDL_GameControllerRumble
    // Arbitrary duration, since it's called every frame anyway, just so the vibration doesn't last
    // forever. SDL expects a value in the range of 0-0xFFFF, so the `257` is just normalizing the
    // 0-255 we get to that range
    if (SDL_GameControllerRumble(m_device_handle, low_rumble * 257, high_rumble * 257, 100) == 0) {
      return 1;
    }
  }
  return 0;
}

void GameController::set_led(const u8 red, const u8 green, const u8 blue) {
  if (!m_has_led) {
    return;
  }
  auto ok = SDL_GameControllerSetLED(m_device_handle, red, green, blue);
  if (ok != 0) {
    m_has_led = false;
  }
}

KeyboardDevice::KeyboardDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
}

void KeyboardDevice::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data,
                                   std::optional<InputBindAssignmentMeta>& bind_assignment) {
  if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
    const auto key_event = event.key;
    if (key_event.repeat != 0) {
      return;
    }

    auto& binds = m_settings->keyboard_binds;

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_KEYDOWN) {
      if (bind_assignment->device_type == InputDeviceType::KEYBOARD) {
        if (bind_assignment->for_analog) {
          binds.assign_analog_bind(key_event.keysym.sym, bind_assignment.value());
        } else {
          binds.assign_button_bind(key_event.keysym.sym, bind_assignment.value());
        }
      }
      return;
    }

    // Normal Buttons
    if (binds.buttons.find(key_event.keysym.sym) != binds.buttons.end()) {
      for (const auto& bind : binds.buttons.at(key_event.keysym.sym)) {
        if (input_bindings::has_necessary_modifiers(bind, key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Buttons (useless for keyboards, but here for completeness)
    if (binds.button_axii.find(key_event.keysym.sym) != binds.button_axii.end()) {
      for (const auto& bind : binds.button_axii.at(key_event.keysym.sym)) {
        if (input_bindings::has_necessary_modifiers(bind, key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Sticks simulating
    if (binds.analog_axii.find(key_event.keysym.sym) != binds.analog_axii.end()) {
      for (const auto& bind : binds.analog_axii.at(key_event.keysym.sym)) {
        if (input_bindings::has_necessary_modifiers(bind, key_event.keysym.mod)) {
          int analog_val = bind.minimum_in_range ? 127 : -127;
          if (event.type == SDL_KEYDOWN) {
            analog_val = bind.minimum_in_range ? -127 : 127;
          }
          data->analog_data.at(bind.pad_data_index) += analog_val;
        }
      }
    }

    // Check for commands
    if (event.type == SDL_KEYDOWN &&
        commands.keyboard_binds.find(key_event.keysym.sym) != commands.keyboard_binds.end()) {
      for (const auto& command : commands.keyboard_binds.at(key_event.keysym.sym)) {
        if (input_bindings::has_necessary_modifiers(command, key_event.keysym.mod)) {
          command.command();
        }
      }
    }
  }
}

MouseDevice::MouseDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
  // https://wiki.libsdl.org/SDL2/SDL_SetRelativeMouseMode
  SDL_SetRelativeMouseMode(sdl_util::sdl_bool(m_enable_mouse_motion_controls));
}

void MouseDevice::process_event(const SDL_Event& event,
                                const CommandBindingGroups& commands,
                                std::shared_ptr<PadData> data,
                                std::optional<InputBindAssignmentMeta>& bind_assignment) {
  // Mouse Button Events
  // https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
  if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
    const auto button_event = event.button;
    auto& binds = m_settings->mouse_binds;

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_MOUSEBUTTONDOWN) {
      if (bind_assignment->device_type == InputDeviceType::MOUSE && !bind_assignment->for_analog) {
        binds.assign_button_bind(button_event.button, bind_assignment.value());
      }
      return;
    }

    // Normal Buttons
    if (binds.buttons.find(button_event.button) != binds.buttons.end()) {
      for (const auto& bind : binds.buttons.at(button_event.button)) {
        if (input_bindings::has_necessary_modifiers(bind, SDL_GetModState())) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_MOUSEBUTTONDOWN;
        }
      }
    }
    // Analog Buttons (useless for mice, but here for completeness)
    if (binds.button_axii.find(button_event.button) != binds.button_axii.end()) {
      for (const auto& bind : binds.button_axii.at(button_event.button)) {
        data->button_data.at(bind.pad_data_index) = event.type == SDL_MOUSEBUTTONDOWN;
      }
    }
    // Analog Sticks simulating
    if (binds.analog_axii.find(button_event.button) != binds.analog_axii.end()) {
      for (const auto& bind : binds.analog_axii.at(button_event.button)) {
        if (input_bindings::has_necessary_modifiers(bind, SDL_GetModState())) {
          int analog_val = event.type == SDL_MOUSEBUTTONDOWN ? 255 : 127;
          if (event.type == SDL_MOUSEBUTTONDOWN && bind.minimum_in_range) {
            analog_val = 0;
          }
          data->analog_data.at(bind.pad_data_index) = analog_val;
        }
      }
    }

    if (m_enable_mouse_motion_controls) {
      // WoW style mouse movement, if you have both buttons held down, you will move forward
      const auto mouse_state = SDL_GetMouseState(NULL, NULL);
      if (event.type == SDL_MOUSEBUTTONDOWN &&
          (mouse_state & SDL_BUTTON_LMASK && mouse_state & SDL_BUTTON_RMASK)) {
        data->analog_data.at(1) += -127;
        m_was_moving_with_mouse = true;
      } else if (m_was_moving_with_mouse) {
        data->analog_data.at(1) += 127;
        m_was_moving_with_mouse = false;
      }
    }

    // Check for commands
    if (event.type == SDL_MOUSEBUTTONDOWN &&
        commands.mouse_binds.find(button_event.button) != commands.mouse_binds.end()) {
      for (const auto& command : commands.mouse_binds.at(button_event.button)) {
        if (input_bindings::has_necessary_modifiers(command, SDL_GetModState())) {
          command.command();
        }
      }
    }
  } else if (event.type == SDL_MOUSEMOTION) {
    // https://wiki.libsdl.org/SDL2/SDL_MouseMotionEvent
    m_xcoord = event.motion.x;
    m_ycoord = event.motion.y;
    if (m_enable_mouse_motion_controls) {
      const auto xadjust = std::clamp(127 + int(float(event.motion.xrel) * m_xsens), 0, 255);
      const auto yadjust = std::clamp(127 + int(float(event.motion.yrel) * m_ysens), 0, 255);
      data->analog_data.at(2) = xadjust;
      data->analog_data.at(3) = yadjust;
    }
  }
}

InputManager::InputManager() {
  // Load user settings
  m_settings = std::make_shared<game_settings::InputSettings>(game_settings::InputSettings());
  // Update to latest controller DB file
  std::string mapping_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "sdl_controller_db.txt").string();
  if (file_util::file_exists(mapping_path)) {
    SDL_GameControllerAddMappingsFromFile(mapping_path.c_str());
  } else {
    lg::error("Could not find SDL Controller DB at path `{}`", mapping_path);
  }
  // Initialize atleast 2 ports, because that's normal for Jak
  // more will be allocated if more controllers are found
  m_data[0] = std::make_shared<PadData>();
  m_data[1] = std::make_shared<PadData>();
  m_keyboard = KeyboardDevice(m_settings);
  m_mouse = MouseDevice(m_settings);

  if (m_data.find(m_keyboard_and_mouse_port) == m_data.end()) {
    m_data[m_keyboard_and_mouse_port] = std::make_shared<PadData>();
  }
  m_command_binds = CommandBindingGroups();
  refresh_device_list();
  ignore_background_controller_events(false);
}

InputManager::~InputManager() {
  for (auto& device : m_available_controllers) {
    device->close_device();
  }
  m_settings->save_settings();
}

void InputManager::refresh_device_list() {
  m_available_controllers.clear();
  m_controller_port_mapping.clear();
  // Enumerate devices
  const auto num_joysticks = SDL_NumJoysticks();
  if (num_joysticks > 0) {
    for (int i = 0; i < num_joysticks; i++) {
      if (!SDL_IsGameController(i)) {
        lg::error("Controller with device id {} is not avaiable via the GameController API", i);
        continue;
      }
      auto controller = std::make_shared<GameController>(i, m_settings);
      m_available_controllers.push_back(controller);
      // By default, controller port mapping is on a first-come-first-served scenario
      //
      // However, we will use previously saved controller port mappings to take precedence
      // For example, if you previous set your PS5 controller to be port 0, then even
      // if another controller is detected first, the PS5 controller should be assigned as expected.
      if (m_settings->controller_port_mapping.find(controller->get_guid()) !=
          m_settings->controller_port_mapping.end()) {
        // Though it's possible for a user to assign multiple controllers to the same port, so the
        // last one wins
        m_controller_port_mapping[m_settings->controller_port_mapping.at(controller->get_guid())] =
            i;
      } else {
        m_controller_port_mapping[m_available_controllers.size() - 1] = i;
        m_settings->controller_port_mapping[controller->get_guid()] =
            m_available_controllers.size() - 1;
      }
      // Allocate a PadData if this is a new port
      if (m_data.find(i) == m_data.end()) {
        m_data[i] = std::make_shared<PadData>();
      }
    }
  }
  if (!m_available_controllers.empty()) {
    lg::warn(
        "No active game controllers could be found or loaded successfully - inputs will not work!");
  }
}

void InputManager::ignore_background_controller_events(const bool ignore) {
  m_ignore_background_controller_events = ignore;
  // TODO - ignoring return value
  SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, ignore ? "0" : "1");
}

void InputManager::process_sdl_event(const SDL_Event& event, const bool ignore_kb_mouse) {
  // Detect controller connections and disconnects
  if (sdl_util::is_any_event_type(event.type,
                                  {SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED})) {
    lg::info("Controller added or removed. refreshing controller device list");
    refresh_device_list();
  }

  if (ignore_kb_mouse && m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
    if (m_keyboard_enabled) {
      m_keyboard.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port),
                               m_waiting_for_bind);
    }
    if (m_mouse_enabled) {
      m_mouse.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port),
                            m_waiting_for_bind);
    }
  }

  // Send event to active controller device
  // This goes last so it takes precedence
  for (const auto& [port, controller_idx] : m_controller_port_mapping) {
    if (m_data.find(port) != m_data.end() && m_available_controllers.size() > controller_idx) {
      m_available_controllers.at(controller_idx)
          ->process_event(event, m_command_binds, m_data.at(port), m_waiting_for_bind);
    }
  }

  // Clear the binding assignment if we got one
  if (m_waiting_for_bind && m_waiting_for_bind->assigned) {
    stop_waiting_for_bind();
  }
}

std::optional<std::shared_ptr<PadData>> InputManager::get_current_data(const int port) const {
  if (m_data.find(port) == m_data.end()) {
    return {};
  }
  return m_data.at(port);
}

int InputManager::update_rumble(int port, u8 low_intensity, u8 high_intensity) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return 0;
  }
  return m_available_controllers.at(m_controller_port_mapping.at(port))
      ->update_rumble(low_intensity, high_intensity);
}

void InputManager::register_command(const CommandBinding::Source source,
                                    const CommandBinding bind) {
  switch (source) {
    case CommandBinding::Source::CONTROLLER:
      if (m_command_binds.controller_binds.find(bind.host_key) ==
          m_command_binds.controller_binds.end()) {
        m_command_binds.controller_binds[bind.host_key] = {};
      }
      m_command_binds.controller_binds[bind.host_key].push_back(bind);
      break;
    case CommandBinding::Source::KEYBOARD:
      if (m_command_binds.keyboard_binds.find(bind.host_key) ==
          m_command_binds.keyboard_binds.end()) {
        m_command_binds.keyboard_binds[bind.host_key] = {};
      }
      m_command_binds.keyboard_binds[bind.host_key].push_back(bind);
      break;
    case CommandBinding::Source::MOUSE:
      if (m_command_binds.mouse_binds.find(bind.host_key) == m_command_binds.mouse_binds.end()) {
        m_command_binds.mouse_binds[bind.host_key] = {};
      }
      m_command_binds.mouse_binds[bind.host_key].push_back(bind);
      break;
  }
}

std::string InputManager::get_controller_name(const int controller_id) {
  if (controller_id >= m_available_controllers.size()) {
    return "";
  }
  return m_available_controllers.at(controller_id)->get_name();
}

std::string InputManager::get_current_bind(const int port,
                                           const InputDeviceType device_type,
                                           const bool buttons,
                                           const int input_idx) {
  std::vector<InputBindingInfo> binding_info;
  switch (device_type) {
    case InputDeviceType::CONTROLLER:
      if (m_controller_port_mapping.find(port) != m_controller_port_mapping.end() &&
          m_controller_port_mapping.at(port) < m_available_controllers.size()) {
        // TODO - some assumptions here to clean up
        binding_info =
            m_settings->controller_binds
                .at(m_available_controllers.at(m_controller_port_mapping.at(port))->get_guid())
                .lookup_button_binds((PadData::ButtonIndex)input_idx);
      }
      break;
    case InputDeviceType::KEYBOARD:
      binding_info =
          m_settings->keyboard_binds.lookup_button_binds((PadData::ButtonIndex)input_idx);
      break;
    case InputDeviceType::MOUSE:
      binding_info = m_settings->mouse_binds.lookup_button_binds((PadData::ButtonIndex)input_idx);
      break;
  }
  if (binding_info.empty()) {
    return "unset";
  }
  return binding_info.front().host_name;
}

void InputManager::set_controller_for_port(const int controller_id, const int port) {
  // Reset inputs as this device won't be able to again!
  m_data.clear();
  if (controller_id < m_available_controllers.size()) {
    auto& controller = m_available_controllers.at(controller_id);
    m_controller_port_mapping[port] = controller_id;
    m_settings->controller_port_mapping[controller->get_guid()] = port;
    m_settings->save_settings();
  }
}

void InputManager::set_controller_led(const int port, const u8 red, const u8 green, const u8 blue) {
  if (m_controller_port_mapping.find(0) == m_controller_port_mapping.end()) {
    return;
  }
  const auto id = m_controller_port_mapping.at(port);
  if (id >= m_available_controllers.size()) {
    return;
  }
  m_available_controllers.at(id)->set_led(red, green, blue);
}

void InputManager::enable_keyboard(const bool enabled) {
  m_keyboard_enabled = enabled;
  if (!enabled) {
    // Reset inputs as this device won't be able to again!
    m_data.clear();
  }
}

void InputManager::enable_mouse(const bool enabled) {
  m_mouse_enabled = enabled;
  if (!enabled) {
    // Reset inputs as this device won't be able to again!
    m_data.clear();
  }
}

void InputManager::set_wait_for_bind(const InputDeviceType device_type,
                                     const bool for_analog,
                                     const bool for_minimum_analog,
                                     const int input_idx) {
  m_waiting_for_bind = InputBindAssignmentMeta();
  m_waiting_for_bind->device_type = device_type;
  m_waiting_for_bind->pad_idx = input_idx;
  m_waiting_for_bind->for_analog = for_analog;
  m_waiting_for_bind->for_analog_minimum = for_minimum_analog;
}
