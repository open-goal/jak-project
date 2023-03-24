#include "input_manager.h"

#include <atomic>
#include <cmath>

#include "sdl_util.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"

GameController::GameController(int sdl_device_id) : m_sdl_instance_id(sdl_device_id) {
  // TODO - load user binds
  m_binds = DEFAULT_CONTROLLER_BINDS;
  m_loaded = false;
  m_device_handle = SDL_GameControllerOpen(sdl_device_id);
  if (!m_device_handle) {
    lg::error("Could not read data from device index `{}`: {}", sdl_device_id, SDL_GetError());
    return;
  }

  auto joystick = SDL_GameControllerGetJoystick(m_device_handle);
  if (!joystick) {
    lg::error("Could not get underlying joystick for gamecontroller: id {}", sdl_device_id);
    return;
  }
  m_sdl_instance_id = SDL_JoystickInstanceID(joystick);
  if (m_sdl_instance_id < 0) {
    lg::error("Could not get instance id for gamecontroller: id {}", sdl_device_id);
    return;
  }
  auto name = SDL_GameControllerName(m_device_handle);
  if (!name) {
    sdl_util::log_error(fmt::format("Could not get device name with id: {}", sdl_device_id));
    m_device_name = "";
  } else {
    m_device_name = name;
  }
  m_loaded = true;
}

void GameController::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data) {
  if (event.type == SDL_CONTROLLERAXISMOTION && event.caxis.which == m_sdl_instance_id) {
    // https://wiki.libsdl.org/SDL2/SDL_GameControllerAxis
    if (event.caxis.axis <= SDL_CONTROLLER_AXIS_INVALID ||
        event.caxis.axis >= SDL_CONTROLLER_AXIS_MAX) {
      return;
    }

    // Handle analog stick binds
    if (event.caxis.axis >= SDL_CONTROLLER_AXIS_LEFTX &&
        event.caxis.axis <= SDL_CONTROLLER_AXIS_RIGHTY &&
        m_binds.analog_axii.find(event.caxis.axis) != m_binds.analog_axii.end()) {
      for (const auto& bind : m_binds.analog_axii.at(event.caxis.axis)) {
        // Adjust the value range to 0-255 (127 being neutral)
        // Values come out of SDL as -32,768 + 32,767
        int axis_val = ((event.caxis.value + 32768) * 256) / 65536;
        data->analog_data.at(bind.pad_data_index) = axis_val;
      }
    } else if (event.caxis.axis >= SDL_CONTROLLER_AXIS_TRIGGERLEFT &&
               event.caxis.axis <= SDL_CONTROLLER_AXIS_TRIGGERRIGHT &&
               m_binds.button_axii.find(event.caxis.axis) != m_binds.button_axii.end()) {
      // and analog triggers
      for (const auto& bind : m_binds.button_axii.at(event.caxis.axis)) {
        // TODO - probably want some kind of a deadzone here too so the slightest touch doesn't
        // trigger it (though how does that work for controllers with buttons instead!)
        //
        // TODO - test with switch controller
        data->button_data.at(bind.pad_data_index) = event.caxis.value > 0;
      }
    }
  } else if ((event.type == SDL_CONTROLLERBUTTONDOWN || event.type == SDL_CONTROLLERBUTTONUP) &&
             event.cbutton.which == m_sdl_instance_id) {
    // https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
    if (event.cbutton.button <= SDL_CONTROLLER_BUTTON_INVALID ||
        event.cbutton.button >= SDL_CONTROLLER_BUTTON_MAX ||
        m_binds.buttons.find(event.cbutton.button) == m_binds.buttons.end()) {
      return;
    }

    // Iterate the binds, and apply all of them
    for (const auto& bind : m_binds.buttons.at(event.cbutton.button)) {
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
    // forever SDL expects in the range of 0-0xFFFF, so the `257` is just normalizing the 0-255 we
    // get
    if (SDL_GameControllerRumble(m_device_handle, low_rumble * 257, high_rumble * 257, 100) == 0) {
      return 1;
    }
  }
  return 0;
}

bool has_necessary_modifiers(const bool need_alt,
                             const bool need_ctrl,
                             const bool need_meta,
                             const bool need_shift,
                             const u16 key_modifiers) {
  // https://wiki.libsdl.org/SDL2/SDL_Keymod
  if (need_alt && ((key_modifiers & KMOD_ALT) == 0)) {
    return false;
  }
  if (need_ctrl && ((key_modifiers & KMOD_CTRL) == 0)) {
    return false;
  }
  if (need_meta && ((key_modifiers & KMOD_GUI) == 0)) {
    return false;
  }
  if (need_shift && ((key_modifiers & KMOD_SHIFT) == 0)) {
    return false;
  }
  return true;
}

bool has_necessary_modifiers(const CommandBinding& bind, const u16 key_modifiers) {
  return has_necessary_modifiers(bind.need_alt, bind.need_ctrl, bind.need_meta, bind.need_shift,
                                 key_modifiers);
}

bool has_necessary_modifiers(const InputBinding& bind, const u16 key_modifiers) {
  return has_necessary_modifiers(bind.need_alt, bind.need_ctrl, bind.need_meta, bind.need_shift,
                                 key_modifiers);
}

void KeyboardDevice::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data) {
  if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
    const auto key_event = event.key;
    if (key_event.repeat != 0) {
      return;
    }
    // Normal Buttons
    if (m_binds.buttons.find(key_event.keysym.sym) != m_binds.buttons.end()) {
      for (const auto& bind : m_binds.buttons.at(key_event.keysym.sym)) {
        if (has_necessary_modifiers(bind, key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Buttons (useless for keyboards)
    if (m_binds.button_axii.find(key_event.keysym.sym) != m_binds.button_axii.end()) {
      for (const auto& bind : m_binds.button_axii.at(key_event.keysym.sym)) {
        if (has_necessary_modifiers(bind, key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Sticks simulating
    if (m_binds.analog_axii.find(key_event.keysym.sym) != m_binds.analog_axii.end()) {
      for (const auto& bind : m_binds.analog_axii.at(key_event.keysym.sym)) {
        if (has_necessary_modifiers(bind, key_event.keysym.mod)) {
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
        if (has_necessary_modifiers(command, key_event.keysym.mod)) {
          command.command();
        }
      }
    }
  }
}

MouseDevice::MouseDevice() {
  m_binds = DEFAULT_MOUSE_BINDS;
  // https://wiki.libsdl.org/SDL2/SDL_SetRelativeMouseMode
  // TODO - get it from from settings
  SDL_SetRelativeMouseMode(sdl_util::sdl_bool(m_enable_mouse_motion_controls));
}

void MouseDevice::process_event(const SDL_Event& event,
                                const CommandBindingGroups& commands,
                                std::shared_ptr<PadData> data) {
  // Mouse Button Events
  // https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
  if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
    const auto button_event = event.button;
    // Normal Buttons
    if (m_binds.buttons.find(button_event.button) != m_binds.buttons.end()) {
      for (const auto& bind : m_binds.buttons.at(button_event.button)) {
        if (has_necessary_modifiers(bind, SDL_GetModState())) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_MOUSEBUTTONDOWN;
        }
      }
    }
    // Analog Buttons (useless for keyboards)
    if (m_binds.button_axii.find(button_event.button) != m_binds.button_axii.end()) {
      for (const auto& bind : m_binds.button_axii.at(button_event.button)) {
        data->button_data.at(bind.pad_data_index) = event.type == SDL_MOUSEBUTTONDOWN;
      }
    }
    // Analog Sticks simulating
    if (m_binds.analog_axii.find(button_event.button) != m_binds.analog_axii.end()) {
      for (const auto& bind : m_binds.analog_axii.at(button_event.button)) {
        if (has_necessary_modifiers(bind, SDL_GetModState())) {
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
        if (has_necessary_modifiers(command, SDL_GetModState())) {
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
  // Update to latest controller DB file
  // TODO - load users controller settings
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
  m_keyboard = KeyboardDevice();
  m_mouse = MouseDevice();

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
      m_available_controllers.push_back(std::make_shared<GameController>(i));
      // By default, controller port mapping is on a first-come-first-served scenario
      // TODO - change this to use the saved JSON settings to remember what port should prefer a
      // controller
      m_controller_port_mapping[i] = i;
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
  // TODO - set and persist settings if changed
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
      m_keyboard.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port));
    }
    if (m_mouse_enabled) {
      m_mouse.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port));
    }
  }

  // Send event to active controller device
  // This goes last so it takes precedence
  for (const auto& [port, controller_idx] : m_controller_port_mapping) {
    if (m_data.find(port) != m_data.end() && m_available_controllers.size() > controller_idx) {
      m_available_controllers.at(controller_idx)
          ->process_event(event, m_command_binds, m_data.at(port));
    }
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

void InputManager::set_controller_for_port(const int controller_id, const int port) {
  // TODO - if changing, probably need to reset inputs
  // TODO - persist settings
}

void InputManager::enable_keyboard(const bool enabled) {
  // TODO - if disabling, probably need to reset inputs
  m_keyboard_enabled = enabled;
  // TODO - persist settings
}

void InputManager::enable_mouse(const bool enabled) {
  // TODO - if disabling, probably need to reset inputs
  m_mouse_enabled = enabled;
  // TODO - persist settings
}
