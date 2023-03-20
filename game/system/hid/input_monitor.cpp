/*!
 * @file newpad.cpp
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include "input_monitor.h"

#include <atomic>
#include <cmath>

#include "sdl_util.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"

// void SetAnalogAxisValue(MappingInfo& mapping_info, int axis, double value) {
//   const double sensitivity_numerator = Gfx::g_global_settings.target_fps;
//   const double minimum_sensitivity = 1e-4;
//
//   for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
//     for (int analog = 0; analog < (int)Analog::Max; ++analog) {
//       if (mapping_info.keyboard_analog_mapping[pad][analog].axis_id == axis) {
//         double newValue = value;
//         if (axis == GlfwKeyCustomAxis::CURSOR_X_AXIS) {
//           if (mapping_info.mouse_x_axis_sensitivities[pad] < minimum_sensitivity) {
//             mapping_info.mouse_x_axis_sensitivities[pad] = minimum_sensitivity;
//           }
//           newValue /= (sensitivity_numerator / mapping_info.mouse_x_axis_sensitivities[pad]);
//         } else if (axis == GlfwKeyCustomAxis::CURSOR_Y_AXIS) {
//           if (mapping_info.mouse_y_axis_sensitivities[pad] < minimum_sensitivity) {
//             mapping_info.mouse_y_axis_sensitivities[pad] = minimum_sensitivity;
//           }
//           newValue /= (sensitivity_numerator / mapping_info.mouse_y_axis_sensitivities[pad]);
//         }
//
//         if (newValue > 1.0) {
//           g_key_analogs[pad][analog] = 1.0;
//         } else if (newValue < -1.0) {
//           g_key_analogs[pad][analog] = -1.0;
//         } else if (std::isnan(newValue)) {
//           g_key_analogs[pad][analog] = 0.0;
//         } else {
//           g_key_analogs[pad][analog] = newValue;
//         }
//
//         // Invert logic used here. Left Y axis movement is based on towards the camera.
//         // In game forward is treated as going away from the camera and backwards is headed
//         towards
//         // the camera.
//         if (axis == GlfwKeyCustomAxis::CURSOR_Y_AXIS) {
//           g_key_analogs[pad][analog] *= -1;
//         }
//       }
//     }
//   }
// }

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
    lg::error("Could not get device name: {}", SDL_GetError());
    m_device_name = fmt::format("Unknown Device {}", sdl_device_id);
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
        // TODO - check for modifiers
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

void KeyboardDevice::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data) {
  if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
    const auto key_event = event.key;
    // TODO - handle modifiers
    // Normal Buttons
    if (m_binds.buttons.find(key_event.keysym.sym) != m_binds.buttons.end()) {
      for (const auto& bind : m_binds.buttons.at(key_event.keysym.sym)) {
        data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
      }
    }
    // Analog Buttons (useless for keyboards)
    if (m_binds.button_axii.find(key_event.keysym.sym) != m_binds.button_axii.end()) {
      for (const auto& bind : m_binds.button_axii.at(key_event.keysym.sym)) {
        data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
      }
    }
    // Analog Sticks simulating
    if (m_binds.analog_axii.find(key_event.keysym.sym) != m_binds.analog_axii.end()) {
      for (const auto& bind : m_binds.analog_axii.at(key_event.keysym.sym)) {
        int analog_val = event.type == SDL_KEYDOWN ? 255 : 127;
        if (event.type == SDL_KEYDOWN && bind.inverse_val) {
          analog_val = 0;
        }
        // TODO - fix these!
        data->analog_data.at(bind.pad_data_index) = analog_val;
      }
    }

    // Check for commands
    if (event.type == SDL_KEYDOWN &&
        commands.keyboard_binds.find(key_event.keysym.sym) != commands.keyboard_binds.end()) {
      for (const auto& command : commands.keyboard_binds.at(key_event.keysym.sym)) {
        // TODO - check for modifiers
        command.command();
      }
    }
  }
}

void MouseDevice::process_event(const SDL_Event& event,
                                const CommandBindingGroups& commands,
                                std::shared_ptr<PadData> data) {
  // Mouse Button Events
  // https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
  if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
    const auto button_event = event.button;
    // TODO - handle modifiers
    // Normal Buttons
    if (m_binds.buttons.find(button_event.button) != m_binds.buttons.end()) {
      for (const auto& bind : m_binds.buttons.at(button_event.button)) {
        data->button_data.at(bind.pad_data_index) = event.type == SDL_MOUSEBUTTONDOWN;
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
        int analog_val = event.type == SDL_MOUSEBUTTONDOWN ? 255 : 127;
        if (event.type == SDL_MOUSEBUTTONDOWN && bind.inverse_val) {
          analog_val = 0;
        }
        data->analog_data.at(bind.pad_data_index) = analog_val;
      }
    }

    // Check for commands
    if (event.type == SDL_MOUSEBUTTONDOWN &&
        commands.mouse_binds.find(button_event.button) != commands.mouse_binds.end()) {
      for (const auto& command : commands.mouse_binds.at(button_event.button)) {
        // TODO - check for modifiers
        command.command();
      }
    }
  } else if (event.type == SDL_MOUSEMOTION) {
    // https://wiki.libsdl.org/SDL2/SDL_MouseMotionEvent
    // TODO - https://wiki.libsdl.org/SDL2/SDL_SetRelativeMouseMode
    m_xcoord = event.motion.x;
    m_ycoord = event.motion.y;
  }
}

InputMonitor::InputMonitor() {
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
}

InputMonitor::~InputMonitor() {
  for (auto& device : m_available_controllers) {
    device->close_device();
  }
}

void InputMonitor::refresh_device_list() {
  m_available_controllers.clear();
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

void InputMonitor::process_sdl_event(const SDL_Event& event) {
  // Detect controller connections and disconnects
  // TODO - do we care about remap events?
  if (sdl_util::is_any_event_type(event.type,
                                  {SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED})) {
    lg::info("Controller added or removed. refreshing controller device list");
    refresh_device_list();
  }

  if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
    m_keyboard.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port));
    m_mouse.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port));
  }

  // Send event to active controller device
  // This goes last so it takes precedence
  for (const auto& [port, controller_idx] : m_controller_port_mapping) {
    if (m_data.find(port) != m_data.end()) {
      m_available_controllers.at(controller_idx)
          ->process_event(event, m_command_binds, m_data.at(port));
    }
  }
}

std::optional<std::shared_ptr<PadData>> InputMonitor::get_current_data(const int port) const {
  if (m_data.find(port) == m_data.end()) {
    return {};
  }
  return m_data.at(port);
}

int InputMonitor::update_rumble(int port, u8 low_intensity, u8 high_intensity) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return 0;
  }
  return m_available_controllers.at(m_controller_port_mapping.at(port))
      ->update_rumble(low_intensity, high_intensity);
}

void InputMonitor::register_command(const CommandBinding::Source source,
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
