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

GameController::GameController(int sdl_device_id, int dead_zone)
    : m_sdl_instance_id(sdl_device_id), m_analog_dead_zone(dead_zone) {
  // TODO - load user binds
  m_binds = s_default_controller_binds;
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

void GameController::process_event(const SDL_Event& event, std::shared_ptr<PadData> data) {
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
        // TODO - deadzone stuff
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
  }
}

void GameController::close_device() {
  if (m_device_handle) {
    SDL_GameControllerClose(m_device_handle);
  }
}

int GameController::update_rumble(const int port, const u8 low_rumble, const u8 high_rumble) {
  // TODO - port!
  if (port != 0) {
    return 0;
  }
  if (m_device_handle) {
    // https://wiki.libsdl.org/SDL2/SDL_GameControllerRumble
    // TODO - not sure about duration here, i assume the next frame will cut it off if needed so
    // overshooting the duration is fine and the better way to go
    if (SDL_GameControllerRumble(m_device_handle, low_rumble * 257, high_rumble * 257, 1000) == 0) {
      return 1;
    }
  }
  return 0;
}

void KeyboardDevice::process_event(const SDL_Event& event, std::shared_ptr<PadData> data) {
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
        data->analog_data.at(bind.pad_data_index) = analog_val;
      }
    }
  }
}

void MouseDevice::process_event(const SDL_Event& event, std::shared_ptr<PadData> data) {
  // TODO - position
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
  }
}

InputMonitor::InputMonitor() {
  // Update to latest controller DB file
  std::string mapping_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "sdl_controller_db.txt").string();
  if (file_util::file_exists(mapping_path)) {
    SDL_GameControllerAddMappingsFromFile(mapping_path.c_str());
  } else {
    lg::error("Could not find SDL Controller DB at path `{}`", mapping_path);
  }
  m_data = std::make_shared<PadData>();
  m_keyboard = KeyboardDevice();
  m_mouse = MouseDevice();
  refresh_device_list();
}

InputMonitor::~InputMonitor() {
  for (auto& device : m_available_devices) {
    device->close_device();
  }
}

void InputMonitor::refresh_device_list() {
  m_available_devices.clear();
  // Enumerate devices
  if (SDL_NumJoysticks() > 0) {
    for (int i = 0; i < SDL_NumJoysticks(); i++) {
      if (!SDL_IsGameController(i)) {
        lg::error("Controller with device id {} is not avaiable via the GameController API", i);
        continue;
      }
      m_available_devices.push_back(std::make_shared<GameController>(i, 0));
      // By default, make the first controller we load the active one
      if (!m_active_device && m_available_devices.at(i)->is_loaded()) {
        m_active_device = m_available_devices.at(i);
      }
    }
  }
  // TODO - mouse one as well (if available?)
  if (!m_active_device) {
    lg::warn(
        "No active input device could be found or loaded successfully - inputs will not work!");
  }
}

void InputMonitor::process_sdl_event(const SDL_Event& event) {
  // Detect controller connections and disconnects
  // TODO - do we care about remap events?
  if (sdl_util::is_any_event_type(event.type,
                                  {SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED})) {
    refresh_device_list();
  }

  m_keyboard.process_event(event, m_data);
  m_mouse.process_event(event, m_data);

  // Send event to active controller device
  // This goes last so it takes precedence
  if (m_active_device) {
    m_active_device->process_event(event, m_data);
  }
}

std::shared_ptr<PadData> InputMonitor::get_current_data() const {
  return m_data;
}

int InputMonitor::update_rumble(int port, u8 low_intensity, u8 high_intensity) {
  // TODO - port
  if (m_active_device) {
    return m_active_device->update_rumble(port, low_intensity, high_intensity);
  }
  return 0;
}
