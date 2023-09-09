#include "game_controller.h"

#include "game/system/hid/sdl_util.h"

#include "third-party/fmt/core.h"

GameController::GameController(int sdl_device_id,
                               std::shared_ptr<game_settings::InputSettings> settings)
    : m_sdl_instance_id(sdl_device_id) {
  m_settings = settings;
  m_loaded = false;
  m_device_handle = SDL_GameControllerOpen(sdl_device_id);
  if (!m_device_handle) {
    sdl_util::log_error(
        fmt::format("Could not open game controller with device id: {}", sdl_device_id));
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
        fmt::format("Could not determine instance id for gamecontroller: id {}", sdl_device_id));
    return;
  }
  const auto controller_guid = SDL_JoystickGetGUID(joystick);
  if (sdl_util::is_SDL_GUID_zero(controller_guid)) {
    sdl_util::log_error(fmt::format("Could not determine controller guid: id {}", sdl_device_id));
    return;
  }
  char guidStr[33];
  SDL_JoystickGetGUIDString(controller_guid, guidStr, sizeof(guidStr));
  m_guid = guidStr;

  // NOTE - it has been observed that this function will return `NULL` which indicates a bad
  // controller this seems to happen if you disconnect the controller while it's in the process of
  // connecting
  //
  // However, SDL_GameControllerGetAttached returns true, and `NULL` here is not an outright failure
  // there could be controllers that just have no name.
  //
  // So I'm letting this one go through, in most cases it had an invalid guid as well (so caught
  // above).
  auto name = SDL_GameControllerName(m_device_handle);
  if (!name) {
    sdl_util::log_error(fmt::format("Could not get device name with id: {}", sdl_device_id));
    m_device_name = "";
  } else {
    m_device_name = name;
  }
  m_has_led = sdl_util::from_sdl_bool(SDL_GameControllerHasLED(m_device_handle));
  m_has_rumble = sdl_util::from_sdl_bool(SDL_GameControllerHasRumble(m_device_handle));

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
    if ((int)event.caxis.axis <= SDL_CONTROLLER_AXIS_INVALID ||
        event.caxis.axis >= SDL_CONTROLLER_AXIS_MAX) {
      return;
    }

    auto& binds = m_settings->controller_binds.at(m_guid);

    // Handle analog stick binds
    if (event.caxis.axis >= SDL_CONTROLLER_AXIS_LEFTX &&
        event.caxis.axis <= SDL_CONTROLLER_AXIS_RIGHTY && !data->analogs_being_simulated() &&
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
        data->button_data.at(bind.pad_data_index) = event.caxis.value > 0;
      }
    }
  } else if ((event.type == SDL_CONTROLLERBUTTONDOWN || event.type == SDL_CONTROLLERBUTTONUP) &&
             event.cbutton.which == m_sdl_instance_id) {
    auto& binds = m_settings->controller_binds.at(m_guid);

    // https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
    if ((int)event.cbutton.button <= SDL_CONTROLLER_BUTTON_INVALID ||
        event.cbutton.button >= SDL_CONTROLLER_BUTTON_MAX) {
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

    if (binds.buttons.find(event.cbutton.button) == binds.buttons.end()) {
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
  if (m_has_rumble && m_device_handle) {
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
  if (m_device_handle) {
    auto ok = SDL_GameControllerSetLED(m_device_handle, red, green, blue);
    if (ok != 0) {
      m_has_led = false;
    }
  }
}
