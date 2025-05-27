#include "game_controller.h"

#include <optional>

#include "dualsense_effects.h"
#include "game_controller.h"

#include "game/system/hid/sdl_util.h"

#include "fmt/format.h"

GameController::GameController(int sdl_device_id,
                               std::shared_ptr<game_settings::InputSettings> settings)
    : m_sdl_instance_id(sdl_device_id) {
  m_settings = settings;
  m_loaded = false;
  m_device_handle = SDL_OpenGamepad(sdl_device_id);
  if (!m_device_handle) {
    sdl_util::log_error(
        fmt::format("Could not open game controller with device id: {}", sdl_device_id));
    return;
  }

  m_low_device_handle = SDL_GetGamepadJoystick(m_device_handle);
  if (!m_low_device_handle) {
    sdl_util::log_error(
        fmt::format("Could not get underlying joystick for gamecontroller: id {}", sdl_device_id));
    return;
  }
  auto test = SDL_GetNumJoystickAxes(m_low_device_handle);
  m_sdl_instance_id = SDL_GetJoystickID(m_low_device_handle);
  if (!m_sdl_instance_id) {
    sdl_util::log_error(
        fmt::format("Could not determine instance id for gamecontroller: id {}", sdl_device_id));
    return;
  }
  const auto controller_guid = SDL_GetJoystickGUID(m_low_device_handle);
  if (sdl_util::is_SDL_GUID_zero(controller_guid)) {
    sdl_util::log_error(fmt::format("Could not determine controller guid: id {}", sdl_device_id));
    return;
  }
  char guidStr[33];
  SDL_GUIDToString(controller_guid, guidStr, sizeof(guidStr));
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
  auto name = SDL_GetGamepadName(m_device_handle);
  if (!name) {
    sdl_util::log_error(fmt::format("Could not get device name with id: {}", sdl_device_id));
    m_device_name = "";
  } else {
    m_device_name = name;
  }
  auto properties = SDL_GetGamepadProperties(m_device_handle);
  if (!properties) {
    sdl_util::log_error(
        fmt::format("Unable to retrieve properties for gamepad: id {}", sdl_device_id));
    m_has_led = false;
    m_has_rumble = false;
    m_has_trigger_rumble = false;
    m_is_dualsense = false;
  } else {
    m_has_led = SDL_GetBooleanProperty(properties, SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN, false);
    m_has_rumble = SDL_GetBooleanProperty(properties, SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN, false);
    m_has_trigger_rumble =
        SDL_GetBooleanProperty(properties, SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN, false);
    m_is_dualsense = SDL_GetGamepadType(m_device_handle) == SDL_GAMEPAD_TYPE_PS5;
    clear_trigger_effect(dualsense_effects::TriggerEffectOption::BOTH);
  }

  if (m_settings->controller_binds.find(m_guid) == m_settings->controller_binds.end()) {
    m_settings->controller_binds[m_guid] = DEFAULT_CONTROLLER_BINDS;
  }

  const auto num_axes = SDL_GetNumJoystickAxes(m_low_device_handle);
  m_has_pressure_sensitive_buttons =
      SDL_GetGamepadType(m_device_handle) == SDL_GAMEPAD_TYPE_PS3 && num_axes == 16;

  if (m_has_pressure_sensitive_buttons) {
    lg::info("Detected a PS3 controller with support for pressure sensitive buttons");
  }
  m_loaded = true;
}

static std::unordered_map<int, int> button_to_pressure_axes = {
    {SDL_GAMEPAD_BUTTON_SOUTH, 6},          {SDL_GAMEPAD_BUTTON_EAST, 7},
    {SDL_GAMEPAD_BUTTON_WEST, 8},           {SDL_GAMEPAD_BUTTON_NORTH, 9},
    {SDL_GAMEPAD_BUTTON_LEFT_SHOULDER, 10}, {SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER, 11},
    {SDL_GAMEPAD_BUTTON_DPAD_UP, 12},       {SDL_GAMEPAD_BUTTON_DPAD_DOWN, 13},
    {SDL_GAMEPAD_BUTTON_DPAD_LEFT, 14},     {SDL_GAMEPAD_BUTTON_DPAD_RIGHT, 15}};

static std::unordered_map<int, int> pressure_axes_to_button = {
    {6, SDL_GAMEPAD_BUTTON_SOUTH},          {7, SDL_GAMEPAD_BUTTON_EAST},
    {8, SDL_GAMEPAD_BUTTON_WEST},           {9, SDL_GAMEPAD_BUTTON_NORTH},
    {10, SDL_GAMEPAD_BUTTON_LEFT_SHOULDER}, {11, SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER},
    {12, SDL_GAMEPAD_BUTTON_DPAD_UP},       {13, SDL_GAMEPAD_BUTTON_DPAD_DOWN},
    {14, SDL_GAMEPAD_BUTTON_DPAD_LEFT},     {15, SDL_GAMEPAD_BUTTON_DPAD_RIGHT}};

// Adjust the value range to 0-255 (127 being neutral)
// Values come out of SDL as -32,768 to 32,767
int normalize_axes_value(int sdl_val) {
  return ((sdl_val + 32768) * 256) / 65536;
}

void GameController::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data,
                                   std::optional<InputBindAssignmentMeta>& bind_assignment) {
  if (event.type == SDL_EVENT_GAMEPAD_AXIS_MOTION && event.gaxis.which == m_sdl_instance_id) {
    // https://wiki.libsdl.org/SDL3/SDL_GamepadAxis
    if ((int)event.gaxis.axis <= SDL_GAMEPAD_AXIS_INVALID ||
        event.gaxis.axis >= SDL_GAMEPAD_AXIS_COUNT) {
      return;
    }

    auto& binds = m_settings->controller_binds.at(m_guid);

    // Handle analog stick binds
    if (event.gaxis.axis >= SDL_GAMEPAD_AXIS_LEFTX && event.gaxis.axis <= SDL_GAMEPAD_AXIS_RIGHTY &&
        !data->analogs_being_simulated() &&
        binds.analog_axii.find(event.gaxis.axis) != binds.analog_axii.end()) {
      for (const auto& bind : binds.analog_axii.at(event.gaxis.axis)) {
        data->analog_data.at(bind.pad_data_index) =
            normalize_axes_value(m_settings->axis_scale * event.gaxis.value);
      }
    } else if (event.gaxis.axis >= SDL_GAMEPAD_AXIS_LEFT_TRIGGER &&
               event.gaxis.axis <= SDL_GAMEPAD_AXIS_RIGHT_TRIGGER &&
               binds.button_axii.find(event.gaxis.axis) != binds.button_axii.end()) {
      // Binding re-assignment
      if (bind_assignment) {
        // In the event that the user binds an analog input to the confirm binds
        // (ie. a trigger on X) we wait until it hits a neutral position (0)
        // before proceeding to rebind.
        //
        // TODO - this still isn't perfect as the data mutation below will trigger it after the bind
        // assignment has been set but atleast it's in a mostly working state right now
        if (!bind_assignment->seen_controller_confirm_neutral) {
          for (const auto& confirm_bind : bind_assignment->controller_confirmation_binds) {
            if (confirm_bind.sdl_idx == event.gaxis.axis) {
              if (event.gaxis.value <= 0) {
                bind_assignment->seen_controller_confirm_neutral = true;
              }
              return;
            }
          }
        }

        if (bind_assignment->device_type == InputDeviceType::CONTROLLER &&
            !bind_assignment->for_analog) {
          binds.assign_button_bind(event.gaxis.axis, bind_assignment.value(), true);
        }
        return;
      }

      // and analog triggers
      for (const auto& bind : binds.button_axii.at(event.gaxis.axis)) {
        data->button_data.at(bind.pad_data_index) = event.gaxis.value > 0;
        const auto pressure_index = data->button_index_to_pressure_index(
            static_cast<PadData::ButtonIndex>(bind.pad_data_index));
        if (pressure_index != PadData::PressureIndex::INVALID_PRESSURE) {
          if (m_settings->enable_pressure_sensitivity && m_has_pressure_sensitive_buttons) {
            data->pressure_data.at(pressure_index) =
                normalize_axes_value(m_settings->pressure_scale * event.gaxis.value);
          } else {
            data->pressure_data.at(pressure_index) = event.gaxis.value > 0 ? 255 : 0;
          }
        }
      }
    }
  } else if ((event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN ||
              event.type == SDL_EVENT_GAMEPAD_BUTTON_UP) &&
             event.gbutton.which == m_sdl_instance_id) {
    auto& binds = m_settings->controller_binds.at(m_guid);

    // https://wiki.libsdl.org/SDL3/SDL_GamepadButton
    if ((int)event.gbutton.button <= SDL_GAMEPAD_BUTTON_INVALID ||
        event.gbutton.button >= SDL_GAMEPAD_BUTTON_COUNT) {
      return;
    }

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN) {
      if (bind_assignment->device_type == InputDeviceType::CONTROLLER &&
          !bind_assignment->for_analog) {
        binds.assign_button_bind(event.gbutton.button, bind_assignment.value());
      }
      return;
    }

    if (binds.buttons.find(event.gbutton.button) == binds.buttons.end()) {
      return;
    }

    // Iterate the binds, and apply all of them
    for (const auto& bind : binds.buttons.at(event.gbutton.button)) {
      // 1. pressed flag
      data->button_data.at(bind.pad_data_index) = event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN;
      // 2. potentially get the pressure value as well
      const auto pressure_index = data->button_index_to_pressure_index(
          static_cast<PadData::ButtonIndex>(bind.pad_data_index));
      if (pressure_index != PadData::PressureIndex::INVALID_PRESSURE) {
        if (m_settings->enable_pressure_sensitivity && m_has_pressure_sensitive_buttons &&
            button_to_pressure_axes.contains(event.gbutton.button)) {
          // TODO - handle error
          const auto pressure_val = SDL_GetJoystickAxis(
              m_low_device_handle, button_to_pressure_axes.at(event.gbutton.button));
          data->pressure_data.at(pressure_index) = normalize_axes_value(pressure_val);
        } else {
          data->pressure_data.at(pressure_index) =
              event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN ? 255 : 0;
        }
      }
    }

    // Check for commands
    if (event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN &&
        commands.controller_binds.find(event.gbutton.button) != commands.controller_binds.end()) {
      for (const auto& command : commands.controller_binds.at(event.gbutton.button)) {
        if (command.event_command) {
          command.event_command(event);
        } else if (command.command) {
          command.command();
        } else {
          lg::warn("CommandBinding has no valid callback for controller bind");
        }
      }
    }
  } else if (SDL_EVENT_JOYSTICK_AXIS_MOTION && m_has_pressure_sensitive_buttons) {
    // handle changes in pressure on axii not mapped to the gamepad (ie. PS3 analog buttons)
    if (m_settings->enable_pressure_sensitivity &&
        pressure_axes_to_button.contains(event.jaxis.axis)) {
      auto& binds = m_settings->controller_binds.at(m_guid);
      // Work backwords, get the button so we can iterate the binds
      const auto sdl_button_index = pressure_axes_to_button.at(event.jaxis.axis);
      for (const auto& bind : binds.buttons.at(sdl_button_index)) {
        const auto pressure_index = data->button_index_to_pressure_index(
            static_cast<PadData::ButtonIndex>(bind.pad_data_index));
        if (pressure_index != PadData::PressureIndex::INVALID_PRESSURE) {
          data->pressure_data.at(pressure_index) = normalize_axes_value(event.jaxis.value);
        }
      }
    }
  }
}

void GameController::close_device() {
  if (m_device_handle) {
    clear_trigger_effect(dualsense_effects::TriggerEffectOption::BOTH);
    SDL_CloseGamepad(m_device_handle);
    m_device_handle = NULL;
  }
}

int GameController::send_rumble(const u8 low_rumble, const u8 high_rumble) {
  if (m_has_rumble && m_device_handle) {
    // https://wiki.libsdl.org/SDL3/SDL_RumbleGamepad
    // Arbitrary duration, since it's called every frame anyway, just so the vibration doesn't last
    // forever. SDL expects a value in the range of 0-0xFFFF, so the `257` is just normalizing the
    // 0-255 we get to that range
    //
    // NOTE - the 100ms is arbitrary, not sure how long the vibration lasted on the original ps2?
    if (!SDL_RumbleGamepad(m_device_handle, low_rumble * 257, high_rumble * 257, 100)) {
      return 1;
    }
  }
  return 0;
}

void GameController::send_trigger_rumble(const u16 left_rumble,
                                         const u16 right_rumble,
                                         const u32 duration_ms) {
  if (m_has_trigger_rumble && m_device_handle) {
    // https://wiki.libsdl.org/SDL3/SDL_RumbleGamepadTriggers
    if (!SDL_RumbleGamepadTriggers(m_device_handle, left_rumble, right_rumble, duration_ms)) {
      sdl_util::log_error("Unable to rumble gamepad's triggers!");
    }
  }
}

dualsense_effects::DS5EffectsState_t prepare_effects_struct(
    const dualsense_effects::TriggerEffectOption option,
    const std::array<u8, 11>& effect) {
  dualsense_effects::DS5EffectsState_t effects;
  SDL_zero(effects);
  // Modify right (0x04) and left (0x08) trigger effect
  if (option == dualsense_effects::TriggerEffectOption::LEFT ||
      option == dualsense_effects::TriggerEffectOption::BOTH) {
    effects.ucEnableBits1 |= 0x08;
    SDL_memcpy(effects.rgucLeftTriggerEffect, effect.data(), sizeof(effect));
  }
  if (option == dualsense_effects::TriggerEffectOption::RIGHT ||
      option == dualsense_effects::TriggerEffectOption::BOTH) {
    effects.ucEnableBits1 |= 0x04;
    SDL_memcpy(effects.rgucRightTriggerEffect, effect.data(), sizeof(effect));
  }
  return effects;
}

void GameController::clear_trigger_effect(dualsense_effects::TriggerEffectOption option) {
  if (m_is_dualsense && m_device_handle) {
    const auto trigger_effect = dualsense_effects::trigger_effect_off();
    const auto effects = prepare_effects_struct(option, trigger_effect);
    if (!SDL_SendGamepadEffect(m_device_handle, &effects, sizeof(effects))) {
      sdl_util::log_error("Unable to clear dualsense trigger effect!");
    }
  }
}

void GameController::send_trigger_effect_feedback(dualsense_effects::TriggerEffectOption option,
                                                  u8 position,
                                                  u8 strength) {
  if (m_is_dualsense && m_device_handle) {
    const auto trigger_effect = dualsense_effects::trigger_effect_feedback(position, strength);
    const auto effects = prepare_effects_struct(option, trigger_effect);
    if (!SDL_SendGamepadEffect(m_device_handle, &effects, sizeof(effects))) {
      sdl_util::log_error("Unable to send dualsense feedback trigger effect!");
    }
  }
}

void GameController::send_trigger_effect_vibrate(dualsense_effects::TriggerEffectOption option,
                                                 u8 position,
                                                 u8 amplitude,
                                                 u8 frequency) {
  if (m_is_dualsense && m_device_handle) {
    const auto trigger_effect =
        dualsense_effects::trigger_effect_vibration(position, amplitude, frequency);
    const auto effects = prepare_effects_struct(option, trigger_effect);
    if (!SDL_SendGamepadEffect(m_device_handle, &effects, sizeof(effects))) {
      sdl_util::log_error("Unable to send dualsense vibrate trigger effect!");
    }
  }
}

void GameController::send_trigger_effect_weapon(dualsense_effects::TriggerEffectOption option,
                                                u8 start_position,
                                                u8 end_position,
                                                u8 strength) {
  if (m_is_dualsense && m_device_handle) {
    const auto trigger_effect =
        dualsense_effects::trigger_effect_weapon(start_position, end_position, strength);
    const auto effects = prepare_effects_struct(option, trigger_effect);
    if (!SDL_SendGamepadEffect(m_device_handle, &effects, sizeof(effects))) {
      sdl_util::log_error("Unable to send dualsense weapon trigger effect!");
    }
  }
}

void GameController::set_led(const u8 red, const u8 green, const u8 blue) {
  if (!m_has_led) {
    return;
  }
  if (m_device_handle) {
    if (!SDL_SetGamepadLED(m_device_handle, red, green, blue)) {
      m_has_led = false;
    }
  }
}
