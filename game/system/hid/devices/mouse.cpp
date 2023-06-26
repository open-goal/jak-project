#include "mouse.h"

#include "game/system/hid/sdl_util.h"

MouseDevice::MouseDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
  enable_relative_mode(m_control_camera);
}

void MouseDevice::process_event(const SDL_Event& event,
                                const CommandBindingGroups& commands,
                                std::shared_ptr<PadData> data,
                                std::optional<InputBindAssignmentMeta>& bind_assignment,
                                bool ignore_inputs) {
  if (event.type == SDL_MOUSEMOTION) {
    // https://wiki.libsdl.org/SDL2/SDL_MouseMotionEvent
    m_xcoord = event.motion.x;
    m_ycoord = event.motion.y;
    if (ignore_inputs) {
      // We still want to keep track of the cursor location even if we aren't using it for inputs
      // return early
      return;
    }
    if (m_control_camera) {
      const auto xadjust = std::clamp(127 + int(float(event.motion.xrel) * m_xsens), 0, 255);
      const auto yadjust = std::clamp(127 + int(float(event.motion.yrel) * m_ysens), 0, 255);
      data->analog_data.at(2) = xadjust;
      data->analog_data.at(3) = yadjust;
    }
  } else if (event.type == SDL_MOUSEBUTTONDOWN || event.type == SDL_MOUSEBUTTONUP) {
    // Mouse Button Events
    // https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
    const auto button_event = event.button;
    // Always update the internal button tracker, this is for GOAL reasons.
    switch (button_event.button) {
      case SDL_BUTTON_LEFT:
        m_button_status.left = event.type == SDL_MOUSEBUTTONDOWN;
        break;
      case SDL_BUTTON_RIGHT:
        m_button_status.right = event.type == SDL_MOUSEBUTTONDOWN;
        break;
      case SDL_BUTTON_MIDDLE:
        m_button_status.middle = event.type == SDL_MOUSEBUTTONDOWN;
        break;
      case SDL_BUTTON_X1:
        m_button_status.mouse4 = event.type == SDL_MOUSEBUTTONDOWN;
        break;
      case SDL_BUTTON_X2:
        m_button_status.mouse5 = event.type == SDL_MOUSEBUTTONDOWN;
        break;
    }

    if (ignore_inputs) {
      return;
    }

    auto& binds = m_settings->mouse_binds;

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_MOUSEBUTTONDOWN) {
      if (bind_assignment->device_type == InputDeviceType::MOUSE && !bind_assignment->for_analog) {
        binds.assign_button_bind(button_event.button, bind_assignment.value(), false,
                                 InputModifiers(SDL_GetModState()));
      }
      return;
    }

    // Normal Buttons
    if (binds.buttons.find(button_event.button) != binds.buttons.end()) {
      for (const auto& bind : binds.buttons.at(button_event.button)) {
        if (bind.modifiers.has_necessary_modifiers(SDL_GetModState())) {
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
        if (bind.modifiers.has_necessary_modifiers(SDL_GetModState())) {
          int analog_val = event.type == SDL_MOUSEBUTTONDOWN ? 255 : 127;
          if (event.type == SDL_MOUSEBUTTONDOWN && bind.minimum_in_range) {
            analog_val = 0;
          }
          data->analog_data.at(bind.pad_data_index) = analog_val;
        }
      }
    }

    if (m_control_movement) {
      // WoW style mouse movement, if you have both buttons held down, you will move forward
      const auto mouse_state = SDL_GetMouseState(NULL, NULL);
      if (!m_was_moving_with_mouse && event.type == SDL_MOUSEBUTTONDOWN &&
          (mouse_state & SDL_BUTTON_LMASK && mouse_state & SDL_BUTTON_RMASK)) {
        data->analog_data.at(1) += -127;
        m_was_moving_with_mouse = true;
        data->update_analog_sim_tracker(false);
      } else if (m_was_moving_with_mouse && event.type == SDL_MOUSEBUTTONUP &&
                 ((mouse_state & SDL_BUTTON_LMASK) == 0 || (mouse_state & SDL_BUTTON_RMASK) == 0)) {
        data->analog_data.at(1) += 127;
        m_was_moving_with_mouse = false;
        data->update_analog_sim_tracker(true);
      }
    }

    // Check for commands
    if (event.type == SDL_MOUSEBUTTONDOWN &&
        commands.mouse_binds.find(button_event.button) != commands.mouse_binds.end()) {
      for (const auto& command : commands.mouse_binds.at(button_event.button)) {
        if (command.modifiers.has_necessary_modifiers(SDL_GetModState())) {
          command.command();
        }
      }
    }
  }
}

void MouseDevice::enable_relative_mode(const bool enable) {
  // https://wiki.libsdl.org/SDL2/SDL_SetRelativeMouseMode
  SDL_SetRelativeMouseMode(sdl_util::sdl_bool(enable));
}

void MouseDevice::enable_camera_control(const bool enable) {
  m_control_camera = enable;
  enable_relative_mode(m_control_camera);
}

void MouseDevice::set_camera_sens(const float xsens, const float ysens) {
  m_xsens = xsens;
  m_ysens = ysens;
}
