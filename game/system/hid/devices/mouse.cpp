#include "mouse.h"

#include "game/system/hid/sdl_util.h"

MouseDevice::MouseDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
  enable_relative_mode(m_control_camera);
}

// I don't trust SDL's key repeat stuff, do it myself to avoid bug reports...(or cause more)
bool MouseDevice::is_action_already_active(const u32 sdl_code, const bool player_movement) {
  for (const auto& action : m_active_actions) {
    if (!player_movement && action.sdl_mouse_button == sdl_code) {
      return true;
    } else if (player_movement && action.player_movement) {
      return true;
    }
  }
  return false;
}

void MouseDevice::poll_state(std::shared_ptr<PadData> data) {
  auto& binds = m_settings->mouse_binds;
  const auto mouse_state = SDL_GetMouseState(NULL, NULL);
  const auto keyboard_modifier_state = SDL_GetModState();

  // Iterate binds, see if there are any new actions we need to track
  // - Normal Buttons
  for (const auto& [sdl_code, bind_list] : binds.buttons) {
    for (const auto& bind : bind_list) {
      if (mouse_state & SDL_BUTTON(sdl_code) &&
          bind.modifiers.has_necessary_modifiers(keyboard_modifier_state) &&
          !is_action_already_active(sdl_code, false)) {
        data->button_data.at(bind.pad_data_index) = true;  // press the button
        m_active_actions.push_back(
            {sdl_code, bind, false, [](std::shared_ptr<PadData> data, InputBinding bind) {
               data->button_data.at(bind.pad_data_index) = false;  // let go of the button
             }});
      }
    }
  }
  // - Analog Buttons (useless for keyboards, but here for completeness)
  for (const auto& [sdl_code, bind_list] : binds.button_axii) {
    for (const auto& bind : bind_list) {
      if (mouse_state & SDL_BUTTON(sdl_code) &&
          bind.modifiers.has_necessary_modifiers(keyboard_modifier_state) &&
          !is_action_already_active(sdl_code, false)) {
        data->button_data.at(bind.pad_data_index) = true;  // press the button
        m_active_actions.push_back(
            {sdl_code, bind, false, [](std::shared_ptr<PadData> data, InputBinding bind) {
               data->button_data.at(bind.pad_data_index) = false;  // let go of the button
             }});
      }
    }
  }
  // No analog stick simulation, not support via the mouse instead we allow for only this:
  // WoW style mouse movement, if you have both buttons held down, you will move forward
  if (m_control_movement && !is_action_already_active(0, true) &&
      (mouse_state & SDL_BUTTON_LMASK && mouse_state & SDL_BUTTON_RMASK)) {
    data->analog_data.at(1) += -127;  // move forward
    data->update_analog_sim_tracker(false);
    ActiveMouseAction action;
    action.player_movement = true;
    action.revert_action = [](std::shared_ptr<PadData> data, InputBinding /*bind*/) {
      data->analog_data.at(1) += 127;  // stop moving forward
      data->update_analog_sim_tracker(true);
    };
    m_active_actions.push_back(action);
  }

  // Check if any previously tracked actions are now invalidated by the new state of the keyboard
  // if so, we'll run their revert code and remove them
  for (auto it = m_active_actions.begin(); it != m_active_actions.end();) {
    // Modifiers are easy, if the action required one and it's not pressed anymore, evict it
    // Alternatively, was the primary key released
    // Alternatively, alternatively the special case'd mouse movement
    if (it->player_movement) {
      if (!(mouse_state & SDL_BUTTON_LMASK) || !(mouse_state & SDL_BUTTON_RMASK)) {
        it->revert_action(data, it->binding);
        it = m_active_actions.erase(it);
      } else {
        it++;
      }
    } else {
      if (!(mouse_state & SDL_BUTTON(it->sdl_mouse_button)) ||
          !it->binding.modifiers.has_necessary_modifiers(keyboard_modifier_state)) {
        it->revert_action(data, it->binding);
        it = m_active_actions.erase(it);
      } else {
        it++;
      }
    }
  }
}

void MouseDevice::clear_actions(std::shared_ptr<PadData> data) {
  for (auto it = m_active_actions.begin(); it != m_active_actions.end();) {
    it->revert_action(data, it->binding);
    it = m_active_actions.erase(it);
  }
}

void MouseDevice::process_event(const SDL_Event& event,
                                const CommandBindingGroups& commands,
                                std::shared_ptr<PadData> data,
                                std::optional<InputBindAssignmentMeta>& bind_assignment) {
  // We still want to keep track of the cursor location even if we aren't using it for inputs
  // return early
  if (event.type == SDL_MOUSEMOTION) {
    // https://wiki.libsdl.org/SDL2/SDL_MouseMotionEvent
    m_xcoord = event.motion.x;
    m_ycoord = event.motion.y;
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
    // Update the internal mouse tracking, this is for GOAL reasons.
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

    auto& binds = m_settings->mouse_binds;

    // Binding re-assignment
    if (bind_assignment && event.type == SDL_MOUSEBUTTONDOWN) {
      if (bind_assignment->device_type == InputDeviceType::MOUSE && !bind_assignment->for_analog) {
        binds.assign_button_bind(button_event.button, bind_assignment.value(), false,
                                 InputModifiers(SDL_GetModState()));
      }
      return;
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
