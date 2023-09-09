#include "keyboard.h"

#include "game/system/hid/sdl_util.h"

KeyboardDevice::KeyboardDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
}

// I don't trust SDL's key repeat stuff, do it myself to avoid bug reports...(or cause more)
bool KeyboardDevice::is_action_already_active(const u32 sdl_keycode) {
  for (const auto& action : m_active_actions) {
    if (action.sdl_keycode == sdl_keycode) {
      return true;
    }
  }
  return false;
}

void KeyboardDevice::poll_state(std::shared_ptr<PadData> data) {
  auto& binds = m_settings->keyboard_binds;
  const auto keyboard_state = SDL_GetKeyboardState(NULL);
  const auto keyboard_modifier_state = SDL_GetModState();

  // Iterate binds, see if there are any new actions we need to track
  // - Normal Buttons
  for (const auto& [sdl_keycode, bind_list] : binds.buttons) {
    for (const auto& bind : bind_list) {
      if (keyboard_state[SDL_GetScancodeFromKey(sdl_keycode)] &&
          bind.modifiers.has_necessary_modifiers(keyboard_modifier_state) &&
          !is_action_already_active(sdl_keycode)) {
        data->button_data.at(bind.pad_data_index) = true;  // press the button
        m_active_actions.push_back(
            {sdl_keycode, bind, [](std::shared_ptr<PadData> data, InputBinding bind) {
               data->button_data.at(bind.pad_data_index) = false;  // let go of the button
             }});
      }
    }
  }
  // - Analog Buttons
  for (const auto& [sdl_keycode, bind_list] : binds.button_axii) {
    for (const auto& bind : bind_list) {
      if (keyboard_state[SDL_GetScancodeFromKey(sdl_keycode)] &&
          bind.modifiers.has_necessary_modifiers(keyboard_modifier_state) &&
          !is_action_already_active(sdl_keycode)) {
        data->button_data.at(bind.pad_data_index) = true;  // press the button
        m_active_actions.push_back(
            {sdl_keycode, bind, [](std::shared_ptr<PadData> data, InputBinding bind) {
               data->button_data.at(bind.pad_data_index) = false;  // let go of the button
             }});
      }
    }
  }
  // - Analog Sticks
  for (const auto& [sdl_keycode, bind_list] : binds.analog_axii) {
    for (const auto& bind : bind_list) {
      if (keyboard_state[SDL_GetScancodeFromKey(sdl_keycode)] &&
          bind.modifiers.has_necessary_modifiers(keyboard_modifier_state) &&
          !is_action_already_active(sdl_keycode)) {
        data->analog_data.at(bind.pad_data_index) += bind.minimum_in_range ? -127 : 127;
        data->update_analog_sim_tracker(false);
        m_active_actions.push_back(
            {sdl_keycode, bind, [](std::shared_ptr<PadData> data, InputBinding bind) {
               data->analog_data.at(bind.pad_data_index) += bind.minimum_in_range ? 127 : -127;
               data->update_analog_sim_tracker(true);
             }});
      }
    }
  }

  // Check if any previously tracked actions are now invalidated by the new state of the keyboard
  // if so, we'll run their revert code and remove them
  for (auto it = m_active_actions.begin(); it != m_active_actions.end();) {
    // Modifiers are easy, if the action required one and it's not pressed anymore, evict it
    // Alternatively, was the primary key released
    if (!keyboard_state[SDL_GetScancodeFromKey(it->sdl_keycode)] ||
        !it->binding.modifiers.has_necessary_modifiers(keyboard_modifier_state)) {
      it->revert_action(data, it->binding);
      it = m_active_actions.erase(it);
    } else {
      it++;
    }
  }
}

void KeyboardDevice::clear_actions(std::shared_ptr<PadData> data) {
  for (auto it = m_active_actions.begin(); it != m_active_actions.end();) {
    it->revert_action(data, it->binding);
    it = m_active_actions.erase(it);
  }
}

void KeyboardDevice::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> /*data*/,
                                   std::optional<InputBindAssignmentMeta>& bind_assignment) {
  if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
    const auto key_event = event.key;
    if (m_ignore_key_on_keyup && m_ignore_key_on_keyup.value() == (u32)key_event.keysym.sym &&
        event.type == SDL_KEYUP) {
      m_ignore_key_on_keyup = std::nullopt;
      return;
    }

    auto& binds = m_settings->keyboard_binds;

    // Binding re-assignment
    if (bind_assignment) {
      if (bind_assignment->device_type != InputDeviceType::KEYBOARD) {
        return;
      }
      // A normal key down event (a new key was pressed) and it's not a modifier
      if (event.type == SDL_KEYDOWN && !sdl_util::is_modifier_key(key_event.keysym.sym)) {
        if (bind_assignment->for_analog) {
          m_ignore_key_on_keyup = key_event.keysym.sym;
          binds.assign_analog_bind(key_event.keysym.sym, bind_assignment.value(),
                                   InputModifiers(key_event.keysym.mod));
        } else {
          binds.assign_button_bind(key_event.keysym.sym, bind_assignment.value(), false,
                                   InputModifiers(key_event.keysym.mod));
        }
      } else if (event.type == SDL_KEYUP) {
        // modifiers are instead inspected on a KEYUP, however if it's one of the keys
        // for triggering the binding assignment, and it's the first time we've seen it -- we ignore
        // it
        if (!bind_assignment->seen_confirm_up) {
          for (const auto& confirm_bind : bind_assignment->keyboard_confirmation_binds) {
            if (confirm_bind.sdl_idx == key_event.keysym.sym) {
              bind_assignment->seen_confirm_up = true;
              return;
            }
          }
        }
        // otherwise, set the bind
        if (bind_assignment->for_analog) {
          binds.assign_analog_bind(key_event.keysym.sym, bind_assignment.value(),
                                   InputModifiers(key_event.keysym.mod));
        } else {
          binds.assign_button_bind(key_event.keysym.sym, bind_assignment.value(), false,
                                   InputModifiers(key_event.keysym.mod));
        }
      }
      return;
    }

    // Check for commands
    if (event.type == SDL_KEYDOWN &&
        commands.keyboard_binds.find(key_event.keysym.sym) != commands.keyboard_binds.end()) {
      for (const auto& command : commands.keyboard_binds.at(key_event.keysym.sym)) {
        if (command.modifiers.has_necessary_modifiers(key_event.keysym.mod)) {
          command.command();
        }
      }
    }
  }
}
