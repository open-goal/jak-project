#include "keyboard.h"

#include "game/system/hid/sdl_util.h"

KeyboardDevice::KeyboardDevice(std::shared_ptr<game_settings::InputSettings> settings) {
  m_settings = settings;
}

void KeyboardDevice::process_event(const SDL_Event& event,
                                   const CommandBindingGroups& commands,
                                   std::shared_ptr<PadData> data,
                                   std::optional<InputBindAssignmentMeta>& bind_assignment,
                                   bool ignore_inputs) {
  if (ignore_inputs) {
    return;
  }
  if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
    const auto key_event = event.key;
    if (key_event.repeat != 0) {
      return;
    }
    if (m_ignore_key_on_keyup && m_ignore_key_on_keyup.value() == key_event.keysym.sym &&
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
        } else {
          // otherwise, set the bind
          if (bind_assignment->for_analog) {
            binds.assign_analog_bind(key_event.keysym.sym, bind_assignment.value(),
                                     InputModifiers(key_event.keysym.mod));
          } else {
            binds.assign_button_bind(key_event.keysym.sym, bind_assignment.value(), false,
                                     InputModifiers(key_event.keysym.mod));
          }
        }
      }
      return;
    }

    // Normal Buttons
    if (binds.buttons.find(key_event.keysym.sym) != binds.buttons.end()) {
      for (const auto& bind : binds.buttons.at(key_event.keysym.sym)) {
        if (bind.modifiers.has_necessary_modifiers(key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Buttons (useless for keyboards, but here for completeness)
    if (binds.button_axii.find(key_event.keysym.sym) != binds.button_axii.end()) {
      for (const auto& bind : binds.button_axii.at(key_event.keysym.sym)) {
        if (bind.modifiers.has_necessary_modifiers(key_event.keysym.mod)) {
          data->button_data.at(bind.pad_data_index) = event.type == SDL_KEYDOWN;
        }
      }
    }
    // Analog Sticks simulating
    if (binds.analog_axii.find(key_event.keysym.sym) != binds.analog_axii.end()) {
      for (const auto& bind : binds.analog_axii.at(key_event.keysym.sym)) {
        if (bind.modifiers.has_necessary_modifiers(key_event.keysym.mod)) {
          int analog_val = bind.minimum_in_range ? 127 : -127;
          if (event.type == SDL_KEYDOWN) {
            analog_val = bind.minimum_in_range ? -127 : 127;
          }
          data->analog_data.at(bind.pad_data_index) += analog_val;
          data->update_analog_sim_tracker(event.type != SDL_KEYDOWN);
        }
      }
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
