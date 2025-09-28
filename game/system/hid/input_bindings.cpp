#include "input_bindings.h"

#include <algorithm>

#include "common/util/json_util.h"

#include "game/system/hid/sdl_util.h"

#include "third-party/SDL/include/SDL3/SDL.h"

InputModifiers::InputModifiers(const u16 sdl_mod_state) {
  need_shift = sdl_mod_state & SDL_KMOD_SHIFT;
  need_alt = sdl_mod_state & SDL_KMOD_ALT;
  need_ctrl = sdl_mod_state & SDL_KMOD_CTRL;
  need_meta = sdl_mod_state & SDL_KMOD_GUI;
}

bool InputModifiers::has_necessary_modifiers(const u16 key_modifiers) const {
  if (need_alt && ((key_modifiers & SDL_KMOD_ALT) == 0)) {
    return false;
  }
  if (need_ctrl && ((key_modifiers & SDL_KMOD_CTRL) == 0)) {
    return false;
  }
  if (need_meta && ((key_modifiers & SDL_KMOD_GUI) == 0)) {
    return false;
  }
  if (need_shift && ((key_modifiers & SDL_KMOD_SHIFT) == 0)) {
    return false;
  }
  return true;
}

void to_json(json& j, const InputModifiers& obj) {
  j = json{{"need_shift", obj.need_shift},
           {"need_ctrl", obj.need_ctrl},
           {"need_meta", obj.need_meta},
           {"need_alt", obj.need_alt}};
}
void from_json(const json& j, InputModifiers& obj) {
  json_deserialize_if_exists(need_shift);
  json_deserialize_if_exists(need_ctrl);
  json_deserialize_if_exists(need_meta);
  json_deserialize_if_exists(need_alt);
}

void to_json(json& j, const InputBinding& obj) {
  j = json{{"pad_data_index", obj.pad_data_index},
           {"minimum_in_range", obj.minimum_in_range},
           {"modifiers", obj.modifiers}};
}

void from_json(const json& j, InputBinding& obj) {
  json_deserialize_if_exists(pad_data_index);
  json_deserialize_if_exists(minimum_in_range);
  json_deserialize_if_exists(modifiers);
}

void to_json(json& j, const InputBindingGroups& obj) {
  j = json{{"device_type", obj.device_type},
           {"analog_axii", obj.analog_axii},
           {"button_axii", obj.button_axii},
           {"buttons", obj.buttons}};
}

void from_json(const json& j, InputBindingGroups& obj) {
  json_deserialize_if_exists(device_type);
  json_deserialize_if_exists(analog_axii);
  json_deserialize_if_exists(button_axii);
  json_deserialize_if_exists(buttons);
}

const InputBindingGroups DEFAULT_CONTROLLER_BINDS = InputBindingGroups(
    CONTROLLER,
    {{SDL_GAMEPAD_AXIS_LEFTX, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDL_GAMEPAD_AXIS_LEFTY, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDL_GAMEPAD_AXIS_RIGHTX, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDL_GAMEPAD_AXIS_RIGHTY, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}}},
    {
        {SDL_GAMEPAD_AXIS_LEFT_TRIGGER, {InputBinding(PadData::ButtonIndex::L2)}},
        {SDL_GAMEPAD_AXIS_RIGHT_TRIGGER, {InputBinding(PadData::ButtonIndex::R2)}},
    },
    {{SDL_GAMEPAD_BUTTON_SOUTH, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDL_GAMEPAD_BUTTON_EAST, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDL_GAMEPAD_BUTTON_WEST, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDL_GAMEPAD_BUTTON_NORTH, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDL_GAMEPAD_BUTTON_LEFT_STICK, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDL_GAMEPAD_BUTTON_RIGHT_STICK, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDL_GAMEPAD_BUTTON_BACK, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDL_GAMEPAD_BUTTON_START, {InputBinding(PadData::ButtonIndex::START)}},
     {SDL_GAMEPAD_BUTTON_LEFT_SHOULDER, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDL_GAMEPAD_BUTTON_DPAD_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDL_GAMEPAD_BUTTON_DPAD_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDL_GAMEPAD_BUTTON_DPAD_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDL_GAMEPAD_BUTTON_DPAD_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}});

const InputBindingGroups DEFAULT_KEYBOARD_BINDS =
    InputBindingGroups(KEYBOARD,
                       {{SDLK_A, {InputBinding(PadData::AnalogIndex::LEFT_X, true)}},
                        {SDLK_D, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
                        {SDLK_S, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
                        {SDLK_W, {InputBinding(PadData::AnalogIndex::LEFT_Y, true)}},
                        {SDLK_L, {InputBinding(PadData::AnalogIndex::RIGHT_X, true)}},
                        {SDLK_J, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
                        {SDLK_K, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}},
                        {SDLK_I, {InputBinding(PadData::AnalogIndex::RIGHT_Y, true)}}},
                       {},
                       {{SDLK_SPACE, {InputBinding(PadData::ButtonIndex::CROSS)}},
                        {SDLK_E, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
                        {SDLK_F, {InputBinding(PadData::ButtonIndex::SQUARE)}},
                        {SDLK_R, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
                        {SDLK_COMMA, {InputBinding(PadData::ButtonIndex::L3)}},
                        {SDLK_PERIOD, {InputBinding(PadData::ButtonIndex::R3)}},
                        {SDLK_APOSTROPHE, {InputBinding(PadData::ButtonIndex::SELECT)}},
                        {SDLK_RETURN, {InputBinding(PadData::ButtonIndex::START)}},
                        {SDLK_Q, {InputBinding(PadData::ButtonIndex::L1)}},
                        {SDLK_O, {InputBinding(PadData::ButtonIndex::R1)}},
                        {SDLK_1, {InputBinding(PadData::ButtonIndex::L2)}},
                        {SDLK_P, {InputBinding(PadData::ButtonIndex::R2)}},
                        {SDLK_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
                        {SDLK_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
                        {SDLK_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
                        {SDLK_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}});

const InputBindingGroups DEFAULT_MOUSE_BINDS = InputBindingGroups(MOUSE, {}, {}, {});

std::vector<InputBindingInfo> InputBindingGroups::lookup_analog_binds(PadData::AnalogIndex idx,
                                                                      bool only_minimum_binds) {
  // First see if it's in the cache, if it is return it
  if (m_analog_lookup.find({idx, only_minimum_binds}) != m_analog_lookup.end()) {
    return m_analog_lookup.at({idx, only_minimum_binds});
  }

  // Didn't find it, let's construct the cache entry
  std::vector<InputBindingInfo> entry = {};
  for (const auto& [sdl_code, binds] : analog_axii) {
    for (const auto& bind : binds) {
      if (bind.pad_data_index != idx || bind.minimum_in_range != only_minimum_binds) {
        continue;
      }
      InputBindingInfo new_info;
      switch (device_type) {
        case KEYBOARD:
          new_info = InputBindingInfo(bind, device_type, sdl_code, false);
          break;
        default:
          new_info.host_name = "TODO - NON-KB ANALOG BIND LOOKUP";
          break;
      }
      entry.push_back(new_info);
    }
  }
  m_analog_lookup[{idx, only_minimum_binds}] = entry;
  return entry;
}

std::vector<InputBindingInfo> InputBindingGroups::lookup_button_binds(PadData::ButtonIndex idx) {
  // First see if it's in the cache, if it is return it
  if (m_button_lookup.find({idx, true}) != m_button_lookup.end()) {
    return m_button_lookup.at({idx, true});
  }

  // Didn't find it, let's construct the cache entry
  std::vector<InputBindingInfo> entry = {};
  for (const auto& [sdl_code, binds] : buttons) {
    for (const auto& bind : binds) {
      if (bind.pad_data_index != idx) {
        continue;
      }
      InputBindingInfo new_info(bind, device_type, sdl_code, false);
      entry.push_back(new_info);
    }
  }
  for (const auto& [sdl_code, binds] : button_axii) {
    for (const auto& bind : binds) {
      if (bind.pad_data_index != idx) {
        continue;
      }
      InputBindingInfo new_info(bind, device_type, sdl_code, true);
      entry.push_back(new_info);
    }
  }
  m_button_lookup[{idx, true}] = entry;
  return entry;
}

void InputBindingGroups::remove_multiple_binds(
    u32 sdl_idx,
    InputBindAssignmentMeta& bind_meta,
    std::unordered_map<u32, std::vector<InputBinding>>& bind_map) {
  for (auto it = bind_map.begin(); it != bind_map.end();) {
    if (it->first == sdl_idx) {
      it++;
      continue;
    }
    bool found_match = false;
    for (const auto& bind : it->second) {
      if (bind.pad_data_index != bind_meta.pad_idx ||
          bind.minimum_in_range != bind_meta.for_analog_minimum) {
        continue;
      }
      it = bind_map.erase(it);
      found_match = true;
      break;
    }
    if (!found_match) {
      it++;
    }
  }
}

std::optional<std::pair<InputBinding, bool>> InputBindingGroups::find_button_bind_from_sdl_idx(
    u32 sdl_idx,
    const std::optional<InputModifiers> modifiers) {
  // We need to check that there isn't a shared SDL key on the button group side
  for (const auto& [sdl_code, binds] : buttons) {
    if (sdl_code == sdl_idx) {
      for (const auto bind : binds) {
        if (!modifiers || bind.modifiers == modifiers.value()) {
          std::pair<InputBinding, bool> result = {bind, false};
          return result;
        }
      }
    }
  }
  for (const auto& [sdl_code, binds] : button_axii) {
    if (sdl_code == sdl_idx) {
      for (const auto bind : binds) {
        if (!modifiers || bind.modifiers == modifiers.value()) {
          std::pair<InputBinding, bool> result = {bind, true};
          return result;
        }
      }
    }
  }
  return {};
}

void InputBindingGroups::assign_analog_bind(u32 sdl_idx,
                                            InputBindAssignmentMeta& bind_meta,
                                            const std::optional<InputModifiers> modifiers) {
  // Find out if the PS2 input is already bound, if it is we will do a swap so no input is ever left
  // unmapped
  const auto current_analog_binds =
      lookup_analog_binds((PadData::AnalogIndex)bind_meta.pad_idx, bind_meta.for_analog_minimum);
  const auto current_button_bind = find_button_bind_from_sdl_idx(sdl_idx, modifiers);
  if (analog_axii.find(sdl_idx) != analog_axii.end()) {
    const auto existing_binds = analog_axii.at(sdl_idx);
    analog_axii[sdl_idx] = {InputBinding((PadData::AnalogIndex)bind_meta.pad_idx,
                                         bind_meta.for_analog_minimum, modifiers)};
    // there already a bind, so swap (as long as it's not the same key)
    if (!current_analog_binds.empty() && (u32)current_analog_binds.front().sdl_idx != sdl_idx) {
      analog_axii[current_analog_binds.front().sdl_idx] = existing_binds;
    }
  } else if (!current_analog_binds.empty() && current_button_bind.has_value()) {
    analog_axii[sdl_idx] = {InputBinding((PadData::AnalogIndex)bind_meta.pad_idx,
                                         bind_meta.for_analog_minimum, modifiers)};
    if (current_button_bind->second) {
      // button_axii
      button_axii.erase(sdl_idx);
      button_axii[current_analog_binds.front().sdl_idx] = {current_button_bind->first};
    } else {
      // button
      buttons.erase(sdl_idx);
      buttons[current_analog_binds.front().sdl_idx] = {current_button_bind->first};
    }
  } else {
    analog_axii[sdl_idx] = {InputBinding((PadData::AnalogIndex)bind_meta.pad_idx,
                                         bind_meta.for_analog_minimum, modifiers)};
  }
  remove_multiple_binds(sdl_idx, bind_meta, analog_axii);
  // Invalidate lookup cache
  m_analog_lookup.clear();
  m_button_lookup.clear();
  bind_meta.assigned = true;
}

std::optional<std::pair<InputBinding, bool>> InputBindingGroups::find_analog_bind_from_sdl_idx(
    u32 sdl_idx,
    const std::optional<InputModifiers> modifiers) {
  // We need to check that there isn't a shared SDL key on the button group side
  for (const auto& [sdl_code, binds] : analog_axii) {
    if (sdl_code == sdl_idx) {
      for (const auto bind : binds) {
        if (!modifiers || bind.modifiers == modifiers.value()) {
          std::pair<InputBinding, bool> result = {bind, false};
          return result;
        }
      }
    }
  }
  return {};
}

void InputBindingGroups::assign_button_bind(u32 sdl_idx,
                                            InputBindAssignmentMeta& bind_meta,
                                            const bool analog_button,
                                            const std::optional<InputModifiers> modifiers) {
  // Find out if the PS2 input is already bound, if it is we will do a swap so no input is ever left
  // unmapped
  const auto current_button_binds = lookup_button_binds((PadData::ButtonIndex)bind_meta.pad_idx);
  const auto current_analog_bind = find_analog_bind_from_sdl_idx(sdl_idx, modifiers);
  if (!analog_button && buttons.find(sdl_idx) != buttons.end()) {
    const auto existing_binds = buttons.at(sdl_idx);
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    }
    // there already a bind, so swap (as long as it's not the same key)
    if (!current_button_binds.empty() && current_button_binds.front().sdl_idx != (s32)sdl_idx) {
      if (current_button_binds.front().analog_button) {
        button_axii[current_button_binds.front().sdl_idx] = existing_binds;
      } else {
        buttons[current_button_binds.front().sdl_idx] = existing_binds;
      }
    }
  } else if (analog_button && button_axii.find(sdl_idx) != button_axii.end()) {
    // TODO - cleanup this duplication
    const auto existing_binds = button_axii.at(sdl_idx);
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    }
    // there already a bind, so swap (as long as it's not the same key)
    if (!current_button_binds.empty() && current_button_binds.front().sdl_idx != (s32)sdl_idx) {
      if (current_button_binds.front().analog_button) {
        button_axii[current_button_binds.front().sdl_idx] = existing_binds;
      } else {
        buttons[current_button_binds.front().sdl_idx] = existing_binds;
      }
    }
  } else if (!current_button_binds.empty() && current_analog_bind.has_value()) {
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    }
    analog_axii.erase(sdl_idx);
    analog_axii[current_button_binds.front().sdl_idx] = {current_analog_bind->first};
  } else {
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx, modifiers)};
    }
  }
  remove_multiple_binds(sdl_idx, bind_meta, buttons);
  remove_multiple_binds(sdl_idx, bind_meta, button_axii);
  // Invalidate lookup cache
  m_button_lookup.clear();
  m_analog_lookup.clear();
  bind_meta.assigned = true;
}

void InputBindingGroups::set_bindings(const InputBindingGroups& binds) {
  analog_axii = binds.analog_axii;
  button_axii = binds.button_axii;
  buttons = binds.buttons;
  m_analog_lookup.clear();
  m_button_lookup.clear();
}

InputBindingInfo::InputBindingInfo(const InputBinding bind,
                                   const InputDeviceType device_type,
                                   const s32 sdl_code,
                                   const bool _analog_button)
    : sdl_idx(sdl_code),
      pad_idx(bind.pad_data_index),
      analog_button(_analog_button),
      modifiers(bind.modifiers) {
  switch (device_type) {
    case CONTROLLER:
      if (analog_button) {
        host_name = sdl_util::get_controller_axis_name(sdl_code);
      } else {
        host_name = sdl_util::get_controller_button_name(sdl_code);
      }
      break;
    case KEYBOARD:
      host_name = sdl_util::get_keyboard_button_name(sdl_code, modifiers);
      break;
    case MOUSE:
      host_name = sdl_util::get_mouse_button_name(sdl_code, modifiers);
      break;
  }
}
