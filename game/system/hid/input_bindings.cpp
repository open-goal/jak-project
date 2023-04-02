#include "input_bindings.h"

#include <algorithm>

#include "common/util/json_util.h"

#include "game/system/hid/sdl_util.h"

#include "third-party/SDL/include/SDL.h"

void to_json(json& j, const InputBinding& obj) {
  j = json{{"pad_data_index", obj.pad_data_index}, {"minimum_in_range", obj.minimum_in_range},
           {"need_shift", obj.need_shift},         {"need_ctrl", obj.need_ctrl},
           {"need_meta", obj.need_meta},           {"need_alt", obj.need_alt}};
}

void from_json(const json& j, InputBinding& obj) {
  json_safe_deserialize(pad_data_index);
  json_safe_deserialize(minimum_in_range);
  json_safe_deserialize(need_shift);
  json_safe_deserialize(need_ctrl);
  json_safe_deserialize(need_meta);
  json_safe_deserialize(need_alt);
}

void to_json(json& j, const InputBindingGroups& obj) {
  j = json{{"device_type", obj.device_type},
           {"analog_axii", obj.analog_axii},
           {"button_axii", obj.button_axii},
           {"buttons", obj.buttons}};
}

void from_json(const json& j, InputBindingGroups& obj) {
  json_safe_deserialize(device_type);
  json_safe_deserialize(analog_axii);
  json_safe_deserialize(button_axii);
  json_safe_deserialize(buttons);
}

const std::vector<PadData::ButtonIndex> PAD_DATA_PRESSURE_INDEX_ORDER = {
    PadData::ButtonIndex::DPAD_RIGHT, PadData::ButtonIndex::DPAD_LEFT,
    PadData::ButtonIndex::DPAD_UP,    PadData::ButtonIndex::DPAD_DOWN,
    PadData::ButtonIndex::TRIANGLE,   PadData::ButtonIndex::CIRCLE,
    PadData::ButtonIndex::CROSS,      PadData::ButtonIndex::SQUARE,
    PadData::ButtonIndex::L1,         PadData::ButtonIndex::R1,
    PadData::ButtonIndex::L2,         PadData::ButtonIndex::R2};

const InputBindingGroups DEFAULT_CONTROLLER_BINDS = InputBindingGroups(
    CONTROLLER,
    {{SDL_CONTROLLER_AXIS_LEFTX, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDL_CONTROLLER_AXIS_LEFTY, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDL_CONTROLLER_AXIS_RIGHTX, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDL_CONTROLLER_AXIS_RIGHTY, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}}},
    {
        {SDL_CONTROLLER_AXIS_TRIGGERLEFT, {InputBinding(PadData::ButtonIndex::L2)}},
        {SDL_CONTROLLER_AXIS_TRIGGERRIGHT, {InputBinding(PadData::ButtonIndex::R2)}},
    },
    {{SDL_CONTROLLER_BUTTON_A, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDL_CONTROLLER_BUTTON_B, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDL_CONTROLLER_BUTTON_X, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDL_CONTROLLER_BUTTON_Y, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDL_CONTROLLER_BUTTON_LEFTSTICK, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSTICK, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDL_CONTROLLER_BUTTON_BACK, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDL_CONTROLLER_BUTTON_START, {InputBinding(PadData::ButtonIndex::START)}},
     {SDL_CONTROLLER_BUTTON_LEFTSHOULDER, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSHOULDER, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDL_CONTROLLER_BUTTON_DPAD_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDL_CONTROLLER_BUTTON_DPAD_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDL_CONTROLLER_BUTTON_DPAD_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDL_CONTROLLER_BUTTON_DPAD_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}});

const InputBindingGroups DEFAULT_KEYBOARD_BINDS =
    InputBindingGroups(KEYBOARD,
                       {{SDLK_a, {InputBinding(PadData::AnalogIndex::LEFT_X, true)}},
                        {SDLK_d, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
                        {SDLK_s, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
                        {SDLK_w, {InputBinding(PadData::AnalogIndex::LEFT_Y, true)}},
                        {SDLK_l, {InputBinding(PadData::AnalogIndex::RIGHT_X, true)}},
                        {SDLK_j, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
                        {SDLK_k, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}},
                        {SDLK_i, {InputBinding(PadData::AnalogIndex::RIGHT_Y, true)}}},
                       {},
                       {{SDLK_SPACE, {InputBinding(PadData::ButtonIndex::CROSS)}},
                        {SDLK_f, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
                        {SDLK_e, {InputBinding(PadData::ButtonIndex::SQUARE)}},
                        {SDLK_r, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
                        {SDLK_COMMA, {InputBinding(PadData::ButtonIndex::L3)}},
                        {SDLK_PERIOD, {InputBinding(PadData::ButtonIndex::R3)}},
                        {SDLK_QUOTE, {InputBinding(PadData::ButtonIndex::SELECT)}},
                        {SDLK_RETURN, {InputBinding(PadData::ButtonIndex::START)}},
                        {SDLK_o, {InputBinding(PadData::ButtonIndex::L1)}},
                        {SDLK_q, {InputBinding(PadData::ButtonIndex::R1)}},
                        {SDLK_1, {InputBinding(PadData::ButtonIndex::L2)}},
                        {SDLK_p, {InputBinding(PadData::ButtonIndex::R2)}},
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
      if (bind.pad_data_index != idx || (only_minimum_binds && !bind.minimum_in_range)) {
        continue;
      }
      InputBindingInfo new_info;
      new_info.host_name = "TODO";
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
  // TODO - clean this up, duplication
  std::vector<InputBindingInfo> entry = {};
  for (const auto& [sdl_code, binds] : buttons) {
    for (const auto& bind : binds) {
      if (bind.pad_data_index != idx) {
        continue;
      }
      InputBindingInfo new_info;
      new_info.sdl_idx = sdl_code;
      new_info.pad_idx = bind.pad_data_index;
      new_info.analog_button = false;
      switch (device_type) {
        case 0:
          new_info.host_name = sdl_util::get_controller_button_name(sdl_code);
          break;
        case 1:
          new_info.host_name = sdl_util::get_keyboard_button_name(sdl_code);
          break;
        case 2:
          new_info.host_name = sdl_util::get_mouse_button_name(sdl_code);
          break;
      }
      entry.push_back(new_info);
    }
  }
  for (const auto& [sdl_code, binds] : button_axii) {
    for (const auto& bind : binds) {
      if (bind.pad_data_index != idx) {
        continue;
      }
      InputBindingInfo new_info;
      new_info.sdl_idx = sdl_code;
      new_info.pad_idx = bind.pad_data_index;
      new_info.analog_button = true;
      switch (device_type) {
        case 0:
          new_info.host_name = sdl_util::get_controller_axis_name(sdl_code);
          break;
        case 1:
          new_info.host_name = sdl_util::get_keyboard_button_name(sdl_code);
          break;
        case 2:
          new_info.host_name = sdl_util::get_mouse_button_name(sdl_code);
          break;
      }
      entry.push_back(new_info);
    }
  }
  m_button_lookup[{idx, true}] = entry;
  return entry;
}

void InputBindingGroups::assign_analog_bind(u32 sdl_idx, InputBindAssignmentMeta& bind_meta) {
  // Find out if the PS2 input is already bound, if it is we will do a swap so no input is ever left
  // unmapped
  const auto current_binds = lookup_analog_binds((PadData::AnalogIndex)bind_meta.pad_idx);
  if (analog_axii.find(sdl_idx) != analog_axii.end()) {
    const auto existing_binds = analog_axii.at(sdl_idx);
    analog_axii[sdl_idx] = {InputBinding((PadData::AnalogIndex)bind_meta.pad_idx)};
    if (!current_binds.empty()) {
      // there was a current bind, so swap
      analog_axii[current_binds.front().sdl_idx] = existing_binds;
    }
  } else {
    analog_axii[sdl_idx] = {InputBinding((PadData::AnalogIndex)bind_meta.pad_idx)};
  }

  // Invalidate lookup cache
  m_analog_lookup.clear();
  bind_meta.assigned = true;
}

void InputBindingGroups::assign_button_bind(u32 sdl_idx,
                                            InputBindAssignmentMeta& bind_meta,
                                            const bool analog_button) {
  // Find out if the PS2 input is already bound, if it is we will do a swap so no input is ever left
  // unmapped
  const auto current_binds = lookup_button_binds((PadData::ButtonIndex)bind_meta.pad_idx);
  if (buttons.find(sdl_idx) != buttons.end()) {
    const auto existing_binds = buttons.at(sdl_idx);
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx)};
    }
    if (!current_binds.empty()) {
      // there already a bind, so swap
      if (current_binds.front().analog_button) {
        button_axii[current_binds.front().sdl_idx] = existing_binds;
      } else {
        buttons[current_binds.front().sdl_idx] = existing_binds;
      }
    }
  } else {
    if (analog_button) {
      button_axii[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx)};
    } else {
      buttons[sdl_idx] = {InputBinding((PadData::ButtonIndex)bind_meta.pad_idx)};
    }
  }

  // Invalidate lookup cache
  m_button_lookup.clear();
  bind_meta.assigned = true;
}

namespace input_bindings {

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
}  // namespace input_bindings
