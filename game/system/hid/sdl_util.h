#pragma once

#include <string>
#include <vector>

#include "input_bindings.h"

#include "third-party/SDL/include/SDL.h"

namespace sdl_util {
void log_error(const std::string& msg = "");
std::string log_and_return_error(const std::string& msg = "");
bool is_any_event_type(uint32_t event_type, const std::vector<uint32_t>& allowed_types);
SDL_bool sdl_bool(const bool val);
bool from_sdl_bool(const SDL_bool val);
bool is_SDL_GUID_zero(SDL_GUID guid);

std::string get_mouse_button_name(const int sdl_mouse_button_id, InputModifiers modifiers);
std::string get_keyboard_button_name(const int sdl_key_code, InputModifiers modifiers);
std::string get_controller_button_name(const int sdl_button_id);
std::string get_controller_axis_name(const int sdl_axis_id);

bool is_modifier_key(const SDL_Keycode key_code);
}  // namespace sdl_util
