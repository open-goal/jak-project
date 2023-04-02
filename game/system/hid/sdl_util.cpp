#include "sdl_util.h"

#include "common/log/log.h"

#include "third-party/SDL/include/SDL.h"

namespace sdl_util {
void log_error(const std::string& msg) {
  std::string sdl_cause = SDL_GetError();
  lg::error("SDL Error: {} - Cause: {}", msg, sdl_cause.empty() ? "n/a" : sdl_cause);
}
bool is_any_event_type(uint32_t event_type, std::vector<uint32_t> allowed_types) {
  for (const auto& allowed_type : allowed_types) {
    if (allowed_type == event_type) {
      return true;
    }
  }
  return false;
}
SDL_bool sdl_bool(const bool val) {
  return val ? SDL_TRUE : SDL_FALSE;
}
bool from_sdl_bool(const SDL_bool val) {
  return val == SDL_TRUE ? true : false;
}
std::string get_mouse_button_name(const int sdl_mouse_button_id) {
  switch (sdl_mouse_button_id) {
    case SDL_BUTTON_LEFT:
      return "LEFT MOUSE";
    case SDL_BUTTON_MIDDLE:
      return "MIDDLE MOUSE";
    case SDL_BUTTON_RIGHT:
      return "RIGHT MOUSE";
    case SDL_BUTTON_X1:
      return "MOUSE 4";
    case SDL_BUTTON_X2:
      return "MOUSE 5";
    default:
      return "unknown";
  }
}
// TODO - handle modifiers nicely
std::string get_keyboard_button_name(const int sdl_key_code) {
  const auto result = SDL_GetKeyName((SDL_KeyCode)sdl_key_code);
  if (!result) {
    return "Unknown";
  }
  return result;
}
std::string get_controller_button_name(const int sdl_button_id) {
  const auto result = SDL_GameControllerGetStringForButton((SDL_GameControllerButton)sdl_button_id);
  if (!result) {
    return "Unknown";
  }
  return result;
}
std::string get_controller_axis_name(const int sdl_axis_id) {
  const auto result = SDL_GameControllerGetStringForAxis((SDL_GameControllerAxis)sdl_axis_id);
  if (!result) {
    return "Unknown";
  }
  return result;
}
}  // namespace sdl_util
