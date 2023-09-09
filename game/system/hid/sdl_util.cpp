#include "sdl_util.h"

#include "common/log/log.h"

#include "third-party/SDL/include/SDL.h"

namespace sdl_util {
void log_error(const std::string& msg) {
  std::string sdl_cause = SDL_GetError();
  lg::error("SDL Error: {} - Cause: {}", msg, sdl_cause.empty() ? "n/a" : sdl_cause);
}
std::string log_and_return_error(const std::string& msg) {
  std::string sdl_cause = SDL_GetError();
  lg::error("SDL Error: {} - Cause: {}", msg, sdl_cause.empty() ? "n/a" : sdl_cause);
  return sdl_cause;
}
bool is_any_event_type(uint32_t event_type, const std::vector<uint32_t>& allowed_types) {
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
bool is_SDL_GUID_zero(SDL_GUID guid) {
  for (int i = 0; i < 16; i++) {
    if (guid.data[i] != 0) {
      return false;
    }
  }
  return true;
}

std::vector<std::string> get_modifier_strings(InputModifiers modifiers) {
  std::vector<std::string> result = {};
  if (modifiers.need_ctrl) {
    result.push_back("ctrl");
  }
  if (modifiers.need_shift) {
    result.push_back("shift");
  }
  if (modifiers.need_alt) {
    result.push_back("alt");
  }
  if (modifiers.need_meta) {
    result.push_back("meta");
  }
  return result;
}
std::string get_mouse_button_name(const int sdl_mouse_button_id, InputModifiers modifiers) {
  std::string result = "";
  switch (sdl_mouse_button_id) {
    case SDL_BUTTON_LEFT:
      result = "LEFT MOUSE";
      break;
    case SDL_BUTTON_MIDDLE:
      result = "MIDDLE MOUSE";
      break;
    case SDL_BUTTON_RIGHT:
      result = "RIGHT MOUSE";
      break;
    case SDL_BUTTON_X1:
      result = "MOUSE 4";
      break;
    case SDL_BUTTON_X2:
      result = "MOUSE 5";
      break;
    default:
      result = "";
  }
  if (result.empty()) {
    return "unknown";
  }
  auto tokens = get_modifier_strings(modifiers);
  tokens.push_back(result);
  return fmt::to_string(fmt::join(tokens, " + "));
}

std::string get_keyboard_button_name(const int sdl_key_code, InputModifiers modifiers) {
  const auto result = SDL_GetKeyName((SDL_KeyCode)sdl_key_code);
  if (!result) {
    return "Unknown";
  }
  auto tokens = get_modifier_strings(modifiers);
  tokens.push_back(result);
  return fmt::to_string(fmt::join(tokens, " + "));
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

bool is_modifier_key(const SDL_Keycode key_code) {
  return key_code == SDLK_LSHIFT || key_code == SDLK_RSHIFT || key_code == SDLK_LALT ||
         key_code == SDLK_RALT || key_code == SDLK_LCTRL || key_code == SDLK_RCTRL ||
         key_code == SDLK_LGUI || key_code == SDLK_RGUI;
}
}  // namespace sdl_util
