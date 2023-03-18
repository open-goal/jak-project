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
}  // namespace sdl_util
