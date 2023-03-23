#pragma once

#include <string>
#include <vector>

#include "third-party/SDL/include/SDL.h"

namespace sdl_util {
void log_error(const std::string& msg = "");
bool is_any_event_type(uint32_t event_type, std::vector<uint32_t> allowed_types);
SDL_bool sdl_bool(const bool val);
}  // namespace sdl_util
