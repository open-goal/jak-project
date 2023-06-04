/*!
 * @file display.cpp
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "display.h"

#include <optional>

#include "gfx.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

#include "third-party/json.hpp"

/*
********************************
* Internal functions
********************************
*/

namespace {

void set_main_display(std::shared_ptr<GfxDisplay> display) {
  if (Display::g_displays.size() > 0) {
    Display::g_displays[0] = display;
  } else {
    Display::g_displays.push_back(display);
  }
}

}  // namespace

/*
********************************
* DISPLAY
********************************
*/

namespace Display {

std::vector<std::shared_ptr<GfxDisplay>> g_displays;
std::shared_ptr<GfxDisplay> GetMainDisplay() {
  if (g_displays.size() == 0)
    return NULL;
  return g_displays.front()->get_display_manager()->is_window_active() ? g_displays.front() : NULL;
}

int InitMainDisplay(int width,
                    int height,
                    const char* title,
                    GfxGlobalSettings& settings,
                    GameVersion version) {
  if (GetMainDisplay() != NULL) {
    lg::warn("InitMainDisplay called when main display already exists.");
    return 1;
  }

  auto display =
      Gfx::GetCurrentRenderer()->make_display(width, height, title, settings, version, true);
  if (display == NULL) {
    lg::error("Failed to make main display.");
    return 1;
  }
  set_main_display(display);
  return 0;
}

void KillMainDisplay() {
  KillDisplay(GetMainDisplay());
}

void KillDisplay(std::shared_ptr<GfxDisplay> display) {
  // lg::debug("kill display #x{:x}", (uintptr_t)display);
  if (!display->get_display_manager()->is_window_active()) {
    lg::warn("display #x{:x} cant be killed because it is not active");
    return;
  }

  if (GetMainDisplay() == display) {
    // killing the main display, kill all children displays too!
    while (g_displays.size() > 1) {
      KillDisplay(g_displays.at(1));
    }
  }

  // find this display in the list and remove it from there
  // if everything went right the smart pointer should delete the display.
  auto this_disp = std::find(g_displays.begin(), g_displays.end(), display);
  g_displays.erase(this_disp);
}

}  // namespace Display
