/*!
 * @file display.cpp
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "display.h"

#include "gfx.h"

#include "common/log/log.h"

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
* GfxDisplay
********************************
*/

void GfxDisplay::set_title(const char* title) {
  if (!is_active()) {
    lg::error("No window to set title `{}`.", title);
    return;
  }

  // TODO set title?
  m_title = title;
}

int GfxDisplay::width() {
  int w;
  get_size(&w, NULL);
  return w;
}

int GfxDisplay::height() {
  int h;
  get_size(NULL, &h);
#ifdef _WIN32
  if (last_fullscreen_mode() == GfxDisplayMode::Borderless) {
    // windows borderless hack
    h--;
  }
#endif
  return h;
}

void GfxDisplay::backup_params() {
  get_size(&m_width, &m_height);
  get_position(&m_xpos, &m_ypos);
  fmt::print("backed up window: {},{} {}x{}\n", m_xpos, m_ypos, m_width, m_height);
}

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
  return g_displays.front()->is_active() ? g_displays.front() : NULL;
}

int InitMainDisplay(int width,
                    int height,
                    const char* title,
                    GfxSettings& settings,
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
  display->set_imgui_visible(true);
  set_main_display(display);
  return 0;
}

void KillMainDisplay() {
  KillDisplay(GetMainDisplay());
}

void KillDisplay(std::shared_ptr<GfxDisplay> display) {
  // lg::debug("kill display #x{:x}", (uintptr_t)display);
  if (!display->is_active()) {
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
