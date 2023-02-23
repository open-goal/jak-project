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

void GfxDisplay::save_display_settings() {
  nlohmann::json json;
  json["window_xpos"] = m_last_windowed_xpos;
  json["window_ypos"] = m_last_windowed_ypos;
  std::string file_path =
      (file_util::get_user_settings_dir(g_game_version) / "display-settings.json").string();
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, json.dump(2));
}

void GfxDisplay::restore_display_settings() {
  try {
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "display-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("reading {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    auto json = parse_commented_json(raw, "display-settings.json");
    if (json.contains("window_xpos")) {
      m_last_windowed_xpos = json.at("window_xpos").get<int>();
    }
    if (json.contains("window_ypos")) {
      m_last_windowed_ypos = json.at("window_ypos").get<int>();
    }
  } catch (std::exception& e) {
    // do nothing
  }
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
  set_main_display(display);
  // Restore window settings
  display->restore_display_settings();
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
    // Save the main display's position to a file so it can be restored upon re-opening
    display->save_display_settings();
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
