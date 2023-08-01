#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include <memory>
#include <vector>

#include "gfx.h"

#include "common/util/Assert.h"

#include "game/system/hid/display_manager.h"

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
// specific renderers should override this class, the methods are abstract.
class GfxDisplay {
  bool m_imgui_visible;

 protected:
  bool m_main;

 public:
  virtual ~GfxDisplay() {}

  virtual std::shared_ptr<DisplayManager> get_display_manager() const = 0;
  virtual std::shared_ptr<InputManager> get_input_manager() const = 0;

  virtual void render() = 0;

  void set_imgui_visible(bool visible) {
    m_imgui_visible = visible;
    Gfx::g_debug_settings.show_imgui = visible;
  }
  bool is_imgui_visible() const { return m_imgui_visible; }
};

namespace Display {

// a list of displays. the first one is the "main" display, all others are spectator-like extra
// views.
extern std::vector<std::shared_ptr<GfxDisplay>> g_displays;

int InitMainDisplay(int width,
                    int height,
                    const char* title,
                    GfxGlobalSettings& settings,
                    GameVersion version);
void KillDisplay(std::shared_ptr<GfxDisplay> display);
void KillMainDisplay();

std::shared_ptr<GfxDisplay> GetMainDisplay();

}  // namespace Display
