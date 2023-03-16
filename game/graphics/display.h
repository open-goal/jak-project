#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include <memory>
#include <vector>

#include "gfx.h"

#include "common/util/Assert.h"

#include <game/system/hid/display_monitor.h>

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
// specific renderers should override this class, the methods are abstract.
class GfxDisplay {
  int m_fullscreen_screen = -1;
  int m_fullscreen_target_screen = -1;
  bool m_imgui_visible;

 protected:
  // TODO - clean this stuff up
  bool m_main;
  // next mode
  WindowDisplayMode m_fullscreen_target_mode = WindowDisplayMode::Windowed;
  // current mode (start as -1 to force an initial fullscreen update)
  WindowDisplayMode m_fullscreen_mode = WindowDisplayMode::ForceUpdate;
  // previous mode (last frame)
  WindowDisplayMode m_last_fullscreen_mode = WindowDisplayMode::Windowed;

  // move it a bit away from the top, or the title bar can be hidden
  int m_last_windowed_xpos = 50;
  int m_last_windowed_ypos = 50;
  int m_last_windowed_width = 640;
  int m_last_windowed_height = 480;

 public:
  virtual ~GfxDisplay() {}

  virtual std::shared_ptr<DisplayMonitor> get_display_monitor() const = 0;
  virtual std::shared_ptr<InputMonitor> get_input_monitor() const = 0;

  virtual void update_fullscreen(WindowDisplayMode mode, int screen) = 0;
  virtual void render() = 0;
  virtual bool fullscreen_pending() {
    return fullscreen_mode() != m_fullscreen_target_mode ||
           m_fullscreen_screen != m_fullscreen_target_screen;
  }
  virtual void fullscreen_flush() {
    update_fullscreen(m_fullscreen_target_mode, m_fullscreen_target_screen);

    m_fullscreen_mode = m_fullscreen_target_mode;
    m_fullscreen_screen = m_fullscreen_target_screen;

    // hack, force a vsync update.
    Gfx::g_global_settings.old_vsync = !Gfx::g_global_settings.vsync;
  }

  void set_fullscreen(WindowDisplayMode mode, int screen) {
    m_fullscreen_target_mode = mode;
    m_fullscreen_target_screen = screen;
  }
  void update_last_fullscreen_mode() { m_last_fullscreen_mode = fullscreen_mode(); }
  WindowDisplayMode last_fullscreen_mode() const { return m_last_fullscreen_mode; }
  WindowDisplayMode fullscreen_mode() { return m_fullscreen_mode; }
  int fullscreen_screen() const { return m_fullscreen_screen; }
  void set_imgui_visible(bool visible) {
    m_imgui_visible = visible;
    Gfx::g_debug_settings.show_imgui = visible;
    Gfx::g_debug_settings.save_settings();
  }
  bool is_imgui_visible() const { return m_imgui_visible; }
  bool windowed() { return fullscreen_mode() == WindowDisplayMode::Windowed; }

  int width();
  int height();

  struct DisplaySettings {
    int window_xpos;
    int window_ypos;
  };

  void save_display_settings();
  void restore_display_settings();
};

namespace Display {

// a list of displays. the first one is the "main" display, all others are spectator-like extra
// views.
extern std::vector<std::shared_ptr<GfxDisplay>> g_displays;

int InitMainDisplay(int width,
                    int height,
                    const char* title,
                    GfxSettings& settings,
                    GameVersion version);
void KillDisplay(std::shared_ptr<GfxDisplay> display);
void KillMainDisplay();

std::shared_ptr<GfxDisplay> GetMainDisplay();

}  // namespace Display
