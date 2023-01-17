#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include <memory>
#include <vector>

#include "gfx.h"

#include "common/util/Assert.h"

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
// specific renderers should override this class, the methods are abstract.
class GfxDisplay {
  const char* m_title;

  int m_fullscreen_screen = -1;
  int m_fullscreen_target_screen = -1;
  bool m_imgui_visible;

 protected:
  bool m_main;
  // next mode
  GfxDisplayMode m_fullscreen_target_mode = GfxDisplayMode::Windowed;
  // current mode (start as -1 to force an initial fullscreen update)
  GfxDisplayMode m_fullscreen_mode = GfxDisplayMode::ForceUpdate;
  // previous mode (last frame)
  GfxDisplayMode m_last_fullscreen_mode = GfxDisplayMode::Windowed;

  // move it a bit away from the top, or the title bar can be hidden
  int m_last_windowed_xpos = 50;
  int m_last_windowed_ypos = 50;
  int m_last_windowed_width = 640;
  int m_last_windowed_height = 480;

 public:
  virtual ~GfxDisplay() {}

  virtual void* get_window() const = 0;
  virtual void set_size(int w, int h) = 0;
  virtual void update_fullscreen(GfxDisplayMode mode, int screen) = 0;
  virtual void get_scale(float* x, float* y) = 0;
  virtual int get_screen_vmode_count() = 0;
  virtual void get_screen_size(int vmode_idx, s32* w, s32* h) = 0;
  virtual int get_screen_rate(int vmode_idx) = 0;
  virtual int get_monitor_count() = 0;
  virtual void get_position(int* x, int* y) = 0;
  virtual void get_size(int* w, int* h) = 0;
  virtual void render() = 0;
  virtual void set_lock(bool lock) = 0;
  virtual bool minimized() = 0;
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
  virtual std::tuple<double, double> get_mouse_pos() = 0;

  bool is_active() const { return get_window() != nullptr; }
  void set_title(const char* title);
  const char* title() const { return m_title; }
  void set_fullscreen(GfxDisplayMode mode, int screen) {
    m_fullscreen_target_mode = mode;
    m_fullscreen_target_screen = screen;
  }
  void update_last_fullscreen_mode() { m_last_fullscreen_mode = fullscreen_mode(); }
  GfxDisplayMode last_fullscreen_mode() const { return m_last_fullscreen_mode; }
  GfxDisplayMode fullscreen_mode() { return m_fullscreen_mode; }
  int fullscreen_screen() const { return m_fullscreen_screen; }
  void set_imgui_visible(bool visible) {
    m_imgui_visible = visible;
    Gfx::g_debug_settings.show_imgui = visible;
    Gfx::g_debug_settings.save_settings();
  }
  bool is_imgui_visible() const { return m_imgui_visible; }
  bool windowed() { return fullscreen_mode() == GfxDisplayMode::Windowed; }

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
