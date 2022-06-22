#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include <vector>
#include <memory>
#include "common/util/Assert.h"
#include "gfx.h"

// a GfxDisplay class is equivalent to a window that displays stuff. This holds an actual internal
// window pointer used by whichever renderer. It also contains functions for setting and
// retrieving certain window parameters.
// Maybe this is better implemented as an abstract class and renderers would have overrides?
class GfxDisplay {
  const char* m_title;

  // NOT actual size! just backups
  int m_width;
  int m_height;
  // same here
  int m_xpos;
  int m_ypos;

  GfxDisplayMode m_fullscreen_target_mode = GfxDisplayMode::Windowed;
  GfxDisplayMode m_last_fullscreen_mode;
  int m_fullscreen_screen;
  int m_fullscreen_target_screen;
  bool m_imgui_visible;

 protected:
  bool m_main;

 public:
  virtual ~GfxDisplay() {}

  virtual void* get_window() const = 0;
  virtual void set_size(int w, int h) = 0;
  virtual void update_fullscreen(GfxDisplayMode mode, int screen) = 0;
  virtual void get_scale(float* x, float* y) = 0;
  virtual void get_screen_size(int vmode_idx, s32* w, s32* h, s32* c) = 0;
  virtual void get_position(int* x, int* y) = 0;
  virtual void get_size(int* w, int* h) = 0;
  virtual GfxDisplayMode get_fullscreen() = 0;
  virtual void render() = 0;
  virtual void set_lock(bool lock) = 0;
  bool is_active() const { return get_window() != nullptr; }
  void set_title(const char* title);
  const char* title() const { return m_title; }

  bool fullscreen_pending() { return get_fullscreen() != m_fullscreen_target_mode; }
  void fullscreen_flush() {
    update_fullscreen(m_fullscreen_target_mode, m_fullscreen_target_screen);
    // TODO no
    m_fullscreen_screen = m_fullscreen_target_screen;
  }
  void set_fullscreen(GfxDisplayMode mode, int screen) {
    m_fullscreen_target_mode = mode;
    m_fullscreen_target_screen = screen;
  }
  void update_last_fullscreen_mode() { m_last_fullscreen_mode = get_fullscreen(); }
  GfxDisplayMode last_fullscreen_mode() const { return m_last_fullscreen_mode; }
  int fullscreen_screen() const { return m_fullscreen_screen; }
  void set_imgui_visible(bool visible) { m_imgui_visible = visible; }
  bool is_imgui_visible() const { return m_imgui_visible; }
  bool windowed() { return get_fullscreen() == GfxDisplayMode::Windowed; }
  void backup_params();
  int width_backup() const { return m_width; }
  int height_backup() const { return m_height; }
  int xpos_backup() const { return m_xpos; }
  int ypos_backup() const { return m_ypos; }

  int width();
  int height();
};

namespace Display {

// a list of displays. the first one is the "main" display, all others are spectator-like extra
// views.
extern std::vector<std::shared_ptr<GfxDisplay>> g_displays;

int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings);
void KillDisplay(std::shared_ptr<GfxDisplay> display);
void KillMainDisplay();

std::shared_ptr<GfxDisplay> GetMainDisplay();

}  // namespace Display
