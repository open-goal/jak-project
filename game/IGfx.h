#pragma once

#include <functional>

#include "common/common_types.h"

class IGfx {
 public:
  virtual u32 Init(void) = 0;
  virtual void Loop(std::function<bool()> f) = 0;
  virtual u32 Exit(void) = 0;
  virtual void register_vsync_callback(std::function<void()> f) = 0;
  virtual void clear_vsync_callback(void) = 0;
  virtual u64 get_window_width(void) = 0;
  virtual u64 get_window_height(void) = 0;
  virtual void set_window_size(u64 w, u64 h) = 0;
  virtual void get_window_scale(float* x, float* y) = 0;
  virtual void set_window_lock(bool lock) = 0;
  virtual void get_screen_size(s64 vmode_idx, s32* w, s32* h) = 0;
  virtual int get_screen_rate(s64 vmode_idx) = 0;
  virtual int get_screen_vmode_count(void) = 0;
  virtual int get_monitor_count(void) = 0;

  virtual void set_vsync(bool vsync) = 0;
  virtual u32 vsync() = 0;
  virtual u32 sync_path() = 0;
  virtual void set_frame_rate(int rate) = 0;
};