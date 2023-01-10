#pragma once

#include "game/IGfx.h"

class JaksGfx final : public IGfx {
  virtual u32 Init(void) override;
  virtual void Loop(std::function<bool()> f) override;
  virtual u32 Exit(void) override;
  virtual void register_vsync_callback(std::function<void()> f) override;
  virtual void clear_vsync_callback(void) override;
  virtual u64 get_window_width(void) override;
  virtual u64 get_window_height(void) override;
  virtual void set_window_size(u64 w, u64 h) override;
  virtual void get_window_scale(float* x, float* y) override;
  virtual void set_window_lock(bool lock) override;
  virtual void get_screen_size(s64 vmode_idx, s32* w, s32* h) override;
  virtual int get_screen_rate(s64 vmode_idx) override;
  virtual int get_screen_vmode_count(void) override;
  virtual int get_monitor_count(void) override;

  virtual void set_vsync(bool vsync) override;
  virtual u32 vsync() override;
  virtual u32 sync_path() override;
  virtual void set_frame_rate(int rate) override;
};
