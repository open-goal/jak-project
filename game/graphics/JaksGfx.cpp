#include "game/graphics/JaksGfx.h"

#include "game/graphics/gfx.h"
#include "game/runtime.h"

u32 JaksGfx::Init(void) {
  return Gfx::Init(g_game_version);
}
void JaksGfx::Loop(std::function<bool()> f) {
  Gfx::Loop(f);
}
u32 JaksGfx::Exit(void) {
  return Gfx::Exit();
}
void JaksGfx::register_vsync_callback(std::function<void()> f) {
  Gfx::register_vsync_callback(f);
}
void JaksGfx::clear_vsync_callback(void) {
  Gfx::clear_vsync_callback();
}
u64 JaksGfx::get_window_width(void) {
  return Gfx::get_window_width();
}
u64 JaksGfx::get_window_height(void) {
  return Gfx::get_window_height();
}
void JaksGfx::set_window_size(u64 w, u64 h) {
  Gfx::set_window_size(w, h);
}
void JaksGfx::get_window_scale(float* x, float* y) {
  Gfx::get_window_scale(x, y);
}
void JaksGfx::set_window_lock(bool lock) {
  Gfx::set_window_lock(lock);
}
void JaksGfx::get_screen_size(s64 vmode_idx, s32* w, s32* h) {
  return Gfx::get_screen_size(vmode_idx, w, h);
}
int JaksGfx::get_screen_rate(s64 vmode_idx) {
  return Gfx::get_screen_rate(vmode_idx);
}
int JaksGfx::get_screen_vmode_count(void) {
  return Gfx::get_screen_vmode_count();
}
int JaksGfx::get_monitor_count(void) {
  return Gfx::get_monitor_count();
}

void JaksGfx::set_vsync(bool vsync) {
  Gfx::set_vsync(vsync);
}
u32 JaksGfx::vsync() {
  return Gfx::vsync();
}
u32 JaksGfx::sync_path() {
  return Gfx::sync_path();
}
void JaksGfx::set_frame_rate(int rate) {
  Gfx::set_frame_rate(rate);
}
