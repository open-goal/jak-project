/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include "gfx.h"

#include <cstdio>
#include <functional>
#include <utility>

#include "display.h"

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/common/file_paths.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/svnrev.h"
#include "game/runtime.h"
#include "pipelines/opengl.h"

namespace Gfx {

std::function<void()> vsync_callback;
GfxGlobalSettings g_global_settings;
game_settings::DebugSettings g_debug_settings;

const GfxRendererModule* GetRenderer(GfxPipeline pipeline) {
  switch (pipeline) {
    case GfxPipeline::Invalid:
      lg::error("Requested invalid renderer", fmt::underlying(pipeline));
      return NULL;
    case GfxPipeline::OpenGL:
      return &gRendererOpenGL;
    default:
      lg::error("Requested unknown renderer {}", fmt::underlying(pipeline));
      return NULL;
  }
}

void SetRenderer(GfxPipeline pipeline) {
  g_global_settings.renderer = GetRenderer(pipeline);
}

const GfxRendererModule* GetCurrentRenderer() {
  return g_global_settings.renderer;
}

u32 Init(GameVersion version) {
  lg::info("GFX Init");

  g_debug_settings = game_settings::DebugSettings();
  g_global_settings.renderer = GetRenderer(GfxPipeline::OpenGL);

  if (GetCurrentRenderer()->init(g_global_settings)) {
    lg::error("Gfx::Init error");
    return 1;
  }

  if (g_main_thread_id != std::this_thread::get_id()) {
    lg::error("Ran Gfx::Init outside main thread. Init display elsewhere?");
  } else {
    // TODO - make this not hardcoded to always be "work in progress"
    Display::InitMainDisplay(640, 480,
                             fmt::format("OpenGOAL - Work in Progress - {}", GIT_VERSION).c_str(),
                             g_global_settings, version);
  }

  return 0;
}

void Loop(std::function<bool()> f) {
  lg::info("GFX Loop");
  while (f()) {
    // check if we have a display
    if (Display::GetMainDisplay()) {
      Display::GetMainDisplay()->render();
    }
  }
}

u32 Exit() {
  lg::info("GFX Exit");
  Display::KillMainDisplay();
  GetCurrentRenderer()->exit();
  g_debug_settings.save_settings();
  return 0;
}

void register_vsync_callback(std::function<void()> f) {
  vsync_callback = std::move(f);
}

void clear_vsync_callback() {
  vsync_callback = nullptr;
}

u32 vsync() {
  if (GetCurrentRenderer()) {
    // Inform the IOP kernel that we're vsyncing so it can run the vblank handler
    if (vsync_callback != nullptr)
      vsync_callback();
    return GetCurrentRenderer()->vsync();
  }
  return 0;
}

u32 sync_path() {
  if (GetCurrentRenderer()) {
    return GetCurrentRenderer()->sync_path();
  }
  return 0;
}

void send_chain(const void* data, u32 offset) {
  if (GetCurrentRenderer()) {
    GetCurrentRenderer()->send_chain(data, offset);
  }
}

void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  if (GetCurrentRenderer()) {
    GetCurrentRenderer()->texture_upload_now(tpage, mode, s7_ptr);
  }
}

void texture_relocate(u32 destination, u32 source, u32 format) {
  if (GetCurrentRenderer()) {
    GetCurrentRenderer()->texture_relocate(destination, source, format);
  }
}

void set_levels(const std::vector<std::string>& levels) {
  if (GetCurrentRenderer()) {
    GetCurrentRenderer()->set_levels(levels);
  }
}

std::optional<std::shared_ptr<PadData>> get_current_frames_pad_data(const int port) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->get_current_data(port);
  }
  return {};
}

int update_rumble(const int port, const u8 low_intensity, const u8 high_intensity) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->update_rumble(port, low_intensity,
                                                                         high_intensity);
  }
  return 0;
}

std::pair<s32, s32> get_mouse_pos() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->get_mouse_pos();
  }
  return {0, 0};
}

int get_controller_count() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->get_num_controllers();
  }
  return 0;
}

std::string get_controller_name(const int id) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->get_controller_name(id);
  }
  return "";
}

std::string get_current_bind(const int port,
                             const int device_type,
                             const bool buttons,
                             const int input_idx,
                             const bool analog_for_minimum) {
  if (Display::GetMainDisplay()) {
    // TODO - return something that lets the runtime use a translatable string if unset
    return Display::GetMainDisplay()->get_input_manager()->get_current_bind(
        port, (InputDeviceType)device_type, buttons, input_idx, analog_for_minimum);
  }
  // TODO - return something that lets the runtime use a translatable string
  return "UNKNOWN";
}

void set_controller_id_for_port(const int id, const int port) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->set_controller_for_port(id, port);
  }
}

void set_keyboard_enabled(const bool enabled) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->enable_keyboard(enabled);
  }
}

void set_mouse_enabled(const bool enabled, const bool control_camera, const bool control_movement) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->enable_mouse(enabled, control_camera,
                                                                        control_movement);
  }
}

void set_mouse_camera_sens(const float xsens, const float ysens) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->set_camera_sens(xsens, ysens);
  }
}

void Gfx::ignore_background_controller_events(const bool ignore) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->ignore_background_controller_events(
        ignore);
  }
}

bool current_controller_has_led() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->controller_has_led(0);
  }
  return false;
}

void set_controller_led(const int port, const u8 red, const u8 green, const u8 blue) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->set_controller_led(port, red, green,
                                                                              blue);
  }
}

bool get_waiting_for_bind() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->get_waiting_for_bind();
  }
  return false;
}

void set_wait_for_bind(const InputDeviceType device_type,
                       const bool for_analog,
                       const bool for_minimum_analog,
                       const const int input_idx) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->set_wait_for_bind(
        device_type, for_analog, for_minimum_analog, input_idx);
  }
}

void stop_waiting_for_bind() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_input_manager()->stop_waiting_for_bind();
  }
}

u64 get_window_width() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_window_width();
  }
  return 0;
}

u64 get_window_height() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_window_height();
  }
  return 0;
}

void set_fullscreen_display(u64 display_id) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_display_manager()->set_fullscreen_display_id(display_id);
  }
}

void set_window_size(u64 width, u64 height) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_display_manager()->set_window_size(width, height);
  }
}

void get_window_scale(float* x, float* y) {
  if (Display::GetMainDisplay()) {
    if (x) {
      *x = Display::GetMainDisplay()->get_display_manager()->get_window_scale_x();
    }
    if (y) {
      *y = Display::GetMainDisplay()->get_display_manager()->get_window_scale_y();
    }
  }
}

int get_connected_display_count() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->num_connected_displays();
  }
  return 0;
}

std::string get_connected_display_name(int id) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_connected_display_name(id);
  }
  return "";
}

int get_active_display_refresh_rate() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_active_display_refresh_rate();
  }
  return 0;
}

void get_active_display_size(s32* width, s32* height) {
  if (Display::GetMainDisplay()) {
    if (width) {
      *width = Display::GetMainDisplay()->get_display_manager()->get_screen_width();
    }
    if (height) {
      *height = Display::GetMainDisplay()->get_display_manager()->get_screen_height();
    }
  }
}

WindowDisplayMode get_window_display_mode() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_window_display_mode();
  } else {
    return WindowDisplayMode::Windowed;
  }
}

void set_window_display_mode(WindowDisplayMode mode) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_display_manager()->set_window_display_mode(mode);
  }
}

void set_game_resolution(int w, int h) {
  g_global_settings.game_res_w = w;
  g_global_settings.game_res_h = h;
}

void set_window_resizable(bool resizable) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_display_manager()->set_window_resizable(resizable);
  }
}

void set_vsync(bool vsync) {
  g_global_settings.vsync = vsync;
}

void set_frame_rate(int rate) {
  g_global_settings.target_fps = rate;
}

void set_letterbox(int w, int h) {
  g_global_settings.lbox_w = w;
  g_global_settings.lbox_h = h;
}

void set_msaa(int samples) {
  g_global_settings.msaa_samples = samples;
}

void SetLod(RendererTreeType tree, int lod) {
  switch (tree) {
    case RendererTreeType::TFRAG3:
      g_global_settings.lod_tfrag = lod;
      break;
    case RendererTreeType::TIE3:
      g_global_settings.lod_tie = lod;
      break;
    default:
      lg::error("Invalid tree {} specified for SetLod ({})", fmt::underlying(tree), lod);
      break;
  }
}

bool CollisionRendererGetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id) {
  int arr_idx = mask_id / 32;
  int arr_ofs = mask_id % 32;

  switch (mode) {
    case GfxGlobalSettings::CollisionRendererMode::Mode:
      return (g_global_settings.collision_mode_mask[arr_idx] >> arr_ofs) & 1;
    case GfxGlobalSettings::CollisionRendererMode::Event:
      return (g_global_settings.collision_event_mask[arr_idx] >> arr_ofs) & 1;
    case GfxGlobalSettings::CollisionRendererMode::Material:
      return (g_global_settings.collision_material_mask[arr_idx] >> arr_ofs) & 1;
    case GfxGlobalSettings::CollisionRendererMode::Skip:
      ASSERT(arr_idx == 0);
      return (g_global_settings.collision_skip_mask >> arr_ofs) & 1;
    default:
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
      return false;
  }
}

void CollisionRendererSetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id) {
  int arr_idx = mask_id / 32;
  int arr_ofs = mask_id % 32;

  switch (mode) {
    case GfxGlobalSettings::CollisionRendererMode::Mode:
      g_global_settings.collision_mode_mask[arr_idx] |= 1 << arr_ofs;
      break;
    case GfxGlobalSettings::CollisionRendererMode::Event:
      g_global_settings.collision_event_mask[arr_idx] |= 1 << arr_ofs;
      break;
    case GfxGlobalSettings::CollisionRendererMode::Material:
      g_global_settings.collision_material_mask[arr_idx] |= 1 << arr_ofs;
      break;
    case GfxGlobalSettings::CollisionRendererMode::Skip:
      ASSERT(arr_idx == 0);
      g_global_settings.collision_skip_mask |= 1 << arr_ofs;
      break;
    default:
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
      break;
  }
}

void CollisionRendererClearMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id) {
  int arr_idx = mask_id / 32;
  int arr_ofs = mask_id % 32;

  switch (mode) {
    case GfxGlobalSettings::CollisionRendererMode::Mode:
      g_global_settings.collision_mode_mask[arr_idx] &= ~(1 << arr_ofs);
      break;
    case GfxGlobalSettings::CollisionRendererMode::Event:
      g_global_settings.collision_event_mask[arr_idx] &= ~(1 << arr_ofs);
      break;
    case GfxGlobalSettings::CollisionRendererMode::Material:
      g_global_settings.collision_material_mask[arr_idx] &= ~(1 << arr_ofs);
      break;
    case GfxGlobalSettings::CollisionRendererMode::Skip:
      ASSERT(arr_idx == 0);
      g_global_settings.collision_skip_mask &= ~(1 << arr_ofs);
      break;
    default:
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
      break;
  }
}

void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode) {
  g_global_settings.collision_mode = mode;
}

}  // namespace Gfx
