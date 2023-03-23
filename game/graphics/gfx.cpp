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

namespace {
// initializes a gfx settings.
// TODO save and load from file
void InitSettings(GfxSettings& settings) {
  // set the current settings version
  settings.version = GfxSettings::CURRENT_VERSION;

  // use opengl by default for now
  settings.renderer = GfxPipeline::OpenGL;  // Gfx::renderers[0];

  // 1 screen update per frame
  settings.vsync = 1;

  // debug for now
  settings.debug = true;
  // settings.pad_mapping_info.buffer_mode = true;
  //// debug input settings
  // settings.pad_mapping_info.debug = true;

  // Pad::DefaultMapping(Gfx::g_settings.pad_mapping_info);
}

}  // namespace

namespace Gfx {

std::function<void()> vsync_callback;
GfxGlobalSettings g_global_settings;
GfxSettings g_settings;
DebugSettings g_debug_settings;

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
  g_settings.renderer = pipeline;
}

const GfxRendererModule* GetCurrentRenderer() {
  return g_global_settings.renderer;
}

u32 Init(GameVersion version) {
  lg::info("GFX Init");
  // initialize settings
  InitSettings(g_settings);
  // guarantee we have no keys detected by pad
  // TODO - probably can be removed -- you can't clear keys because
  // we don't poll them every frame anymore!
  // Pad::ForceClearKeys();

  // TODO - rewrite
  // LoadSettings();
  SetRenderer(g_settings.renderer);

  if (GetCurrentRenderer()->init(g_settings)) {
    lg::error("Gfx::Init error");
    return 1;
  }

  if (g_main_thread_id != std::this_thread::get_id()) {
    lg::error("Ran Gfx::Init outside main thread. Init display elsewhere?");
  } else {
    Display::InitMainDisplay(640, 480,
                             fmt::format("OpenGOAL - Work in Progress - {}", GIT_VERSION).c_str(),
                             g_settings, version);
  }

  return 0;
}

void Loop(std::function<bool()> f) {
  lg::info("GFX Loop");
  while (f()) {
    // check if we have a display
    if (Display::GetMainDisplay()) {
      // lg::debug("run display");
      Display::GetMainDisplay()->render();
    }
  }
}

u32 Exit() {
  lg::info("GFX Exit");
  Display::KillMainDisplay();
  GetCurrentRenderer()->exit();
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

void poll_events() {
  if (GetCurrentRenderer()) {
    GetCurrentRenderer()->poll_events();
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

// TODO - rewrite
void input_mode_set(u32 enable) {
  // if (enable == s7.offset + jak1_symbols::FIX_SYM_TRUE) {  // #t
  //   Pad::g_input_mode_mapping = g_settings.pad_mapping_info;
  //   Pad::EnterInputMode();
  // } else {
  //   Pad::ExitInputMode(enable != s7.offset);  // use #f for graceful exit, or 'canceled for
  //   abrupt
  // }
}

void input_mode_save() {
  // if (Pad::input_mode_get() == (u64)Pad::InputModeStatus::Enabled) {
  //   lg::error("Can't save controller mapping while mapping controller.");
  // } else if (Pad::input_mode_get() == (u64)Pad::InputModeStatus::Disabled) {
  //   g_settings.pad_mapping_info_backup = g_settings.pad_mapping_info;  // copy to backup
  //   g_settings.pad_mapping_info = Pad::g_input_mode_mapping;           // set current mapping

  //  SavePeripheralSettings();
  //}
}

s64 get_mapped_button(s64 pad, s64 button) {
  /*if (pad < 0 || pad > Pad::CONTROLLER_COUNT || button < 0 || button > 16) {
    lg::error("Invalid parameters to get_mapped_button({}, {})", pad, button);
    return -1;
  }

  return (Pad::GetGamepadState(pad) > -1)
             ? (s64)g_settings.pad_mapping_info.controller_button_mapping[pad][button]
             : (s64)g_settings.pad_mapping_info.keyboard_button_mapping[pad][button];*/
}

// int PadIsPressed(Pad::Button button, int port) {
//   return Pad::IsPressed(g_settings.pad_mapping_info, button, port);
// }
//
// int PadGetAnalogValue(Pad::Analog analog, int port) {
//   return Pad::GetAnalogValue(g_settings.pad_mapping_info, analog, port);
// }

u64 get_window_width() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_window_width();
  } else {
    return 0;
  }
}

u64 get_window_height() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_display_manager()->get_window_height();
  } else {
    return 0;
  }
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
