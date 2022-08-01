/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include "gfx.h"

#include <cstdio>
#include <functional>

#include "display.h"

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"

#include "game/common/file_paths.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/svnrev.h"
#include "game/runtime.h"
#include "game/system/newpad.h"
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

  // use buffered input mode
  settings.pad_mapping_info.buffer_mode = true;
  // debug input settings
  settings.pad_mapping_info.debug = true;
  // use a default mapping
  Pad::DefaultMapping(settings.pad_mapping_info);
}

}  // namespace

namespace Gfx {

GfxGlobalSettings g_global_settings;
GfxSettings g_settings;
// const std::vector<const GfxRendererModule*> renderers = {&moduleOpenGL};

// TODO serialize
void LoadSettings() {
  const auto filename = file_util::get_file_path({GAME_CONFIG_DIR_NAME, SETTINGS_GFX_FILE_NAME});
  if (fs::exists(filename)) {
    // this is just wrong LOL
    FILE* fp = file_util::open_file(filename.c_str(), "rb");
    lg::info("Found graphics configuration file. Checking version.");
    u64 version;
    fread(&version, sizeof(u64), 1, fp);
    if (version == GfxSettings::CURRENT_VERSION) {
      fseek(fp, 0, SEEK_SET);
      fread(&g_settings, sizeof(GfxSettings), 1, fp);
      lg::info("Loaded graphics configuration file.");
    } else {
      // TODO upgrade func
      lg::info("Detected graphics configuration file from old version. Ignoring.");
    }
    fclose(fp);
  }
}

void SaveSettings() {
  const auto filename = file_util::get_file_path({GAME_CONFIG_DIR_NAME, SETTINGS_GFX_FILE_NAME});
  file_util::create_dir_if_needed(file_util::get_file_path({GAME_CONFIG_DIR_NAME}));
  FILE* fp = file_util::open_file(filename.c_str(), "wb");
  fwrite(&g_settings, sizeof(GfxSettings), 1, fp);
  fclose(fp);
  lg::info("Saved graphics configuration file.");
}

const GfxRendererModule* GetRenderer(GfxPipeline pipeline) {
  switch (pipeline) {
    case GfxPipeline::Invalid:
      lg::error("Requested invalid renderer", pipeline);
      return NULL;
    case GfxPipeline::OpenGL:
      return &gRendererOpenGL;
    default:
      lg::error("Requested unknown renderer {}", (u64)pipeline);
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
  Pad::ForceClearKeys();

  LoadSettings();
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

u32 vsync() {
  if (GetCurrentRenderer()) {
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

u64 get_window_width() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->width();
  } else {
    return 0;
  }
}

u64 get_window_height() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->height();
  } else {
    return 0;
  }
}

void set_window_size(u64 w, u64 h) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->set_size(w, h);
  }
}

void get_window_scale(float* x, float* y) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_scale(x, y);
  }
}

GfxDisplayMode get_fullscreen() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->fullscreen_mode();
  } else {
    return GfxDisplayMode::Windowed;
  }
}

int get_screen_vmode_count() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_screen_vmode_count();
  }
  return 0;
}

int get_screen_rate(s64 vmode_idx) {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_screen_rate(vmode_idx);
  }
  return 0;
}

int get_monitor_count() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_monitor_count();
  }
  return 0;
}

void get_screen_size(s64 vmode_idx, s32* w, s32* h) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->get_screen_size(vmode_idx, w, h);
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

void set_fullscreen(GfxDisplayMode mode, int screen) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->set_fullscreen(mode, screen);
  }
}

void set_window_lock(bool lock) {
  if (Display::GetMainDisplay()) {
    Display::GetMainDisplay()->set_lock(lock);
  }
}

void set_game_resolution(int w, int h) {
  g_global_settings.game_res_w = w;
  g_global_settings.game_res_h = h;
}

void set_msaa(int samples) {
  g_global_settings.msaa_samples = samples;
}

void input_mode_set(u32 enable) {
  if (enable == s7.offset + jak1_symbols::FIX_SYM_TRUE) {  // #t
    Pad::g_input_mode_mapping = g_settings.pad_mapping_info;
    Pad::EnterInputMode();
  } else {
    Pad::ExitInputMode(enable != s7.offset);  // use #f for graceful exit, or 'canceled for abrupt
  }
}

void input_mode_save() {
  if (Pad::input_mode_get() == (u64)Pad::InputModeStatus::Enabled) {
    lg::error("Can't save controller mapping while mapping controller.");
  } else if (Pad::input_mode_get() == (u64)Pad::InputModeStatus::Disabled) {
    g_settings.pad_mapping_info_backup = g_settings.pad_mapping_info;  // copy to backup
    g_settings.pad_mapping_info = Pad::g_input_mode_mapping;           // set current mapping

    SaveSettings();
  }
}

s64 get_mapped_button(s64 pad, s64 button) {
  if (pad < 0 || pad > Pad::CONTROLLER_COUNT || button < 0 || button > 16) {
    lg::error("Invalid parameters to get_mapped_button({}, {})", pad, button);
    return -1;
  }
  return (s64)g_settings.pad_mapping_info.pad_mapping[pad][button];
}

int PadIsPressed(Pad::Button button, int port) {
  return Pad::IsPressed(g_settings.pad_mapping_info, button, port);
}

int PadAnalogValue(Pad::Analog analog, int port) {
  return Pad::AnalogValue(g_settings.pad_mapping_info, analog, port);
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
      lg::error("Invalid tree {} specified for SetLod ({})", tree, lod);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, mode, mask_id);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, mode, mask_id);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, mode, mask_id);
      break;
  }
}

void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode) {
  g_global_settings.collision_mode = mode;
}

}  // namespace Gfx
