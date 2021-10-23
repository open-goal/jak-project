/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include <cstdio>
#include <functional>
#include <filesystem>

#include "gfx.h"
#include "display.h"
#include "pipelines/opengl.h"

#include "common/symbols.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "game/common/file_paths.h"
#include "game/kernel/kscheme.h"
#include "game/runtime.h"
#include "game/system/newpad.h"

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

GfxSettings g_settings;
// const std::vector<const GfxRendererModule*> renderers = {&moduleOpenGL};

void LoadSettings() {
  const auto filename = file_util::get_file_path({GAME_CONFIG_DIR_NAME, SETTINGS_GFX_FILE_NAME});
  if (std::filesystem::exists(filename)) {
    FILE* fp = fopen(filename.c_str(), "rb");
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
  FILE* fp = fopen(filename.c_str(), "wb");
  fwrite(&g_settings, sizeof(GfxSettings), 1, fp);
  fclose(fp);
  lg::info("Saved graphics configuration file.");
}

const GfxRendererModule* GetRenderer(GfxPipeline pipeline) {
  switch (pipeline) {
    case GfxPipeline::Invalid:
      lg::error("Requested invalid graphics pipeline!");
      return NULL;
      break;
    case GfxPipeline::OpenGL:
      return &moduleOpenGL;
    default:
      lg::error("Unknown graphics pipeline {}", (u64)pipeline);
      return NULL;
  }
}

const GfxRendererModule* GetCurrentRenderer() {
  return GetRenderer(g_settings.renderer);
}

u32 Init() {
  lg::info("GFX Init");
  // initialize settings
  InitSettings(g_settings);
  // guarantee we have no keys detected by pad
  Pad::ForceClearKeys();

  LoadSettings();

  if (GetCurrentRenderer()->init(g_settings)) {
    lg::error("Gfx::Init error");
    return 1;
  }

  if (g_main_thread_id != std::this_thread::get_id()) {
    lg::warn("Ran Gfx::Init outside main thread. Init display elsewhere?");
  } else {
    Display::InitMainDisplay(640, 480, "OpenGOAL GL Window", g_settings);
  }

  return 0;
}

void Loop(std::function<bool()> f) {
  lg::info("GFX Loop");
  while (f()) {
    // check if we have a display
    if (Display::GetMainDisplay()) {
      // lg::debug("run display");
      Display::GetMainDisplay()->render_graphics();
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
  return GetCurrentRenderer()->vsync();
}

u32 sync_path() {
  return GetCurrentRenderer()->sync_path();
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

void poll_events() {
  GetCurrentRenderer()->poll_events();
}

void input_mode_set(u32 enable) {
  if (enable == s7.offset + FIX_SYM_TRUE) {  // #t
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

}  // namespace Gfx
