/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include <functional>

#include "gfx.h"
#include "display.h"
#include "pipelines/opengl.h"

#include "game/kernel/kscheme.h"
#include "common/symbols.h"
#include "common/log/log.h"
#include "game/runtime.h"
#include "game/system/newpad.h"

namespace {

// initializes a gfx settings.
// TODO save and load from file
void InitSettings(GfxSettings& settings) {
  // set the current settings version
  settings.version = GfxSettings::CURRENT_VERSION;

  // use opengl by default for now
  settings.renderer = Gfx::GetRenderer(GfxPipeline::OpenGL);  // Gfx::renderers[0];

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

u32 Init() {
  lg::info("GFX Init");
  // initialize settings
  InitSettings(g_settings);
  // guarantee we have no keys detected by pad
  Pad::ForceClearKeys();

  if (g_settings.renderer->init(g_settings)) {
    lg::error("Gfx::Init error");
    return 1;
  }

  if (g_main_thread_id != std::this_thread::get_id()) {
    lg::warn("ran Gfx::Init outside main thread. Init display elsewhere?");
  } else {
    Display::InitMainDisplay(640, 480, "testy", g_settings);
  }

  return 0;
}

void Loop(std::function<bool()> f) {
  lg::info("GFX Loop");
  while (f()) {
    // clean the inputs
    Pad::ClearKeys();

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
  g_settings.renderer->exit();
  return 0;
}

u32 vsync() {
  return g_settings.renderer->vsync();
}

u32 sync_path() {
  return g_settings.renderer->sync_path();
}

void send_chain(const void* data, u32 offset) {
  if (g_settings.renderer) {
    g_settings.renderer->send_chain(data, offset);
  }
}

void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  if (g_settings.renderer) {
    g_settings.renderer->texture_upload_now(tpage, mode, s7_ptr);
  }
}

void texture_relocate(u32 destination, u32 source, u32 format) {
  if (g_settings.renderer) {
    g_settings.renderer->texture_relocate(destination, source, format);
  }
}

void poll_events() {
  g_settings.renderer->poll_events();
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
    g_settings.pad_mapping_info_backup = g_settings.pad_mapping_info; // copy to backup
    g_settings.pad_mapping_info = Pad::g_input_mode_mapping; // set current mapping
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
