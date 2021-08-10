/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include "gfx.h"
#include <functional>
#include "common/log/log.h"
#include "game/runtime.h"
#include "display.h"

#include "pipelines/opengl.h"

namespace {

// initializes a gfx settings.
// TODO save and load from file
void InitSettings(GfxSettings& settings) {
  // use opengl by default for now
  settings.renderer = Gfx::GetRenderer(GfxPipeline::OpenGL);  // Gfx::renderers[0];

  // 1 screen update per frame
  settings.vsync = 1;

  return;
}

}  // namespace

namespace Gfx {

GfxVertex g_vertices_temp[VERTEX_BUFFER_LENGTH_TEMP];

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

  if (g_settings.renderer->init()) {
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
    // check if we have a display
    if (Display::GetMainDisplay()) {
      // lg::debug("run display");
      Display::GetMainDisplay()->render_graphics();
    }
  }
}

u32 Exit() {
  lg::info("GFX Exit");
  Display::KillDisplay(Display::GetMainDisplay());
  g_settings.renderer->exit();
  return 0;
}

u32 vsync() {
  return g_settings.renderer->vsync();
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

void texture_relocate(u32 destination, u32 source) {
  if (g_settings.renderer) {
    g_settings.renderer->texture_relocate(destination, source);
  }
}

}  // namespace Gfx
