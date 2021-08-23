#pragma once

/*!
 * @file gfx.h
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include <functional>
#include <memory>

#include "common/common_types.h"
#include "game/kernel/kboot.h"
#include "game/system/newpad.h"

// forward declarations
struct GfxSettings;
class GfxDisplay;

// enum for rendering pipeline
enum class GfxPipeline { Invalid = 0, OpenGL };

// module for the different rendering pipelines
struct GfxRendererModule {
  std::function<int(GfxSettings&)> init;
  std::function<std::shared_ptr<GfxDisplay>(int w, int h, const char* title, GfxSettings& settings)>
      make_main_display;
  std::function<void(GfxDisplay*)> kill_display;
  std::function<void(GfxDisplay*)> render_display;
  std::function<void()> exit;
  std::function<u32()> vsync;
  std::function<u32()> sync_path;
  std::function<void(const void*, u32)> send_chain;
  std::function<void(const u8*, int, u32)> texture_upload_now;
  std::function<void(u32, u32, u32)> texture_relocate;
  std::function<void()> poll_events;

  GfxPipeline pipeline;
  const char* name;
};

// store settings related to the gfx systems
struct GfxSettings {
  // current version of the settings. this should be set up so that newer versions are always higher
  // than older versions
  // increment this whenever you change this struct.
  // there's probably a smarter way to do this (automatically deduce size etc.)
  static constexpr u64 CURRENT_VERSION = 0x0000'0000'0004'0001;

  u64 version;  // the version of this settings struct. MUST ALWAYS BE THE FIRST THING!

  Pad::MappingInfo pad_mapping_info;         // button mapping
  Pad::MappingInfo pad_mapping_info_backup;  // button mapping backup (see newpad.h)

  int vsync;   // (temp) number of screen update per frame
  bool debug;  // graphics debugging

  GfxPipeline renderer;  // which rendering pipeline to use.
};

namespace Gfx {

extern GfxSettings g_settings;
// extern const std::vector<const GfxRendererModule*> renderers;

const GfxRendererModule* GetRenderer(GfxPipeline pipeline);

u32 Init();
void Loop(std::function<bool()> f);
u32 Exit();

u32 vsync();
u32 sync_path();
void send_chain(const void* data, u32 offset);
void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr);
void texture_relocate(u32 destination, u32 source, u32 format);
void poll_events();
void input_mode_set(u32 enable);
void input_mode_save();
s64 get_mapped_button(s64 pad, s64 button);

int PadIsPressed(Pad::Button button, int port);

}  // namespace Gfx
