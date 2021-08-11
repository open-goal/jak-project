#pragma once

/*!
 * @file gfx.h
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include <functional>
#include <memory>
#include "common/common_types.h"
#include "game/kernel/kboot.h"

// forward declarations
struct GfxSettings;
class GfxDisplay;

// enum for rendering pipeline
enum class GfxPipeline { Invalid = 0, OpenGL };

// module for the different rendering pipelines
struct GfxRendererModule {
  std::function<int()> init;
  std::function<std::shared_ptr<GfxDisplay>(int w, int h, const char* title, GfxSettings& settings)>
      make_main_display;
  std::function<void(GfxDisplay* display)> kill_display;
  std::function<void(GfxDisplay* display)> render_display;
  std::function<void()> exit;
  std::function<u32()> vsync;
  std::function<u32()> sync_path;
  std::function<void(const void*, u32)> send_chain;
  std::function<void(const u8*, int, u32)> texture_upload_now;
  std::function<void(u32, u32)> texture_relocate;

  GfxPipeline pipeline;
  const char* name;
};

// store settings related to the gfx systems
struct GfxSettings {
  const GfxRendererModule* renderer;  // which rendering pipeline to use.

  int vsync;  // (temp) number of screen update per frame
};

// struct for a single vertex. this should in theory be renderer-agnostic
struct GfxVertex {
  // x y z
  float x, y, z;

  // rgba or the full u32 thing.
  union {
    u32 rgba;
    struct {
      u8 r, g, b, a;
    };
  };
};

namespace Gfx {

static constexpr int VERTEX_BUFFER_LENGTH_TEMP = 4096;
extern GfxVertex g_vertices_temp[VERTEX_BUFFER_LENGTH_TEMP];

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
void texture_relocate(u32 destination, u32 source);

}  // namespace Gfx
