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

}  // namespace Gfx
