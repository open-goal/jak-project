#pragma once

/*!
 * @file gfx.h
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include <array>
#include <functional>
#include <memory>

#include "common/common_types.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

#include "game/kernel/common/kboot.h"
#include "game/settings/settings.h"
#include "game/system/hid/display_manager.h"
#include "game/system/hid/input_manager.h"

// forward declarations
struct GfxGlobalSettings;
class GfxDisplay;

// enum for rendering pipeline
enum class GfxPipeline { Invalid = 0, OpenGL };

// module for the different rendering pipelines
struct GfxRendererModule {
  std::function<int(GfxGlobalSettings&)> init;
  std::function<std::shared_ptr<GfxDisplay>(int width,
                                            int height,
                                            const char* title,
                                            GfxGlobalSettings& settings,
                                            GameVersion version,
                                            bool is_main)>
      make_display;
  std::function<void()> exit;
  std::function<u32()> vsync;
  std::function<u32()> sync_path;
  std::function<void(const void*, u32)> send_chain;
  std::function<void(const u8*, int, u32)> texture_upload_now;
  std::function<void(u32, u32, u32)> texture_relocate;
  std::function<void(const std::vector<std::string>&)> set_levels;
  std::function<void(float)> set_pmode_alp;
  GfxPipeline pipeline;
  const char* name;
};

// runtime settings
static constexpr int PAT_MOD_COUNT = 3;
static constexpr int PAT_EVT_COUNT = 7;
static constexpr int PAT_MAT_COUNT = 23;
struct GfxGlobalSettings {
  bool debug = true;  // graphics debugging

  // note: this is actually the size of the display that ISN'T letterboxed
  // the excess space is what will be letterboxed away.
  int lbox_w = 640;
  int lbox_h = 480;

  // actual game resolution
  int game_res_w = 640;
  int game_res_h = 480;

  // multi-sampled anti-aliasing sample count. 1 = disabled.
  int msaa_samples = 2;

  // current renderer
  const GfxRendererModule* renderer;

  // lod settings, used by bucket renderers
  int lod_tfrag = 0;
  int lod_tie = 0;

  // vsync enable
  bool vsync = true;
  bool old_vsync = false;
  // target frame rate
  float target_fps = 60;
  // use custom frame limiter
  bool framelimiter = true;

  // frame timing things
  bool experimental_accurate_lag = false;
  bool sleep_in_frame_limiter = true;

  // fancy effect things
  bool hack_no_tex = false;

  // collision renderer settings
  bool collision_enable = false;
  bool collision_wireframe = true;

  // matching enum in kernel-defs.gc !!
  enum CollisionRendererMode { None, Mode, Event, Material, Skip } collision_mode = Mode;
  std::array<u32, (PAT_MOD_COUNT + 31) / 32> collision_mode_mask = {UINT32_MAX};
  std::array<u32, (PAT_EVT_COUNT + 31) / 32> collision_event_mask = {UINT32_MAX};
  std::array<u32, (PAT_MAT_COUNT + 31) / 32> collision_material_mask = {UINT32_MAX};
  u32 collision_skip_mask = UINT32_MAX;
};

namespace Gfx {

extern GfxGlobalSettings g_global_settings;
extern game_settings::DebugSettings g_debug_settings;

const GfxRendererModule* GetCurrentRenderer();

u32 Init(GameVersion version);
void Loop(std::function<bool()> f);
u32 Exit();

u32 vsync();
void register_vsync_callback(std::function<void()> f);
void clear_vsync_callback();
u32 sync_path();

// matching enum in kernel-defs.gc !!
enum class RendererTreeType { NONE = 0, TFRAG3 = 1, TIE3 = 2, INVALID };
bool CollisionRendererGetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererClearMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode);

}  // namespace Gfx
