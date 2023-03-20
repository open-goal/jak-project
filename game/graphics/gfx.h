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
#include "common/versions.h"

#include "game/kernel/common/kboot.h"
#include <game/settings/settings.h>
#include <game/system/hid/display_monitor.h>
#include <game/system/hid/input_monitor.h>

// forward declarations
struct GfxSettings;
class GfxDisplay;

// enum for rendering pipeline
enum class GfxPipeline { Invalid = 0, OpenGL };

// module for the different rendering pipelines
struct GfxRendererModule {
  std::function<int(GfxSettings&)> init;
  std::function<std::shared_ptr<GfxDisplay>(int width,
                                            int height,
                                            const char* title,
                                            GfxSettings& settings,
                                            GameVersion version,
                                            bool is_main)>
      make_display;
  std::function<void()> exit;
  std::function<u32()> vsync;
  std::function<u32()> sync_path;
  std::function<void(const void*, u32)> send_chain;
  std::function<void(const u8*, int, u32)> texture_upload_now;
  std::function<void(u32, u32, u32)> texture_relocate;
  /// NOTE - this is called _no where_ right now
  /// it's call was commented out in `libpad.cpp` a long time ago
  std::function<void()> poll_events;
  std::function<void(const std::vector<std::string>&)> set_levels;
  std::function<void(float)> set_pmode_alp;
  GfxPipeline pipeline;
  const char* name;
};

// store settings related to the gfx systems
// TODO merge with globalsettings
struct GfxSettings {
  // current version of the settings. this should be set up so that newer versions are always higher
  // than older versions
  // increment this whenever you change this struct.
  // there's probably a smarter way to do this (automatically deduce size etc.)
  static constexpr u64 CURRENT_VERSION = 0x0000'0000'0005'0000;

  u64 version;  // the version of this settings struct. MUST ALWAYS BE THE FIRST THING!

  int vsync;   // (temp) number of screen update per frame
  bool debug;  // graphics debugging

  GfxPipeline renderer = GfxPipeline::Invalid;  // which rendering pipeline to use.
};

// runtime settings
static constexpr int PAT_MOD_COUNT = 3;
static constexpr int PAT_EVT_COUNT = 7;
static constexpr int PAT_MAT_COUNT = 23;
struct GfxGlobalSettings {
  // note: this is actually the size of the display that ISN'T letterboxed
  // the excess space is what will be letterboxed away.
  int lbox_w = 640;
  int lbox_h = 480;

  // actual game resolution
  int game_res_w = 640;
  int game_res_h = 480;

  // multi-sampled anti-aliasing sample count. 1 = disabled.
  int msaa_samples = 4;

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
extern GfxSettings g_settings;
extern DebugSettings g_debug_settings;

const GfxRendererModule* GetCurrentRenderer();

u32 Init(GameVersion version);
void Loop(std::function<bool()> f);
u32 Exit();

u32 vsync();
void register_vsync_callback(std::function<void()> f);
void clear_vsync_callback();
u32 sync_path();
void send_chain(const void* data, u32 offset);
void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr);
void texture_relocate(u32 destination, u32 source, u32 format);
/// Unused
void poll_events();
void set_levels(const std::vector<std::string>& levels);

// InputMonitor usages
std::optional<std::shared_ptr<PadData>> get_current_frames_pad_data(const int port);
int update_rumble(const int port, const u8 low_intensity, const u8 high_intensity);
std::pair<s32, s32> get_mouse_pos();
void input_mode_set(u32 enable);
void input_mode_save();
s64 get_mapped_button(s64 pad, s64 button);

// VideoMonitor usages
u64 get_window_width();
u64 get_window_height();
void set_window_size(u64 width, u64 hheight);
void get_window_scale(float* x, float* y);
int get_connected_display_count();
std::string get_connected_display_name(int id);
int get_active_display_mode_count();
// TODO - this was always called via GOAL with -1 which meant "give me the current rate for the
// current display mode"
int get_active_display_refresh_rate();
void get_active_display_size(s32* w, s32* h);
void set_window_resizable(bool resizable);
WindowDisplayMode get_window_display_mode();
void set_window_display_mode(WindowDisplayMode mode);

// Global Settings Related
// TODO - how many of these actually need to be globals and instead should just be virtual functions
// in the gfx pipeline
void set_game_resolution(int w, int h);
void set_msaa(int samples);
void set_frame_rate(int rate);
void set_vsync(bool vsync);
void set_letterbox(int w, int h);

// matching enum in kernel-defs.gc !!
enum class RendererTreeType { NONE = 0, TFRAG3 = 1, TIE3 = 2, INVALID };
void SetLod(RendererTreeType tree, int lod);
bool CollisionRendererGetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererClearMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode);

}  // namespace Gfx
