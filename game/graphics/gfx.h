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
#include "game/system/newpad.h"
#include "game/tools/filter_menu/filter_menu.h"

// forward declarations
struct GfxSettings;
class GfxDisplay;

// enum for rendering pipeline
enum class GfxPipeline { Invalid = 0, OpenGL };
enum GfxDisplayMode { ForceUpdate = -1, Windowed = 0, Fullscreen = 1, Borderless = 2 };

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
  static constexpr u64 CURRENT_VERSION = 0x0000'0000'0004'0001;

  u64 version;  // the version of this settings struct. MUST ALWAYS BE THE FIRST THING!

  Pad::MappingInfo pad_mapping_info;         // button mapping
  Pad::MappingInfo pad_mapping_info_backup;  // button mapping backup (see newpad.h)

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

struct DebugSettings {
  bool show_imgui = false;
  bool ignore_imgui_hide_keybind = false;
  std::vector<DebugTextFilter> debug_text_filters = {};
  bool debug_text_check_range = false;
  float debug_text_max_range = 0;

  void load_settings(const ghc::filesystem::path& filepath);
  void save_settings();
};

extern DebugSettings g_debug_settings;

const GfxRendererModule* GetCurrentRenderer();

u32 Init(GameVersion version);
void Loop(std::function<bool()> f);
u32 Exit();

Pad::MappingInfo& get_button_mapping();

u32 vsync();
void register_vsync_callback(std::function<void()> f);
void clear_vsync_callback();
u32 sync_path();
void send_chain(const void* data, u32 offset);
void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr);
void texture_relocate(u32 destination, u32 source, u32 format);
void set_levels(const std::vector<std::string>& levels);
void poll_events();
u64 get_window_width();
u64 get_window_height();
void set_window_size(u64 w, u64 h);
void get_window_scale(float* x, float* y);
std::tuple<double, double> get_mouse_pos();
GfxDisplayMode get_fullscreen();
int get_screen_vmode_count();
int get_screen_rate(s64 vmode_idx);
int get_monitor_count();
void get_screen_size(s64 vmode_idx, s32* w, s32* h);
void set_frame_rate(int rate);
void set_vsync(bool vsync);
void set_letterbox(int w, int h);
void set_fullscreen(GfxDisplayMode mode, int screen);
void set_window_lock(bool lock);
void set_game_resolution(int w, int h);
void set_msaa(int samples);
void input_mode_set(u32 enable);
void input_mode_save();
s64 get_mapped_button(s64 pad, s64 button);

int PadIsPressed(Pad::Button button, int port);
int PadGetAnalogValue(Pad::Analog analog, int port);

// matching enum in kernel-defs.gc !!
enum class RendererTreeType { NONE = 0, TFRAG3 = 1, TIE3 = 2, INVALID };
void SetLod(RendererTreeType tree, int lod);
bool CollisionRendererGetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererClearMask(GfxGlobalSettings::CollisionRendererMode mode, int mask_id);
void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode);

}  // namespace Gfx
