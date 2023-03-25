/*!
 * @file gfx.cpp
 * Graphics component for the runtime. Abstraction layer for the main graphics routines.
 */

#include "gfx.h"

#include <cstdio>
#include <functional>
#include <utility>

#include "display.h"

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

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

  Pad::DefaultMapping(Gfx::g_settings.pad_mapping_info);
}

}  // namespace

namespace Gfx {

std::function<void()> vsync_callback;
GfxGlobalSettings g_global_settings;
GfxSettings g_settings;
DebugSettings g_debug_settings;

void DebugSettings::load_settings(const ghc::filesystem::path& filepath) {
  auto file_txt = file_util::read_text_file(filepath);
  auto json = parse_commented_json(file_txt, filepath.string());

  if (json.contains("show_imgui")) {
    show_imgui = json["show_imgui"].get<bool>();
  }
  if (json.contains("ignore_imgui_hide_keybind")) {
    ignore_imgui_hide_keybind = json["ignore_imgui_hide_keybind"].get<bool>();
  }
  if (json.contains("debug_text_check_range")) {
    debug_text_check_range = json["debug_text_check_range"].get<bool>();
  }
  if (json.contains("debug_text_max_range")) {
    debug_text_max_range = json["debug_text_max_range"].get<float>();
  }
  // TODO - not loading filters because they aren't being persisted
}

void DebugSettings::save_settings() {
  nlohmann::json json;
  json["show_imgui"] = show_imgui;
  json["ignore_imgui_hide_keybind"] = ignore_imgui_hide_keybind;
  json["debug_text_check_range"] = debug_text_check_range;
  json["debug_text_max_range"] = debug_text_max_range;
  // TODO - persist the filters as well, not doing it yet because i havn't added a way to remove em
  // via the UI
  auto debug_settings_filename =
      file_util::get_user_misc_dir(g_game_version) / "debug-settings.json";
  file_util::create_dir_if_needed_for_file(debug_settings_filename);
  file_util::write_text_file(debug_settings_filename, json.dump(2));
}

Pad::MappingInfo& get_button_mapping() {
  return g_settings.pad_mapping_info;
}

// const std::vector<const GfxRendererModule*> renderers = {&moduleOpenGL};

// Not crazy about this declaration
const std::pair<std::string, Pad::Button> gamepad_map[] = {{"Select", Pad::Button::Select},
                                                           {"L3", Pad::Button::L3},
                                                           {"R3", Pad::Button::R3},
                                                           {"Start", Pad::Button::Start},
                                                           {"Up", Pad::Button::Up},
                                                           {"Right", Pad::Button::Right},
                                                           {"Down", Pad::Button::Down},
                                                           {"Left", Pad::Button::Left},
                                                           {"L1", Pad::Button::L1},
                                                           {"R1", Pad::Button::R1},
                                                           {"Triangle", Pad::Button::Triangle},
                                                           {"Circle", Pad::Button::Circle},
                                                           {"X", Pad::Button::X},
                                                           {"Square", Pad::Button::Square}};

const std::pair<std::string, Pad::Analog> analog_map[] = {
    {"Left X Axis", Pad::Analog::Left_X},
    {"Left Y Axis", Pad::Analog::Left_Y},
    {"Right X Axis", Pad::Analog::Right_X},
    {"Right Y Axis", Pad::Analog::Right_Y},
};

void DumpToJson(ghc::filesystem::path& filename) {
  nlohmann::json json;
  auto& peripherals_json = json["Peripherals"];
  json["Use Mouse"] = g_settings.pad_mapping_info.use_mouse;

  for (uint32_t i = 0; i < Pad::CONTROLLER_COUNT; ++i) {
    nlohmann::json peripheral_json;
    peripheral_json["ID"] = i + 1;

    auto& controller_json = peripheral_json["Controller"];
    auto& controller_buttons_json = controller_json["Buttons"];
    for (const auto& [name, value] : gamepad_map) {
      controller_buttons_json[name] =
          g_settings.pad_mapping_info.controller_button_mapping[i][(int)value];
    }

    auto& keyboard_json = peripheral_json["Keyboard+Mouse"];
    auto& keyboard_buttons_json = keyboard_json["Buttons"];
    for (const auto& [name, value] : gamepad_map) {
      keyboard_buttons_json[name] =
          g_settings.pad_mapping_info.keyboard_button_mapping[i][(int)value];
    }

    auto& keyboard_analogs_json = keyboard_json["Analog"];
    for (const auto& [name, value] : analog_map) {
      if (g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].mode ==
          Pad::AnalogMappingMode::AnalogInput) {
        keyboard_analogs_json[name]["Axis Id"] =
            g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].axis_id;
      } else {
        keyboard_analogs_json[name]["Positive Key"] =
            g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].positive_key;
        keyboard_analogs_json[name]["Negative Key"] =
            g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].negative_key;
      }
    }
    peripheral_json["X-Axis Mouse Sensitivity"] =
        g_settings.pad_mapping_info.mouse_x_axis_sensitivities[i];
    peripheral_json["Y-Axis Mouse Sensitivity"] =
        g_settings.pad_mapping_info.mouse_y_axis_sensitivities[i];
    peripherals_json.emplace_back(peripheral_json);
  }

  file_util::write_text_file(filename, json.dump(4));
}

void SavePeripheralSettings() {
  auto filename = (file_util::get_user_config_dir() / "controller" / "controller-settings.json");
  file_util::create_dir_if_needed_for_file(filename);

  DumpToJson(filename);
  lg::info("Saved graphics configuration file.");
}

void LoadPeripheralSettings(const ghc::filesystem::path& filepath) {
  Pad::DefaultMapping(g_settings.pad_mapping_info);

  lg::info("reading {}", filepath.string());
  auto file_txt = file_util::read_text_file(filepath);
  auto configuration = parse_commented_json(file_txt, filepath.string());

  if (configuration.find("Use Mouse") != configuration.end()) {
    g_settings.pad_mapping_info.use_mouse = configuration["Use Mouse"].get<bool>();
  }
  int controller_index = 0;
  for (const auto& peripheral : configuration["Peripherals"]) {
    auto& controller_buttons_json = peripheral["Controller"]["Buttons"];
    auto& keyboard_buttons_json = peripheral["Keyboard+Mouse"]["Buttons"];

    for (const auto& [name, button] : gamepad_map) {
      if (controller_buttons_json.find(name) != controller_buttons_json.end()) {
        g_settings.pad_mapping_info.controller_button_mapping[controller_index][(int)button] =
            controller_buttons_json[name].get<int>();
      } else {
        lg::warn(
            "Controller button override not found for {}. Using controller default value: {}", name,
            g_settings.pad_mapping_info.controller_button_mapping[controller_index][(int)button]);
      }

      if (keyboard_buttons_json.find(name) != keyboard_buttons_json.end()) {
        g_settings.pad_mapping_info.keyboard_button_mapping[controller_index][(int)button] =
            keyboard_buttons_json[name].get<int>();
      } else {
        lg::warn(
            "Keyboard button override not found for {}. Using keyboard default value: {}", name,
            g_settings.pad_mapping_info.keyboard_button_mapping[controller_index][(int)button]);
      }
    }

    auto& keyboard_analogs_json = peripheral["Keyboard+Mouse"]["Analog"];
    for (const auto& [name, value] : analog_map) {
      Pad::AnalogMappingInfo analog_mapping;
      if (keyboard_analogs_json[name].contains("Axis Id") == true) {
        analog_mapping.mode = Pad::AnalogMappingMode::AnalogInput;
        analog_mapping.axis_id = keyboard_analogs_json[name]["Axis Id"].get<int>();
        g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value] =
            analog_mapping;
        continue;
      }

      if (keyboard_analogs_json[name].contains("Positive Key") == true) {
        analog_mapping.positive_key = keyboard_analogs_json[name]["Positive Key"].get<int>();
      } else {
        lg::warn("Keyboard analog override not found for {}. Using keyboard default value: {}",
                 name,
                 g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value]
                     .positive_key);
      }

      if (keyboard_analogs_json[name].contains("Negative Key") == true) {
        analog_mapping.negative_key = keyboard_analogs_json[name]["Negative Key"].get<int>();
      } else {
        lg::warn("Keyboard analog override not found for {}. Using keyboard default value: {}",
                 name,
                 g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value]
                     .negative_key);
      }
      g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value] =
          analog_mapping;
    }
    g_settings.pad_mapping_info.mouse_x_axis_sensitivities[controller_index] =
        peripheral["X-Axis Mouse Sensitivity"].get<double>();
    g_settings.pad_mapping_info.mouse_y_axis_sensitivities[controller_index] =
        peripheral["Y-Axis Mouse Sensitivity"].get<double>();
    controller_index++;
  }
}

void LoadSettings() {
  // load controller settings
  // TODO - make this game specific as well
  auto controller_settings_filename =
      file_util::get_user_config_dir() / "controller" / "controller-settings.json";
  if (fs::exists(controller_settings_filename)) {
    LoadPeripheralSettings(controller_settings_filename);
    lg::info("Loaded controller configuration file.");
  } else {
    SavePeripheralSettings();
    lg::info(
        "Couldn't find $USER/controller/controller-settings.json creating new controller settings "
        "file.");
  }
  // load debug settings
  auto debug_settings_filename =
      file_util::get_user_misc_dir(g_game_version) / "debug-settings.json";
  if (fs::exists(debug_settings_filename)) {
    g_debug_settings.load_settings(debug_settings_filename);
    lg::info("Loaded debug settings file.");
  } else {
    lg::info("Couldn't find $USER/misc/debug-settings.json creating new controller settings file.");
    g_debug_settings.save_settings();
  }
}

const GfxRendererModule* GetRenderer(GfxPipeline pipeline) {
  switch (pipeline) {
    case GfxPipeline::Invalid:
      lg::error("Requested invalid renderer", fmt::underlying(pipeline));
      return NULL;
    case GfxPipeline::OpenGL:
      return &gRendererOpenGL;
    default:
      lg::error("Requested unknown renderer {}", fmt::underlying(pipeline));
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

void register_vsync_callback(std::function<void()> f) {
  vsync_callback = std::move(f);
}

void clear_vsync_callback() {
  vsync_callback = nullptr;
}

u32 vsync() {
  if (GetCurrentRenderer()) {
    // Inform the IOP kernel that we're vsyncing so it can run the vblank handler
    if (vsync_callback != nullptr)
      vsync_callback();
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

std::tuple<double, double> get_mouse_pos() {
  if (Display::GetMainDisplay()) {
    return Display::GetMainDisplay()->get_mouse_pos();
  } else {
    return {0, 0};
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

    SavePeripheralSettings();
  }
}

s64 get_mapped_button(s64 pad, s64 button) {
  if (pad < 0 || pad > Pad::CONTROLLER_COUNT || button < 0 || button > 16) {
    lg::error("Invalid parameters to get_mapped_button({}, {})", pad, button);
    return -1;
  }

  return (Pad::GetGamepadState(pad) > -1)
             ? (s64)g_settings.pad_mapping_info.controller_button_mapping[pad][button]
             : (s64)g_settings.pad_mapping_info.keyboard_button_mapping[pad][button];
}

int PadIsPressed(Pad::Button button, int port) {
  return Pad::IsPressed(g_settings.pad_mapping_info, button, port);
}

int PadGetAnalogValue(Pad::Analog analog, int port) {
  return Pad::GetAnalogValue(g_settings.pad_mapping_info, analog, port);
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
      lg::error("Invalid tree {} specified for SetLod ({})", fmt::underlying(tree), lod);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
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
      lg::error("{} invalid params {} {}", __PRETTY_FUNCTION__, fmt::underlying(mode), mask_id);
      break;
  }
}

void CollisionRendererSetMode(GfxGlobalSettings::CollisionRendererMode mode) {
  g_global_settings.collision_mode = mode;
}

}  // namespace Gfx
