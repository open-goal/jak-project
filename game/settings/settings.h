#pragma once

#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/system/hid/input_bindings.h"
#include "game/system/hid/sdl_util.h"
#include "game/tools/filter_menu/filter_menu.h"

namespace game_settings {
struct DebugSettings {
  DebugSettings();

  std::string version = "1.2";

  bool show_imgui = false;
  int imgui_font_size = 16;
  bool monospaced_font = true;
  bool alternate_style = false;
  bool ignore_hide_imgui = false;
  bool treat_pad0_as_pad1 = false;

  std::vector<DebugTextFilter> text_filters = {};
  bool text_check_range = false;
  float text_max_range = 0;
  u32 hide_imgui_key = SDLK_LALT;

  void save_settings();
};
void to_json(json& j, const DebugSettings& obj);
void from_json(const json& j, DebugSettings& obj);

struct DisplaySettings {
  DisplaySettings();

  std::string version = "1.1";

  int window_xpos = 50;
  int window_ypos = 50;
  int display_id = 0;

  void save_settings();
};

void to_json(json& j, const DisplaySettings& obj);
void from_json(const json& j, DisplaySettings& obj);

struct InputSettings {
  InputSettings();

  std::string version = "1.0";

  // NOTE - assumes only port 0
  std::string last_selected_controller_guid = "";
  std::unordered_map<std::string, int> controller_port_mapping;
  std::unordered_map<std::string, InputBindingGroups> controller_binds;
  InputBindingGroups keyboard_binds;
  InputBindingGroups mouse_binds;

  void save_settings();
};

void to_json(json& j, const InputSettings& obj);
void from_json(const json& j, InputSettings& obj);

}  // namespace game_settings
