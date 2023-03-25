#pragma once

#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/tools/filter_menu/filter_menu.h"

namespace GameSettings {
struct DebugSettings {
  bool show_imgui = false;
  int imgui_font_size = 14;
  bool monospaced_font = true;
  bool alternate_style = false;
  bool ignore_hide_imgui = false;

  std::vector<DebugTextFilter> text_filters = {};
  bool text_check_range = false;
  float text_max_range = 0;

  void load_settings();
  void save_settings();
};
void to_json(json& j, const DebugSettings& obj);
void from_json(const json& j, DebugSettings& obj);

struct DisplaySettings {
  int window_xpos = 50;
  int window_ypos = 50;
  int display_id = 0;

  void load_settings();
  void save_settings();
};

void to_json(json& j, const DisplaySettings& obj);
void from_json(const json& j, DisplaySettings& obj);
}  // namespace GameSettings
