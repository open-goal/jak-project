#pragma once

#include "common/util/FileUtil.h"

#include "game/tools/filter_menu/filter_menu.h"

struct DebugSettings {
  // TODO - configurable debug text font size
  bool show_imgui = false;
  bool ignore_imgui_hide_keybind = false;
  std::vector<DebugTextFilter> debug_text_filters = {};
  bool debug_text_check_range = false;
  float debug_text_max_range = 0;

  void load_settings(const ghc::filesystem::path& filepath);
  void save_settings();
};
