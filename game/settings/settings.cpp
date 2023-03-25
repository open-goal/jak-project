#include "settings.h"

#include "common/log/log.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

namespace GameSettings {

void to_json(json& j, const DebugSettings& obj) {
  j = json{{"show_imgui", obj.show_imgui},
           {"imgui_font_size", obj.imgui_font_size},
           {"monospaced_font", obj.monospaced_font},
           {"alternate_style", obj.alternate_style},
           {"ignore_hide_imgui", obj.ignore_hide_imgui},
           {"text_filters", obj.text_filters},
           {"text_check_range", obj.text_check_range},
           {"text_max_range", obj.text_max_range}};
}

void from_json(const json& j, DebugSettings& obj) {
  if (j.contains("show_imgui")) {
    j.at("show_imgui").get_to(obj.show_imgui);
  }
  if (j.contains("imgui_font_size")) {
    j.at("imgui_font_size").get_to(obj.imgui_font_size);
  }
  if (j.contains("monospaced_font")) {
    j.at("monospaced_font").get_to(obj.monospaced_font);
  }
  if (j.contains("alternate_style")) {
    j.at("alternate_style").get_to(obj.alternate_style);
  }
  if (j.contains("ignore_hide_imgui")) {
    j.at("ignore_hide_imgui").get_to(obj.ignore_hide_imgui);
  }
  if (j.contains("text_filters")) {
    j.at("text_filters").get_to(obj.text_filters);
  }
  if (j.contains("text_check_range")) {
    j.at("text_check_range").get_to(obj.text_check_range);
  }
  if (j.contains("text_max_range")) {
    j.at("text_max_range").get_to(obj.text_max_range);
  }
}

void DebugSettings::load_settings() {
  try {
    std::string file_path =
        (file_util::get_user_misc_dir(g_game_version) / "debug-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading display settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    DebugSettings saved_settings = parse_commented_json(raw, "debug-settings.json");
    show_imgui = saved_settings.show_imgui;
    imgui_font_size = saved_settings.imgui_font_size;
    monospaced_font = saved_settings.monospaced_font;
    alternate_style = saved_settings.alternate_style;
    ignore_hide_imgui = saved_settings.ignore_hide_imgui;
    text_filters = saved_settings.text_filters;
    text_check_range = saved_settings.text_check_range;
    text_max_range = saved_settings.text_max_range;
  } catch (std::exception& e) {
    // do nothing
    lg::error("Error encountered when attempting to load debug settings {}", e.what());
  }
}

void DebugSettings::save_settings() {
  json data = *this;
  auto debug_settings_filename =
      file_util::get_user_misc_dir(g_game_version) / "debug-settings.json";
  file_util::create_dir_if_needed_for_file(debug_settings_filename);
  file_util::write_text_file(debug_settings_filename, data.dump(2));
}

void to_json(json& j, const DisplaySettings& obj) {
  j = json{{"display_id", obj.display_id},
           {"window_xpos", obj.window_xpos},
           {"window_ypos", obj.window_ypos}};
}
void from_json(const json& j, DisplaySettings& obj) {
  if (j.contains("display_id")) {
    j.at("display_id").get_to(obj.display_id);
  }
  if (j.contains("window_xpos")) {
    j.at("window_xpos").get_to(obj.window_xpos);
  }
  if (j.contains("window_ypos")) {
    j.at("window_ypos").get_to(obj.window_ypos);
  }
}

void DisplaySettings::load_settings() {
  try {
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "display-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading display settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    DisplaySettings saved_settings = parse_commented_json(raw, "display-settings.json");
    display_id = saved_settings.display_id;
    window_xpos = saved_settings.window_xpos;
    window_ypos = saved_settings.window_ypos;
  } catch (std::exception& e) {
    // do nothing
    lg::error("Error encountered when attempting to load display settings {}", e.what());
  }
}

void DisplaySettings::save_settings() {
  json data = *this;
  auto file_path = file_util::get_user_settings_dir(g_game_version) / "display-settings.json";
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, data.dump(2));
}
}  // namespace GameSettings
