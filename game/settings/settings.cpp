#include "settings.h"

#include "common/log/log.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

namespace game_settings {

void to_json(json& j, const DebugSettings& obj) {
  j = json{{"version", obj.version},
           {"show_imgui", obj.show_imgui},
           {"imgui_font_size", obj.imgui_font_size},
           {"monospaced_font", obj.monospaced_font},
           {"alternate_style", obj.alternate_style},
           {"ignore_hide_imgui", obj.ignore_hide_imgui},
           {"text_filters", obj.text_filters},
           {"text_check_range", obj.text_check_range},
           {"text_max_range", obj.text_max_range}};
}

void from_json(const json& j, DebugSettings& obj) {
  json_safe_deserialize(version);
  json_safe_deserialize(show_imgui);
  json_safe_deserialize(imgui_font_size);
  json_safe_deserialize(monospaced_font);
  json_safe_deserialize(alternate_style);
  json_safe_deserialize(ignore_hide_imgui);
  json_safe_deserialize(text_filters);
  json_safe_deserialize(text_check_range);
  json_safe_deserialize(text_max_range);
}

DebugSettings::DebugSettings() {
  try {
    std::string file_path =
        (file_util::get_user_misc_dir(g_game_version) / "debug-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading display settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    from_json(parse_commented_json(raw, "debug-settings.json"), *this);
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
  json_safe_deserialize(version);
  json_safe_deserialize(display_id);
  json_safe_deserialize(window_xpos);
  json_safe_deserialize(window_ypos);
}

DisplaySettings::DisplaySettings() {
  try {
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "display-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading display settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    from_json(parse_commented_json(raw, "display-settings.json"), *this);
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

void to_json(json& j, const InputSettings& obj) {
  j = json{{"version", obj.version},
           {"controller_port_mapping", obj.controller_port_mapping},
           {"controller_binds", obj.controller_binds},
           {"keyboard_binds", obj.keyboard_binds},
           {"mouse_binds", obj.mouse_binds}};
}

void from_json(const json& j, InputSettings& obj) {
  json_safe_deserialize(version);
  json_safe_deserialize(controller_port_mapping);
  json_safe_deserialize(controller_binds);
  if (j.contains("keyboard_binds")) {
    j.at("keyboard_binds").get_to(obj.keyboard_binds);
  } else {
    obj.keyboard_binds = DEFAULT_KEYBOARD_BINDS;
  }
  if (j.contains("mouse_binds")) {
    j.at("mouse_binds").get_to(obj.mouse_binds);
  } else {
    obj.mouse_binds = DEFAULT_MOUSE_BINDS;
  }
}

InputSettings::InputSettings() {
  try {
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "display-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading display settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    from_json(parse_commented_json(raw, "input-settings.json"), *this);
  } catch (std::exception& e) {
    // do nothing
    lg::error("Error encountered when attempting to load input settings {}", e.what());
  }
}
void InputSettings::save_settings() {
  json data = *this;
  auto file_path = file_util::get_user_settings_dir(g_game_version) / "input-settings.json";
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, data.dump(2));
}
}  // namespace game_settings
