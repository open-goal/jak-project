#include "settings.h"

#include "common/log/log.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

namespace game_settings {

void to_json(json& j, const DebugSettings& obj) {
  json_serialize(version);
  json_serialize(show_imgui);
  json_serialize(imgui_font_size);
  json_serialize(monospaced_font);
  json_serialize(alternate_style);
  json_serialize(ignore_hide_imgui);
  json_serialize(text_filters);
  json_serialize(text_check_range);
  json_serialize(text_max_range);
  json_serialize(hide_imgui_key);
}

void from_json(const json& j, DebugSettings& obj) {
  json_deserialize_if_exists(version);
  json_deserialize_if_exists(show_imgui);
  json_deserialize_if_exists(imgui_font_size);
  json_deserialize_if_exists(monospaced_font);
  json_deserialize_if_exists(alternate_style);
  json_deserialize_if_exists(ignore_hide_imgui);
  json_deserialize_if_exists(text_filters);
  json_deserialize_if_exists(text_check_range);
  json_deserialize_if_exists(text_max_range);
  json_deserialize_if_exists(hide_imgui_key);
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
    // TODO - make sure a bad debug-settings.json is fine, check others below as well
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
  json_deserialize_if_exists(version);
  json_deserialize_if_exists(display_id);
  json_deserialize_if_exists(window_xpos);
  json_deserialize_if_exists(window_ypos);
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
           {"last_selected_controller_guid", obj.last_selected_controller_guid},
           {"controller_port_mapping", obj.controller_port_mapping},
           {"controller_binds", obj.controller_binds},
           {"keyboard_binds", obj.keyboard_binds},
           {"mouse_binds", obj.mouse_binds}};
}

void from_json(const json& j, InputSettings& obj) {
  json_deserialize_if_exists(version);
  json_deserialize_if_exists(last_selected_controller_guid);
  json_deserialize_if_exists(controller_port_mapping);
  json_deserialize_if_exists(controller_binds);
  json_deserialize_if_exists(keyboard_binds);
  json_deserialize_if_exists(mouse_binds);
}

InputSettings::InputSettings() {
  try {
    keyboard_binds = DEFAULT_KEYBOARD_BINDS;
    mouse_binds = DEFAULT_MOUSE_BINDS;
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "input-settings.json").string();
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
