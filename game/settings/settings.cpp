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

DebugSettings::DebugSettings() {}

void DebugSettings::load_settings() {
  try {
    std::string file_path =
        (file_util::get_user_misc_dir(g_game_version) / "debug-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading debug settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    from_json(parse_commented_json(raw, "debug-settings.json"), *this);
  } catch (std::exception& e) {
    // do nothing
    lg::error("Error encountered when attempting to load debug settings {}", e.what());
    // TODO - make sure a bad debug-settings.json is fine, check others below as well
  }
}

void DebugSettings::save_settings() {
  // Update the version string as we are now saving it back ground
  version = current_version;
  json data = *this;
  auto debug_settings_filename =
      file_util::get_user_misc_dir(g_game_version) / "debug-settings.json";
  file_util::create_dir_if_needed_for_file(debug_settings_filename);
  file_util::write_text_file(debug_settings_filename, data.dump(2));
}

void to_json(json& j, const DisplaySettings& obj) {
  json_serialize(version);
  json_serialize(display_id);
  json_serialize(window_xpos);
  json_serialize(window_ypos);
  json_serialize(display_mode);
}
void from_json(const json& j, DisplaySettings& obj) {
  json_deserialize_if_exists(version);
  json_deserialize_if_exists(display_id);
  json_deserialize_if_exists(window_xpos);
  json_deserialize_if_exists(window_ypos);
  if (j.contains("display_mode")) {
    int mode = j.at("display_mode");
    obj.display_mode = static_cast<DisplaySettings::DisplayMode>(mode);
  }
}

DisplaySettings::DisplaySettings() {}

void DisplaySettings::load_settings() {
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
  // Update the version string as we are now saving it back ground
  version = current_version;
  json data = *this;
  auto file_path = file_util::get_user_settings_dir(g_game_version) / "display-settings.json";
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, data.dump(2));
}

void to_json(json& j, const InputSettings& obj) {
  json_serialize(version);
  json_serialize(last_selected_controller_guid);
  json_serialize(controller_port_mapping);
  json_serialize(controller_binds);
  json_serialize(keyboard_binds);
  json_serialize(mouse_binds);
  json_serialize(keyboard_enabled);
}

void from_json(const json& j, InputSettings& obj) {
  json_deserialize_if_exists(version);
  json_deserialize_if_exists(last_selected_controller_guid);
  json_deserialize_if_exists(controller_port_mapping);
  json_deserialize_if_exists(controller_binds);
  json_deserialize_if_exists(keyboard_binds);
  json_deserialize_if_exists(mouse_binds);
  json_deserialize_if_exists(keyboard_enabled);
}

InputSettings::InputSettings() {}

void InputSettings::load_settings() {
  try {
    keyboard_binds = DEFAULT_KEYBOARD_BINDS;
    mouse_binds = DEFAULT_MOUSE_BINDS;
    std::string file_path =
        (file_util::get_user_settings_dir(g_game_version) / "input-settings.json").string();
    if (!file_util::file_exists(file_path)) {
      return;
    }
    lg::info("Loading input settings at {}", file_path);
    auto raw = file_util::read_text_file(file_path);
    from_json(parse_commented_json(raw, "input-settings.json"), *this);
  } catch (std::exception& e) {
    // do nothing
    lg::error("Error encountered when attempting to load input settings {}", e.what());
  }
}

void InputSettings::save_settings() {
  // Update the version string as we are now saving it back ground
  version = current_version;
  json data = *this;
  auto file_path = file_util::get_user_settings_dir(g_game_version) / "input-settings.json";
  file_util::create_dir_if_needed_for_file(file_path);
  file_util::write_text_file(file_path, data.dump(2));
}
}  // namespace game_settings
