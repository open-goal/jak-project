#include "settings.h"

#include "common/util/json_util.h"

#include "game/runtime.h"

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

// Pad::MappingInfo& get_button_mapping() {
//   return g_settings.pad_mapping_info;
// }

// const std::vector<const GfxRendererModule*> renderers = {&moduleOpenGL};

// Not crazy about this declaration
// const std::pair<std::string, Pad::Button> gamepad_map[] = {{"Select", Pad::Button::Select},
//                                                           {"L3", Pad::Button::L3},
//                                                           {"R3", Pad::Button::R3},
//                                                           {"Start", Pad::Button::Start},
//                                                           {"Up", Pad::Button::Up},
//                                                           {"Right", Pad::Button::Right},
//                                                           {"Down", Pad::Button::Down},
//                                                           {"Left", Pad::Button::Left},
//                                                           {"L1", Pad::Button::L1},
//                                                           {"R1", Pad::Button::R1},
//                                                           {"Triangle", Pad::Button::Triangle},
//                                                           {"Circle", Pad::Button::Circle},
//                                                           {"X", Pad::Button::X},
//                                                           {"Square", Pad::Button::Square}};
//
// const std::pair<std::string, Pad::Analog> analog_map[] = {
//    {"Left X Axis", Pad::Analog::Left_X},
//    {"Left Y Axis", Pad::Analog::Left_Y},
//    {"Right X Axis", Pad::Analog::Right_X},
//    {"Right Y Axis", Pad::Analog::Right_Y},
//};

// void DumpToJson(ghc::filesystem::path& filename) {
//   nlohmann::json json;
//   auto& peripherals_json = json["Peripherals"];
//   /*json["Use Mouse"] = g_settings.pad_mapping_info.use_mouse;
//
//   for (uint32_t i = 0; i < Pad::CONTROLLER_COUNT; ++i) {
//     nlohmann::json peripheral_json;
//     peripheral_json["ID"] = i + 1;
//
//     auto& controller_json = peripheral_json["Controller"];
//     auto& controller_buttons_json = controller_json["Buttons"];
//     for (const auto& [name, value] : gamepad_map) {
//       controller_buttons_json[name] =
//           g_settings.pad_mapping_info.controller_button_mapping[i][(int)value];
//     }
//
//     auto& keyboard_json = peripheral_json["Keyboard+Mouse"];
//     auto& keyboard_buttons_json = keyboard_json["Buttons"];
//     for (const auto& [name, value] : gamepad_map) {
//       keyboard_buttons_json[name] =
//           g_settings.pad_mapping_info.keyboard_button_mapping[i][(int)value];
//     }
//
//     auto& keyboard_analogs_json = keyboard_json["Analog"];
//     for (const auto& [name, value] : analog_map) {
//       if (g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].mode ==
//           Pad::AnalogMappingMode::AnalogInput) {
//         keyboard_analogs_json[name]["Axis Id"] =
//             g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].axis_id;
//       } else {
//         keyboard_analogs_json[name]["Positive Key"] =
//             g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].positive_key;
//         keyboard_analogs_json[name]["Negative Key"] =
//             g_settings.pad_mapping_info.keyboard_analog_mapping[i][(int)value].negative_key;
//       }
//     }
//     peripheral_json["X-Axis Mouse Sensitivity"] =
//         g_settings.pad_mapping_info.mouse_x_axis_sensitivities[i];
//     peripheral_json["Y-Axis Mouse Sensitivity"] =
//         g_settings.pad_mapping_info.mouse_y_axis_sensitivities[i];
//     peripherals_json.emplace_back(peripheral_json);
//   }*/
//
//   file_util::write_text_file(filename, json.dump(4));
// }
//
// void SavePeripheralSettings() {
//   auto filename = (file_util::get_user_config_dir() / "controller" / "controller-settings.json");
//   file_util::create_dir_if_needed_for_file(filename);
//
//   DumpToJson(filename);
//   lg::info("Saved graphics configuration file.");
// }
//
// void LoadPeripheralSettings(const ghc::filesystem::path& filepath) {
//   /*Pad::DefaultMapping(g_settings.pad_mapping_info);
//
//   lg::info("reading {}", filepath.string());
//   auto file_txt = file_util::read_text_file(filepath);
//   auto configuration = parse_commented_json(file_txt, filepath.string());
//
//   if (configuration.find("Use Mouse") != configuration.end()) {
//     g_settings.pad_mapping_info.use_mouse = configuration["Use Mouse"].get<bool>();
//   }
//   int controller_index = 0;
//   for (const auto& peripheral : configuration["Peripherals"]) {
//     auto& controller_buttons_json = peripheral["Controller"]["Buttons"];
//     auto& keyboard_buttons_json = peripheral["Keyboard+Mouse"]["Buttons"];
//
//     for (const auto& [name, button] : gamepad_map) {
//       if (controller_buttons_json.find(name) != controller_buttons_json.end()) {
//         g_settings.pad_mapping_info.controller_button_mapping[controller_index][(int)button] =
//             controller_buttons_json[name].get<int>();
//       } else {
//         lg::warn(
//             "Controller button override not found for {}. Using controller default value: {}",
//             name,
//             g_settings.pad_mapping_info.controller_button_mapping[controller_index][(int)button]);
//       }
//
//       if (keyboard_buttons_json.find(name) != keyboard_buttons_json.end()) {
//         g_settings.pad_mapping_info.keyboard_button_mapping[controller_index][(int)button] =
//             keyboard_buttons_json[name].get<int>();
//       } else {
//         lg::warn(
//             "Keyboard button override not found for {}. Using keyboard default value: {}", name,
//             g_settings.pad_mapping_info.keyboard_button_mapping[controller_index][(int)button]);
//       }
//     }
//
//     auto& keyboard_analogs_json = peripheral["Keyboard+Mouse"]["Analog"];
//     for (const auto& [name, value] : analog_map) {
//       Pad::AnalogMappingInfo analog_mapping;
//       if (keyboard_analogs_json[name].contains("Axis Id") == true) {
//         analog_mapping.mode = Pad::AnalogMappingMode::AnalogInput;
//         analog_mapping.axis_id = keyboard_analogs_json[name]["Axis Id"].get<int>();
//         g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value] =
//             analog_mapping;
//         continue;
//       }
//
//       if (keyboard_analogs_json[name].contains("Positive Key") == true) {
//         analog_mapping.positive_key = keyboard_analogs_json[name]["Positive Key"].get<int>();
//       } else {
//         lg::warn("Keyboard analog override not found for {}. Using keyboard default value: {}",
//                  name,
//                  g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value]
//                      .positive_key);
//       }
//
//       if (keyboard_analogs_json[name].contains("Negative Key") == true) {
//         analog_mapping.negative_key = keyboard_analogs_json[name]["Negative Key"].get<int>();
//       } else {
//         lg::warn("Keyboard analog override not found for {}. Using keyboard default value: {}",
//                  name,
//                  g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value]
//                      .negative_key);
//       }
//       g_settings.pad_mapping_info.keyboard_analog_mapping[controller_index][(int)value] =
//           analog_mapping;
//     }
//     g_settings.pad_mapping_info.mouse_x_axis_sensitivities[controller_index] =
//         peripheral["X-Axis Mouse Sensitivity"].get<double>();
//     g_settings.pad_mapping_info.mouse_y_axis_sensitivities[controller_index] =
//         peripheral["Y-Axis Mouse Sensitivity"].get<double>();
//     controller_index++;
//   }*/
// }
//
// void LoadSettings() {
//   // load controller settings
//   // TODO - make this game specific as well
//   auto controller_settings_filename =
//       file_util::get_user_config_dir() / "controller" / "controller-settings.json";
//   if (fs::exists(controller_settings_filename)) {
//     LoadPeripheralSettings(controller_settings_filename);
//     lg::info("Loaded controller configuration file.");
//   } else {
//     SavePeripheralSettings();
//     lg::info(
//         "Couldn't find $USER/controller/controller-settings.json creating new controller settings
//         " "file.");
//   }
//   // load debug settings
//   auto debug_settings_filename =
//       file_util::get_user_misc_dir(g_game_version) / "debug-settings.json";
//   if (fs::exists(debug_settings_filename)) {
//     g_debug_settings.load_settings(debug_settings_filename);
//     lg::info("Loaded debug settings file.");
//   } else {
//     lg::info("Couldn't find $USER/misc/debug-settings.json creating new controller settings
//     file."); g_debug_settings.save_settings();
//   }
// }
