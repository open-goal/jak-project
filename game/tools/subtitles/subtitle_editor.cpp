#include "subtitle_editor.h"

#include <regex>
#include <string_view>

#include "common/deserialization/subtitles/subtitles.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

SubtitleEditor::SubtitleEditor() : m_repl(8181) {
  update_subtitle_editor_db();
  m_subtitle_db = load_subtitle_project();
  m_filter = m_filter_placeholder;
  m_filter_hints = m_filter_placeholder;
}

void SubtitleEditor::repl_set_continue_point(const std::string_view& continue_point) {
  m_repl.eval(
      fmt::format("(start 'play (get-continue-by-name *game-info* \"{}\"))", continue_point));
}

void SubtitleEditor::repl_move_jak(const double x, const double y, const double z) {
  m_repl.eval(
      fmt::format("(move-to-point! (-> *target* control) (new 'static 'vector :x (meters {:.1f}) "
                  ":y (meters {:.1f}) :z (meters {:.1f})))",
                  x, y, z));
  m_repl.eval("(send-event *camera* 'teleport)");
}

void SubtitleEditor::repl_reset_game() {
  m_repl.eval("(set! (-> *game-info* mode) 'debug)");
  m_repl.eval("(initialize! *game-info* 'game (the-as game-save #f) (the-as string #f))");
}

std::string SubtitleEditor::repl_get_process_string(const std::string_view& entity_type,
                                                    const std::string_view& process_name) {
  return fmt::format("(the-as {} (process-by-name \"{}\" *active-pool*))", entity_type,
                     process_name);
}

void SubtitleEditor::repl_play_hint(const std::string_view& hint_name) {
  repl_reset_game();
  // repl_set_continue_point("village1-hut");
  // TODO - move into water fountain
  m_repl.eval(
      fmt::format("(level-hint-spawn (game-text-id zero) \"{}\" (the-as entity #f) *entity-pool* "
                  "(game-task none))",
                  hint_name));
}

void SubtitleEditor::repl_execute_cutscene_code(const SubtitleEditorDB::Entry& entry) {
  // Reset the game first to get to a known state
  repl_reset_game();

  if (entry.move_first) {
    // Set Jak's Continue Point
    if (!entry.continue_name.empty()) {
      repl_set_continue_point(entry.continue_name);
    }
    // Move Jak into position
    if (!entry.move_to.empty()) {
      repl_move_jak(entry.move_to[0], entry.move_to[1], entry.move_to[2]);
    }
  }

  // Run any requirements to setup the task state
  if (!entry.requirements.empty()) {
    // Replace __GET-PROCESS__
    for (const auto& form : entry.requirements) {
      std::string temp = form;
      temp = std::regex_replace(temp, std::regex("__GET-PROCESS__"),
                                repl_get_process_string(entry.entity_type, entry.process_name));
      m_repl.eval(temp);
    }
  }

  if (!entry.move_first) {
    // Set Jak's Continue Point
    if (!entry.continue_name.empty()) {
      repl_set_continue_point(entry.continue_name);
    }
    // Move Jak into position
    if (!entry.move_to.empty()) {
      repl_move_jak(entry.move_to[0], entry.move_to[1], entry.move_to[2]);
    }
  }

  // Execute the critical code - typically this means sending a 'play-anim event to the
  // process-taskable in question
  if (!entry.execute_code.empty()) {
    std::string temp = entry.execute_code;
    temp = std::regex_replace(temp, std::regex("__GET-PROCESS__"),
                              repl_get_process_string(entry.entity_type, entry.process_name));
    m_repl.eval("(send-event *camera* 'teleport)");
    if (entry.delay_frames == 0) {
      m_repl.eval(temp);
    } else {
      // We do this in a separate thread to introduce a delay -- allow the game to catch up before
      // running the critical section
      auto code =
          fmt::format("(process-spawn-function process (lambda () (dotimes (i {}) (suspend)) {}))",
                      entry.delay_frames, temp);
      m_repl.eval(code);
    }
  }
}

void SubtitleEditor::repl_rebuild_text() {
  m_repl.eval("(make-text)");
  // increment the language id of the in-memory text file so that it won't match the current
  // language and the game will want to reload it asap
  m_repl.eval("(1+! (-> *subtitle-text* lang))");
}

bool SubtitleEditor::is_scene_in_current_lang(const std::string& scene_name) {
  return m_subtitle_db.m_banks.at(m_current_language)->m_scenes.count(scene_name) > 0;
}

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");

  if (ImGui::Button("Save Changes")) {
    m_files_saved_successfully = std::make_optional(write_subtitle_db_to_files(m_subtitle_db));
    repl_rebuild_text();
  }
  if (m_files_saved_successfully.has_value()) {
    ImGui::SameLine();
    if (m_files_saved_successfully.value()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_success_text_color);
      ImGui::Text("Saved!");
      ImGui::PopStyleColor();
    } else {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Error!");
      ImGui::PopStyleColor();
    }
  }

  draw_edit_options();
  draw_repl_options();

  if (!m_current_scene) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
  } else {
    ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
  }
  if (ImGui::TreeNode("Currently Selected Cutscene")) {
    ImGui::PopStyleColor();
    if (m_current_scene) {
      draw_subtitle_options(*m_current_scene, true);
    } else {
      ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 0, 0, 255));
      ImGui::Text("Select a Scene from Below!");
      ImGui::PopStyleColor();
    }
    ImGui::TreePop();
  } else {
    ImGui::PopStyleColor();
  }

  if (ImGui::TreeNode("All Cutscenes")) {
    ImGui::InputText("New Scene Name", &m_new_scene_name);
    if (ImGui::BeginCombo("Sorting Group", m_new_scene_group.c_str())) {
      for (size_t i = 0; i < m_subtitle_db.m_subtitle_groups->m_group_order.size(); ++i) {
        if (ImGui::Selectable(m_subtitle_db.m_subtitle_groups->m_group_order[i].c_str())) {
          m_new_scene_group = m_subtitle_db.m_subtitle_groups->m_group_order[i];
        }
      }
      ImGui::EndCombo();
    }
    ImGui::InputText("Filter", &m_filter, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    if (is_scene_in_current_lang(m_new_scene_name)) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Scene already exists with that name, no!");
      ImGui::PopStyleColor();
    }
    if (m_new_scene_group.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("You must provide a group to sort the scene into!");
      ImGui::PopStyleColor();
    }
    if (!is_scene_in_current_lang(m_new_scene_name) && !m_new_scene_name.empty() &&
        !m_new_scene_group.empty()) {
      if (ImGui::Button("Add Scene")) {
        GameSubtitleSceneInfo newScene(SubtitleSceneKind::Movie);
        newScene.m_name = m_new_scene_name;
        newScene.m_id = 0;  // TODO - id is always zero, bug in subtitles.cpp?
        newScene.m_sorting_group = m_new_scene_group;
        m_subtitle_db.m_banks.at(m_current_language)->add_scene(newScene);
        m_subtitle_db.m_subtitle_groups->add_scene(newScene.m_sorting_group, newScene.m_name);
        if (m_add_new_scene_as_current) {
          auto& scenes = m_subtitle_db.m_banks.at(m_current_language)->m_scenes;
          auto& scene_info = scenes.at(m_new_scene_name);
          m_current_scene = &scene_info;
        }
        m_new_scene_name = "";
      }
      ImGui::SameLine();
      ImGui::Checkbox("Add as Current Scene", &m_add_new_scene_as_current);
      ImGui::NewLine();
    }

    draw_all_cutscene_groups();
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("All Hints")) {
    ImGui::InputText("New Scene Name", &m_new_scene_name);
    ImGui::InputText("New Scene ID (hex)", &m_new_scene_id);
    if (ImGui::BeginCombo("Sorting Group", m_new_scene_group.c_str())) {
      for (size_t i = 0; i < m_subtitle_db.m_subtitle_groups->m_group_order.size(); ++i) {
        if (ImGui::Selectable(m_subtitle_db.m_subtitle_groups->m_group_order[i].c_str())) {
          m_new_scene_group = m_subtitle_db.m_subtitle_groups->m_group_order[i];
        }
      }
      ImGui::EndCombo();
    }
    ImGui::InputText("Filter", &m_filter_hints,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    if (is_scene_in_current_lang(m_new_scene_name)) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Scene already exists with that name, no!");
      ImGui::PopStyleColor();
    }
    if (m_new_scene_group.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("You must provide a group to sort the scene into!");
      ImGui::PopStyleColor();
    }
    if (!is_scene_in_current_lang(m_new_scene_name) && !m_new_scene_name.empty() &&
        !m_new_scene_group.empty()) {
      if (ImGui::Button("Add Scene")) {
        GameSubtitleSceneInfo newScene(SubtitleSceneKind::Hint);
        newScene.m_name = m_new_scene_name;
        if (m_new_scene_id == "0") {
          newScene.m_kind = SubtitleSceneKind::Hint;
          newScene.m_id = strtoul(m_new_scene_id.c_str(), nullptr, 16);
        } else {
          newScene.m_kind = SubtitleSceneKind::HintNamed;
          newScene.m_id = strtoul(m_new_scene_id.c_str(), nullptr, 16);
        }
        // currently hints have no way in the editor to add a line, so give us one for free
        newScene.add_line(0, "", "", false);
        newScene.m_sorting_group = m_new_scene_group;
        m_subtitle_db.m_banks.at(m_current_language)->add_scene(newScene);
        m_subtitle_db.m_subtitle_groups->add_scene(newScene.m_sorting_group, newScene.m_name);
        m_new_scene_name = "";
      }
    }

    draw_all_hint_groups();
    ImGui::TreePop();
  }

  ImGui::End();
}

void SubtitleEditor::update_subtitle_editor_db() {
  std::string db_path = (file_util::get_jak_project_dir() / "game" / "assets" / "jak1" /
                         "subtitle" / "subtitle-editor-db.json")
                            .string();
  auto config_str = file_util::read_text_file(db_path);
  auto db_data = parse_commented_json(config_str, db_path);

  for (const auto& [key, val] : db_data.items()) {
    auto new_entry = SubtitleEditorDB::Entry();
    try {
      new_entry.entity_type = val.at("entity_type").get<std::string>();
      new_entry.process_name = val.at("process_name").get<std::string>();
      new_entry.continue_name = val.at("continue_name").get<std::string>();
      new_entry.move_to = val.at("move_to").get<std::vector<double>>();
      if (val.contains("delay")) {
        new_entry.delay_frames = val.at("delay").get<int>();
      } else {
        new_entry.delay_frames = 0;
      }
      if (val.contains("move_first")) {
        new_entry.move_first = val.at("move_first").get<bool>();
      } else {
        new_entry.move_first = false;
      }
      if (new_entry.move_to.size() != 0 && new_entry.move_to.size() != 3) {
        fmt::print("Bad subtitle db entry, provide 0 or 3 coordinates for 'move_to' - {}", key);
        continue;
      }
      new_entry.execute_code = val.at("execute_code").get<std::string>();
      new_entry.requirements = val.at("requirements").get<std::vector<std::string>>();
      if (m_db.count(key) == 0) {
        m_db.emplace(key, new_entry);
      } else {
        m_db[key] = new_entry;
      }

    } catch (std::exception& ex) {
      fmt::print("Bad subtitle db entry - {} - {}", key, ex.what());
    }
  }
}

void SubtitleEditor::draw_edit_options() {
  if (ImGui::TreeNode("Editing Options")) {
    if (ImGui::BeginCombo(
            "Editing Language ID",
            fmt::format("[{}] {}", m_subtitle_db.m_banks[m_current_language]->m_lang_id,
                        m_subtitle_db.m_banks[m_current_language]->file_path)
                .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_current_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->m_lang_id, value->file_path).c_str(),
                              isSelected)) {
          m_current_language = key;
        }
        if (isSelected) {
          ImGui::SetItemDefaultFocus();
        }
      }
      ImGui::EndCombo();
    }
    if (ImGui::BeginCombo("Base Language ID",
                          fmt::format("[{}] {}", m_subtitle_db.m_banks[m_base_language]->m_lang_id,
                                      m_subtitle_db.m_banks[m_base_language]->file_path)
                              .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_base_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->m_lang_id, value->file_path).c_str(),
                              isSelected)) {
          m_base_language = key;
        }
        if (isSelected) {
          ImGui::SetItemDefaultFocus();
        }
      }
      ImGui::EndCombo();
    }
    ImGui::Checkbox("Show missing cutscenes from base", &m_base_show_missing_cutscenes);
    ImGui::InputText("New Subtitle Group Name", &m_new_scene_group_name);
    if (!m_new_scene_group_name.empty()) {
      if (m_new_scene_group_name == "_groups" ||
          std::find(m_subtitle_db.m_subtitle_groups->m_group_order.begin(),
                    m_subtitle_db.m_subtitle_groups->m_group_order.end(), m_new_scene_group_name) !=
              m_subtitle_db.m_subtitle_groups->m_group_order.end()) {
        ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
        ImGui::Text("Invalid group name, has to be unique and not '_groups'");
        ImGui::PopStyleColor();
      } else if (ImGui::Button("Add New Group")) {
        m_subtitle_db.m_subtitle_groups->m_group_order.push_back(m_new_scene_group_name);
        m_new_scene_group_name = "";
      }
    }
    if (ImGui::Button("Update Editor DB")) {
      update_subtitle_editor_db();
    }
    ImGui::TreePop();
  }
}

void SubtitleEditor::draw_repl_options() {
  if (ImGui::TreeNode("REPL Options")) {
    // TODO - the ReplServer should eventually be able to return statuses to make this easier:
    // - Has the game been built before?
    // - Is the repl connected?
    ImGui::TextWrapped(
        "This tool requires a REPL connected to the game, with the game built. Run the following "
        "to do so:");
    ImGui::Text(" - `task repl`");
    ImGui::Text(" - `(lt)`");
    ImGui::Text(" - `(mi)`");
    ImGui::Text(" - Click Connect Below!");
    if (m_repl.is_connected()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_success_text_color);
      ImGui::Text("REPL Connected, should be good to go!");
      ImGui::PopStyleColor();
    } else {
      if (ImGui::Button("Connect to REPL")) {
        m_repl.connect();
        if (!m_repl.is_connected()) {
          ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
          ImGui::Text("Could not connect.");
          ImGui::PopStyleColor();
        }
      }
    }
    ImGui::TreePop();
  }
}

bool SubtitleEditor::any_cutscenes_in_group(const std::string& group_name) {
  auto& scenes = m_subtitle_db.m_banks.at(m_current_language)->m_scenes;
  auto scenes_in_group = m_subtitle_db.m_subtitle_groups->m_groups[group_name];
  for (auto& scene_name : scenes_in_group) {
    if (scenes.count(scene_name) != 0) {
      auto& scene_info = scenes.at(scene_name);
      if (scene_info.m_kind == SubtitleSceneKind::Movie) {
        return true;
      }
    }
  }
  return false;
}

void SubtitleEditor::draw_all_cutscene_groups() {
  for (auto& group_name : m_subtitle_db.m_subtitle_groups->m_group_order) {
    if (!m_filter.empty() && m_filter != m_filter_placeholder) {
      ImGui::SetNextItemOpen(true);
    }
    if (m_subtitle_db.m_subtitle_groups->m_groups.count(group_name) == 0 ||
        !any_cutscenes_in_group(group_name)) {
      continue;
    }
    if (ImGui::TreeNode(group_name.c_str())) {
      draw_all_scenes(group_name, false);
      draw_all_scenes(group_name, true);
      ImGui::TreePop();
    }
  }
}

bool SubtitleEditor::any_hints_in_group(const std::string& group_name) {
  auto& scenes = m_subtitle_db.m_banks.at(m_current_language)->m_scenes;
  auto scenes_in_group = m_subtitle_db.m_subtitle_groups->m_groups[group_name];
  for (auto& scene_name : scenes_in_group) {
    if (scenes.count(scene_name) != 0) {
      auto& scene_info = scenes.at(scene_name);
      if (scene_info.m_kind != SubtitleSceneKind::Movie) {
        return true;
      }
    }
  }
  return false;
}

void SubtitleEditor::draw_all_hint_groups() {
  for (auto& group_name : m_subtitle_db.m_subtitle_groups->m_group_order) {
    if (!m_filter_hints.empty() && m_filter_hints != m_filter_placeholder) {
      ImGui::SetNextItemOpen(true);
    }
    if (m_subtitle_db.m_subtitle_groups->m_groups.count(group_name) == 0 ||
        !any_hints_in_group(group_name)) {
      continue;
    }
    if (ImGui::TreeNode(group_name.c_str())) {
      draw_all_hints(group_name, false);
      draw_all_hints(group_name, true);
      ImGui::TreePop();
    }
  }
}

void SubtitleEditor::draw_all_scenes(std::string group_name, bool base_cutscenes) {
  auto& scenes =
      m_subtitle_db.m_banks.at(base_cutscenes ? m_base_language : m_current_language)->m_scenes;
  auto scenes_in_group = m_subtitle_db.m_subtitle_groups->m_groups[group_name];
  for (auto& scene_name : scenes_in_group) {
    if (scenes.count(scene_name) == 0) {
      continue;
    }
    auto& scene_info = scenes.at(scene_name);
    // Don't duplicate entries
    if (base_cutscenes && is_scene_in_current_lang(scene_name)) {
      continue;
    }
    bool is_current_scene = m_current_scene && m_current_scene->m_name == scene_info.m_name;
    if (scene_info.m_kind != SubtitleSceneKind::Movie) {
      continue;
    }
    if ((!m_filter.empty() && m_filter != m_filter_placeholder) &&
        scene_name.find(m_filter) == std::string::npos) {
      continue;
    }
    if (!base_cutscenes && is_current_scene) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
    }
    if (base_cutscenes) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else if (m_db.count(scene_name) == 0) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_warning_color);
    }

    if (ImGui::TreeNode(
            fmt::format("{}-{}", scene_name, base_cutscenes ? m_base_language : m_current_language)
                .c_str(),
            "%s", scene_name.c_str())) {
      if (base_cutscenes || is_current_scene || m_db.count(scene_name) == 0) {
        ImGui::PopStyleColor();
      }
      if (!base_cutscenes && !is_current_scene) {
        if (ImGui::Button("Select as Current")) {
          m_current_scene = &scene_info;
        }
      }
      if (base_cutscenes) {
        if (ImGui::Button("Copy from Base Language")) {
          m_subtitle_db.m_banks.at(m_current_language)->add_scene(scene_info);
        }
      }
      draw_subtitle_options(scene_info);
      ImGui::TreePop();
    } else if (base_cutscenes || is_current_scene || m_db.count(scene_name) == 0) {
      ImGui::PopStyleColor();
    }
  }
}

void SubtitleEditor::draw_all_hints(std::string group_name, bool base_cutscenes) {
  auto& scenes =
      m_subtitle_db.m_banks.at(base_cutscenes ? m_base_language : m_current_language)->m_scenes;
  auto scenes_in_group = m_subtitle_db.m_subtitle_groups->m_groups[group_name];
  for (auto& scene_name : scenes_in_group) {
    if (scenes.count(scene_name) == 0) {
      continue;
    }
    auto& scene_info = scenes.at(scene_name);
    // Don't duplicate entries
    if (base_cutscenes && is_scene_in_current_lang(scene_name)) {
      continue;
    }
    if (scene_info.m_kind != SubtitleSceneKind::Hint &&
        scene_info.m_kind != SubtitleSceneKind::HintNamed) {
      continue;
    }
    if ((!m_filter_hints.empty() && m_filter_hints != m_filter_placeholder) &&
        scene_name.find(m_filter_hints) == std::string::npos) {
      continue;
    }
    if (base_cutscenes) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    }

    if (ImGui::TreeNode(
            fmt::format("{}-{}", scene_name, base_cutscenes ? m_base_language : m_current_language)
                .c_str(),
            "%s", scene_name.c_str())) {
      if (base_cutscenes) {
        ImGui::PopStyleColor();
      }
      if (base_cutscenes) {
        if (ImGui::Button("Copy from Base Language")) {
          m_subtitle_db.m_banks.at(m_current_language)->add_scene(scene_info);
        }
      }
      draw_subtitle_options(scene_info);
      ImGui::TreePop();
    } else if (base_cutscenes) {
      ImGui::PopStyleColor();
    }
  }
}

void SubtitleEditor::draw_subtitle_options(GameSubtitleSceneInfo& scene, bool current_scene) {
  if (!m_repl.is_connected()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("REPL not connected, can't play!");
    ImGui::PopStyleColor();
  } else {
    // Cutscenes
    if (scene.m_kind == SubtitleSceneKind::Movie && m_db.count(scene.m_name) > 0) {
      if (ImGui::Button("Play Scene")) {
        update_subtitle_editor_db();
        repl_execute_cutscene_code(m_db[scene.m_name]);
      }
      ImGui::SameLine();
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
      ImGui::TextWrapped("You may have to click twice, load times cause issues");
      ImGui::PopStyleColor();
      ImGui::NewLine();
    }
    // Hints
    else if (scene.m_kind == SubtitleSceneKind::Hint ||
             scene.m_kind == SubtitleSceneKind::HintNamed) {
      if (ImGui::Button("Play Hint")) {
        repl_play_hint(scene.m_name);
      }
      // TODO add "Remove Hint" button (if you named it wrong or something)
      ImGui::SameLine();
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
      ImGui::TextWrapped("You may have to click twice, load times cause issues");
      ImGui::PopStyleColor();
      ImGui::NewLine();
    }
  }
  if (ImGui::BeginCombo("Sorting Group", scene.m_sorting_group.c_str())) {
    for (size_t i = 0; i < m_subtitle_db.m_subtitle_groups->m_group_order.size(); ++i) {
      const bool isSelected = (scene.m_sorting_group_idx == (int)i);
      if (ImGui::Selectable(m_subtitle_db.m_subtitle_groups->m_group_order[i].c_str(),
                            isSelected)) {
        // Remove from current group
        m_subtitle_db.m_subtitle_groups->remove_scene(scene.m_sorting_group, scene.m_name);
        // Add to new group
        scene.m_sorting_group_idx = i;
        scene.m_sorting_group = m_subtitle_db.m_subtitle_groups->m_group_order.at(i);
        m_subtitle_db.m_subtitle_groups->add_scene(scene.m_sorting_group, scene.m_name);
      }
      if (isSelected) {
        ImGui::SetItemDefaultFocus();
      }
    }
    ImGui::EndCombo();
  }
  if (current_scene) {
    draw_new_cutscene_line_form();
  }
  auto font =
      get_font_bank(parse_text_only_version(m_subtitle_db.m_banks[m_current_language]->file_path));
  for (size_t i = 0; i < scene.m_lines.size(); i++) {
    auto& subtitleLine = scene.m_lines.at(i);
    auto linetext = font->convert_game_to_utf8(subtitleLine.line.c_str());
    std::string summary;
    if (linetext.empty()) {
      summary = fmt::format("[{}] Clear Screen", subtitleLine.frame);
    } else if (linetext.length() >= 30) {
      summary = fmt::format("[{}] {} - '{}...'", subtitleLine.frame, subtitleLine.speaker,
                            linetext.substr(0, 30));
    } else {
      summary = fmt::format("[{}] {} - '{}'", subtitleLine.frame, subtitleLine.speaker,
                            linetext.substr(0, 30));
    }
    if (linetext.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else if (subtitleLine.offscreen) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_offscreen_text_color);
    }
    if (ImGui::TreeNode(fmt::format("{}", i).c_str(), "%s", summary.c_str())) {
      if (linetext.empty() || subtitleLine.offscreen) {
        ImGui::PopStyleColor();
      }
      ImGui::InputInt("Starting Frame", &subtitleLine.frame,
                      ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
      ImGui::InputText("Speaker", &subtitleLine.speaker);
      ImGui::InputText("Text", &linetext);
      ImGui::Checkbox("Offscreen?", &subtitleLine.offscreen);
      // TODO - deleting while iterating is a bad pattern, especially with imgui's declarative
      // style
      // disabling this for now, it's not working in it's current state.
      // if (scene.m_lines.size() > 1) {  // prevent creating an empty scene
      //  ImGui::PushStyleColor(ImGuiCol_Button, m_warning_color);
      //  if (ImGui::Button("Remove Line")) {
      //    scene.m_lines.erase(scene.m_lines.begin() + i);
      //  }
      //  ImGui::PopStyleColor();
      //}
      ImGui::TreePop();
    } else if (linetext.empty() || subtitleLine.offscreen) {
      ImGui::PopStyleColor();
    }
    auto newtext = font->convert_utf8_to_game_with_escape(linetext);
    subtitleLine.line = newtext;
  }
}

void SubtitleEditor::draw_new_cutscene_line_form() {
  ImGui::InputInt("Frame Number", &m_current_scene_frame,
                  ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
  ImGui::InputText("Speaker", &m_current_scene_speaker);
  ImGui::InputText("Text", &m_current_scene_text);
  ImGui::Checkbox("Offscreen", &m_current_scene_offscreen);
  bool rendered_text_entry_btn = false;
  if (m_current_scene_frame < 0 || m_current_scene_text.empty() ||
      m_current_scene_speaker.empty()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("Can't add a new text entry with the current fields!");
    ImGui::PopStyleColor();
  } else {
    rendered_text_entry_btn = true;
    if (ImGui::Button("Add Text Entry")) {
      m_current_scene->add_line(m_current_scene_frame, m_current_scene_text,
                                m_current_scene_speaker, m_current_scene_offscreen);
    }
  }
  if (m_current_scene_frame < 0) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("Can't add a clear screen entry with the current fields!");
    ImGui::PopStyleColor();
  } else {
    if (rendered_text_entry_btn) {
      ImGui::SameLine();
    }
    if (ImGui::Button("Add Clear Screen Entry")) {
      m_current_scene->add_line(m_current_scene_frame, "", "", false);
    }
  }
  ImGui::NewLine();
}
