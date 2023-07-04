#include "subtitle_editor.h"

#include <regex>
#include <string_view>

#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"
#include <common/serialization/text/text_ser.h>

#include "game/runtime.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

SubtitleEditor::SubtitleEditor() : m_repl(8181) {
  m_filter_cutscenes = m_filter_placeholder;
  m_filter_non_cutscenes = m_filter_placeholder;
}

bool SubtitleEditor::is_scene_in_current_lang(const std::string& scene_name) {
  return m_subtitle_db.m_banks.at(m_current_language)->m_scenes.count(scene_name) > 0;
}

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");
  // Lazily load the first time the window is displayed, NOT when the game is launched
  // why? because it takes like 1-2 seconds!
  if (!db_loaded) {
    auto subtitle_version = GameSubtitleDB::SubtitleFormat::V2;
    if (g_game_version == GameVersion::Jak1) {
      subtitle_version = GameSubtitleDB::SubtitleFormat::V1;
      m_jak1_editor_db.update();
    }
    m_subtitle_db = load_subtitle_project(subtitle_version, g_game_version);
    db_loaded = true;
  }

  if (ImGui::Button("Save Changes")) {
    m_files_saved_successfully =
        std::make_optional(m_subtitle_db.write_subtitle_db_to_files(g_game_version));
    m_repl.rebuild_text();
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

  if (ImGui::TreeNode("Cutscenes")) {
    draw_scene_section_header(false);
    ImGui::InputText("Filter", &m_filter_cutscenes,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    draw_all_cutscenes(false);
    draw_all_cutscenes(true);
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Non-Cutscenes")) {
    draw_scene_section_header(true);
    ImGui::InputText("Filter", &m_filter_non_cutscenes,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    draw_all_non_cutscenes(false);
    draw_all_non_cutscenes(true);
    ImGui::TreePop();
  }
  ImGui::End();
}

void SubtitleEditor::draw_edit_options() {
  if (ImGui::TreeNode("Editing Options")) {
    if (ImGui::BeginCombo(
            "Editing Language ID",
            fmt::format("[{}] {}", m_subtitle_db.m_banks[m_current_language]->m_lang_id,
                        m_subtitle_db.m_banks[m_current_language]->m_file_path)
                .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_current_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->m_lang_id, value->m_file_path).c_str(),
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
                                      m_subtitle_db.m_banks[m_base_language]->m_file_path)
                              .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_base_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->m_lang_id, value->m_file_path).c_str(),
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
    if (g_game_version == GameVersion::Jak1) {
      if (ImGui::Button("Update Editor DB")) {
        m_jak1_editor_db.update();
      }
    }
    ImGui::TreePop();
  }
}

void SubtitleEditor::draw_repl_options() {
  if (ImGui::TreeNode("REPL Options")) {
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

void SubtitleEditor::draw_scene_section_header(const bool non_cutscenes) {
  if (ImGui::TreeNode("Create New Scene Entry")) {
    ImGui::InputText("New Scene Name", &m_new_scene_name);
    if (non_cutscenes && g_game_version == GameVersion::Jak1) {
      ImGui::InputText("New Scene ID (hex)", &m_new_scene_id);
    }
    if (is_scene_in_current_lang(m_new_scene_name)) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Scene already exists with that name, no!");
      ImGui::PopStyleColor();
    }
    if (!is_scene_in_current_lang(m_new_scene_name) && !m_new_scene_name.empty()) {
      if (ImGui::Button("Add Scene")) {
        GameSubtitleSceneInfo new_scene;
        new_scene.is_cutscene = !non_cutscenes;
        if (non_cutscenes && g_game_version == GameVersion::Jak1) {
          new_scene.m_hint_id = strtoul(m_new_scene_id.c_str(), nullptr, 16);
        } else {
          new_scene.m_hint_id = 0;
        }
        m_subtitle_db.m_banks.at(m_current_language)->m_scenes.emplace(m_new_scene_name, new_scene);
        m_new_scene_name = "";
      }
      ImGui::NewLine();
    }
  }
}

void SubtitleEditor::draw_all_cutscenes(bool base_cutscenes) {
  for (auto& [scene_name, scene_info] :
       m_subtitle_db.m_banks.at(base_cutscenes ? m_base_language : m_current_language)->m_scenes) {
    if (!scene_info.is_cutscene || base_cutscenes && is_scene_in_current_lang(scene_name)) {
      continue;
    }
    if ((!m_filter_cutscenes.empty() && m_filter_cutscenes != m_filter_placeholder) &&
        str_util::to_lower(scene_name).find(str_util::to_lower(m_filter_cutscenes)) ==
            std::string::npos) {
      continue;
    }
    bool is_current_scene = m_current_scene && m_current_scene->m_name == scene_name;
    bool pop_color = false;
    if (!base_cutscenes && is_current_scene) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
      pop_color = true;
    } else if (base_cutscenes) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
      pop_color = true;
    } else if (g_game_version == GameVersion::Jak1 &&
               m_jak1_editor_db.m_db.find(scene_name) == m_jak1_editor_db.m_db.end()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_warning_color);
      pop_color = true;
    }

    if (ImGui::TreeNode(
            fmt::format("{}-{}", scene_name, base_cutscenes ? m_base_language : m_current_language)
                .c_str(),
            "%s", scene_name.c_str())) {
      if (pop_color) {
        ImGui::PopStyleColor();
      }
      if (!base_cutscenes && !is_current_scene) {
        if (ImGui::Button("Select as Current Cutscene")) {
          m_current_scene = std::make_shared<GameSubtitleSceneInfo>(scene_info);
        }
      }
      if (base_cutscenes) {
        if (ImGui::Button("Copy from Base Language")) {
          m_subtitle_db.m_banks.at(m_current_language)->m_scenes.emplace(scene_name, scene_info);
        }
      }
      draw_subtitle_options(scene_info);
      ImGui::TreePop();
    } else if (pop_color) {
      ImGui::PopStyleColor();
    }
  }
}

// TODO - amalgamate with the above
void SubtitleEditor::draw_all_non_cutscenes(bool base_cutscenes) {
  for (auto& [scene_name, scene_info] :
       m_subtitle_db.m_banks.at(base_cutscenes ? m_base_language : m_current_language)->m_scenes) {
    if (scene_info.is_cutscene || base_cutscenes && is_scene_in_current_lang(scene_name)) {
      continue;
    }
    if ((!m_filter_non_cutscenes.empty() && m_filter_non_cutscenes != m_filter_placeholder) &&
        str_util::to_lower(scene_name).find(str_util::to_lower(m_filter_non_cutscenes)) ==
            std::string::npos) {
      continue;
    }
    bool pop_color = false;
    if (base_cutscenes) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
      pop_color = true;
    }

    if (ImGui::TreeNode(
            fmt::format("{}-{}", scene_name, base_cutscenes ? m_base_language : m_current_language)
                .c_str(),
            "%s", scene_name.c_str())) {
      if (pop_color) {
        ImGui::PopStyleColor();
      }
      if (base_cutscenes) {
        if (ImGui::Button("Copy from Base Language")) {
          m_subtitle_db.m_banks.at(m_current_language)->m_scenes.emplace(scene_name, scene_info);
        }
      }
      draw_subtitle_options(scene_info);
      ImGui::TreePop();
    } else if (pop_color) {
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
    if (ImGui::Button("Play")) {
      if (g_game_version == GameVersion::Jak1) {
        m_jak1_editor_db.update();
        if (scene.is_cutscene) {
          m_repl.execute_jak1_cutscene_code(m_jak1_editor_db.m_db.at(scene.m_name));
        } else {
          m_repl.play_hint(scene.m_name);
        }
      } else {
        // TODO
      }
    }
    ImGui::SameLine();
    ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    ImGui::TextWrapped("You may have to click twice, load times cause issues");
    ImGui::PopStyleColor();
    ImGui::NewLine();
  }
  if (current_scene) {
    draw_new_cutscene_line_form();
  }
  auto font = get_font_bank(m_subtitle_db.m_banks[m_current_language]->m_text_version);
  int i = 0;
  for (auto subtitle_line = scene.m_lines.begin(); subtitle_line != scene.m_lines.end();) {
    auto& line_text = subtitle_line->text;
    auto& line_speaker = subtitle_line->metadata.speaker;
    std::string summary;
    if (line_text.empty()) {
      summary = fmt::format("[{}] Clear Screen", subtitle_line->metadata.frame_start);
    } else if (line_text.length() >= 30) {
      summary = fmt::format("[{}] {} - '{}...'", subtitle_line->metadata.frame_start, line_speaker,
                            line_text.substr(0, 30));
    } else {
      summary = fmt::format("[{}] {} - '{}'", subtitle_line->metadata.frame_start, line_speaker,
                            line_text.substr(0, 30));
    }
    if (line_text.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else if (subtitle_line->metadata.offscreen) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_offscreen_text_color);
    }
    if (ImGui::TreeNode(fmt::format("{}", i).c_str(), "%s", summary.c_str())) {
      if (line_text.empty() || subtitle_line->metadata.offscreen) {
        ImGui::PopStyleColor();
      }
      ImGui::InputInt("Starting Frame", &subtitle_line->metadata.frame_start,
                      ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
      // TODO - speaker dropdown instead
      ImGui::InputText("Speaker", &line_speaker);
      ImGui::InputText("Text", &line_text);
      ImGui::Checkbox("Offscreen?", &subtitle_line->metadata.offscreen);
      if (scene.m_lines.size() > 1) {  // prevent creating an empty scene
        ImGui::PushStyleColor(ImGuiCol_Button, m_warning_color);
        if (ImGui::Button("Remove")) {
          subtitle_line = scene.m_lines.erase(subtitle_line);
          ImGui::PopStyleColor();
          ImGui::TreePop();
          continue;
        }
        ImGui::PopStyleColor();
      }
      ImGui::TreePop();
    } else if (line_text.empty() || subtitle_line->metadata.offscreen) {
      ImGui::PopStyleColor();
    }
    subtitle_line->text = font->convert_utf8_to_game(line_text, true);
    subtitle_line->metadata.speaker = font->convert_utf8_to_game(line_speaker, true);
    i++;
    subtitle_line++;
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
      auto font = get_font_bank(
          parse_text_only_version(m_subtitle_db.m_banks[m_current_language]->m_file_path));
      m_current_scene->add_line(m_current_scene_text, m_current_scene_frame, 0,
                                m_current_scene_offscreen, m_current_scene_speaker, false);
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
      m_current_scene->add_line("", m_current_scene_frame, 0, m_current_scene_offscreen, "", false);
    }
  }
  ImGui::NewLine();
}
