#include "subtitle_editor.h"

#include <regex>
#include <string_view>

#include "common/serialization/text/text_ser.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "game/runtime.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

SubtitleEditor::SubtitleEditor() {
  m_filter_cutscenes = m_filter_placeholder;
  m_filter_non_cutscenes = m_filter_placeholder;
  if (g_game_version == GameVersion::Jak1) {
    m_subtitle_version = GameSubtitleDB::SubtitleFormat::V1;
  } else {
    m_subtitle_version = GameSubtitleDB::SubtitleFormat::V2;
  }
}

bool SubtitleEditor::is_v1_format() {
  return m_subtitle_db.m_subtitle_version == GameSubtitleDB::SubtitleFormat::V1;
}

bool SubtitleEditor::is_scene_in_current_lang(const std::string& scene_name) {
  return m_subtitle_db.m_banks.at(m_current_language)->m_scenes.find(scene_name) !=
         m_subtitle_db.m_banks.at(m_current_language)->m_scenes.end();
}

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");
  // Lazily load the first time the window is displayed
  if (!m_db_loaded && !m_db_failed_to_load) {
    if (g_game_version == GameVersion::Jak1) {
      m_jak1_editor_db.update();
    }
    m_subtitle_db = load_subtitle_project(m_subtitle_version, g_game_version);
    if (m_subtitle_db.m_load_error) {
      m_db_failed_to_load = true;
    } else {
      m_db_loaded = true;
    }
  } else if (m_db_failed_to_load) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("%s",
                fmt::format("Error Loading - {}!", m_subtitle_db.m_load_error.value()).c_str());
    ImGui::PopStyleColor();
    if (ImGui::Button("Try Again")) {
      if (g_game_version == GameVersion::Jak1) {
        m_jak1_editor_db.update();
      }
      m_subtitle_db = load_subtitle_project(m_subtitle_version, g_game_version);
      if (m_subtitle_db.m_load_error) {
        m_db_failed_to_load = true;
      } else {
        m_db_loaded = true;
      }
    }
  }

  if (ImGui::Button("Save Changes")) {
    m_files_saved_successfully =
        std::make_optional(m_subtitle_db.write_subtitle_db_to_files(g_game_version));
    m_repl.rebuild_text();
    // TODO - reloading the project would be a good idea because then cutscens that have since been
    // modified would appear as such but that creates race conditions when the GUI is parsing at the
    // same time it seems so, disabled for now Same Below

    // m_subtitle_db = load_subtitle_project(m_subtitle_version, g_game_version);
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
  /*if (ImGui::Button("Reload Project")) {
    m_subtitle_db = load_subtitle_project(m_subtitle_version, g_game_version);
  }*/

  draw_edit_options();
  draw_repl_options();
  draw_speaker_options();

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
    if (m_subtitle_db.m_banks[m_current_language]->m_file_base_path) {
      draw_all_cutscenes(true);
    }
    draw_all_cutscenes(false);
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("Non-Cutscenes")) {
    draw_scene_section_header(true);
    ImGui::InputText("Filter", &m_filter_non_cutscenes,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    if (m_subtitle_db.m_banks[m_current_language]->m_file_base_path) {
      draw_all_non_cutscenes(true);
    }
    draw_all_non_cutscenes(false);
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
    if (m_subtitle_db.m_banks.find(m_current_language) != m_subtitle_db.m_banks.end()) {
      if (m_subtitle_db.m_banks.at(m_current_language)->m_file_base_path) {
        ImGui::Text("Language Base - %s",
                    m_subtitle_db.m_banks.at(m_current_language)->m_file_base_path.value().c_str());
      } else {
        ImGui::Text("This language has no base language!");
      }
    }
    ImGui::Checkbox("Truncate line summaries", &m_truncate_summaries);
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
    ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
    ImGui::Text(" task repl");
    ImGui::Text(" (lt)");
    ImGui::Text(" (mi)");
    ImGui::PopStyleColor();
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

void SubtitleEditor::draw_speaker_options() {
  if (ImGui::TreeNode("Speakers")) {
    const auto& bank = m_subtitle_db.m_banks[m_current_language];
    for (auto& [speaker_id, speaker_localized] : bank->m_speakers) {
      if (speaker_id == "none") {
        continue;
      }
      // Insertion or deletion not needed here as it has to be wired up in .gc and C++ code
      // nothing would get persisted and there has to be a translation for all speakers (even if
      // it's no translation at all)
      ImGui::InputText(speaker_id.c_str(), &speaker_localized);
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
    } else if (!m_new_scene_name.empty()) {
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
    ImGui::TreePop();
  }
}

void SubtitleEditor::draw_scene_node(const bool base_cutscenes,
                                     const std::string& scene_name,
                                     GameSubtitleSceneInfo& scene_info,
                                     std::unordered_set<std::string>& scenes_to_delete) {
  bool is_current_scene = m_current_scene && m_current_scene->m_name == scene_name;
  bool pop_color = false;
  if (!base_cutscenes && is_current_scene) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
    pop_color = true;
  } else if (base_cutscenes) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    pop_color = true;
  } else if (g_game_version == GameVersion::Jak1 && scene_info.is_cutscene &&
             m_jak1_editor_db.m_db.find(scene_name) == m_jak1_editor_db.m_db.end()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_warning_color);
    pop_color = true;
  }

  if (ImGui::TreeNode(fmt::format("{}-{}", scene_name, base_cutscenes).c_str(), "%s",
                      scene_name.c_str())) {
    if (pop_color) {
      ImGui::PopStyleColor();
    }
    if (!is_current_scene) {
      if (ImGui::Button("Select as Current Cutscene")) {
        m_current_scene = &scene_info;
      }
    }
    draw_subtitle_options(scene_info);
    ImGui::PushStyleColor(ImGuiCol_Button, m_warning_color);
    if (ImGui::Button("Delete")) {
      if (scene_info.m_name == m_current_scene->m_name) {
        m_current_scene = nullptr;
      }
      scenes_to_delete.insert(scene_name);
    }
    ImGui::PopStyleColor();
    ImGui::TreePop();
  } else if (pop_color) {
    ImGui::PopStyleColor();
  }
}

void SubtitleEditor::draw_all_cutscenes(bool base_cutscenes) {
  std::unordered_set<std::string> scenes_to_delete;
  for (auto& [scene_name, scene_info] : m_subtitle_db.m_banks.at(m_current_language)->m_scenes) {
    if (!scene_info.is_cutscene || (base_cutscenes && !scene_info.only_defined_in_base) ||
        (!base_cutscenes &&
         m_subtitle_db.m_banks[m_current_language]->m_file_base_path.has_value() &&
         scene_info.only_defined_in_base)) {
      continue;
    }
    if ((!m_filter_cutscenes.empty() && m_filter_cutscenes != m_filter_placeholder) &&
        str_util::to_lower(scene_name).find(str_util::to_lower(m_filter_cutscenes)) ==
            std::string::npos) {
      continue;
    }
    draw_scene_node(base_cutscenes, scene_name, scene_info, scenes_to_delete);
  }
  for (auto& scene_name : scenes_to_delete) {
    if (m_subtitle_db.m_banks.at(m_current_language)->scene_exists(scene_name)) {
      m_subtitle_db.m_banks.at(m_current_language)->m_scenes.erase(scene_name);
    }
  }
}

void SubtitleEditor::draw_all_non_cutscenes(bool base_cutscenes) {
  std::unordered_set<std::string> scenes_to_delete;
  for (auto& [scene_name, scene_info] : m_subtitle_db.m_banks.at(m_current_language)->m_scenes) {
    if (scene_info.is_cutscene || (base_cutscenes && !scene_info.only_defined_in_base) ||
        (!base_cutscenes &&
         m_subtitle_db.m_banks[m_current_language]->m_file_base_path.has_value() &&
         scene_info.only_defined_in_base)) {
      continue;
    }
    if ((!m_filter_non_cutscenes.empty() && m_filter_non_cutscenes != m_filter_placeholder) &&
        str_util::to_lower(scene_name).find(str_util::to_lower(m_filter_non_cutscenes)) ==
            std::string::npos) {
      continue;
    }
    draw_scene_node(base_cutscenes, scene_name, scene_info, scenes_to_delete);
  }
  for (auto& scene_name : scenes_to_delete) {
    if (m_subtitle_db.m_banks.at(m_current_language)->scene_exists(scene_name)) {
      m_subtitle_db.m_banks.at(m_current_language)->m_scenes.erase(scene_name);
    }
  }
}

std::string SubtitleEditor::subtitle_line_summary(
    const SubtitleLine& line,
    const SubtitleLineMetadata& line_meta,
    const std::shared_ptr<GameSubtitleBank> /*bank*/) {
  // Truncate the text if it's too long, it's supposed to just be a summary at a glance
  std::string line_text = "";
  if (!line.text.empty()) {
    if (m_truncate_summaries && line.text.size() > 30) {
      line_text = line.text.substr(0, 30) + "...";
    } else {
      line_text = line.text;
    }
  } else {
    if (is_v1_format()) {
      line_text = "Clear Screen";
    } else if (line_meta.merge) {
      line_text = "<Merge>";
    }
  }
  // Append important info about the frame / speaker to the front
  std::string info_header = fmt::format("[{}", line_meta.frame_start);
  // V1
  if (is_v1_format()) {
    if (line.text.empty()) {
      return fmt::format("{}] {}", info_header, line_text);
    }
    return fmt::format("{}] {} - '{}'", info_header,
                       m_subtitle_db.m_banks[m_current_language]->m_speakers.at(line_meta.speaker),
                       line_text);
  }
  // V2
  if (line_meta.merge) {
    return fmt::format("{}-{}] {} - {}", info_header, line_meta.frame_end,
                       m_subtitle_db.m_banks[m_current_language]->m_speakers.at(line_meta.speaker),
                       line_text);
  }
  return fmt::format("{}-{}] {} - '{}'", info_header, line_meta.frame_end,
                     m_subtitle_db.m_banks[m_current_language]->m_speakers.at(line_meta.speaker),
                     line_text);
}

void SubtitleEditor::draw_subtitle_options(GameSubtitleSceneInfo& scene, bool current_scene) {
  if (!m_repl.is_connected()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("REPL not connected, can't play!");
    ImGui::PopStyleColor();
  } else {
    bool play = false;
    bool save_and_reload_text = false;
    if (ImGui::Button("Play")) {
      play = true;
    }
    ImGui::SameLine();
    if (ImGui::Button("Save and Play")) {
      play = true;
      save_and_reload_text = true;
    }
    if (play) {
      if (save_and_reload_text) {
        m_subtitle_db.write_subtitle_db_to_files(g_game_version);
        m_repl.rebuild_text();
      }
      if (g_game_version == GameVersion::Jak1) {
        m_jak1_editor_db.update();
        if (scene.is_cutscene) {
          if (m_jak1_editor_db.m_db.find(scene.m_name) == m_jak1_editor_db.m_db.end()) {
            lg::error("{} not defined in Jak 1's subtitle editor database!", scene.m_name);
          } else {
            m_repl.execute_jak1_cutscene_code(m_jak1_editor_db.m_db.at(scene.m_name));
          }
        } else {
          m_repl.play_hint(scene.m_name);
        }
      } else {
        m_repl.play_vag(scene.m_name, scene.is_cutscene);
      }
    }
  }
  if (current_scene) {
    draw_new_scene_line_form();
  }
  int i = 0;
  for (auto subtitle_line = scene.m_lines.begin(); subtitle_line != scene.m_lines.end();) {
    auto& line_text = subtitle_line->text;
    auto& line_speaker = subtitle_line->metadata.speaker;
    auto& line_meta = subtitle_line->metadata;
    int frames[2] = {line_meta.frame_start, line_meta.frame_end};
    std::string summary =
        subtitle_line_summary(*subtitle_line, line_meta, m_subtitle_db.m_banks[m_current_language]);
    if (line_text.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else if (subtitle_line->metadata.offscreen) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_offscreen_text_color);
    }
    if (ImGui::TreeNode(fmt::format("{}", i).c_str(), "%s", summary.c_str())) {
      if (line_text.empty() || subtitle_line->metadata.offscreen) {
        ImGui::PopStyleColor();
      }
      bool hide_line_options = false;
      if (is_v1_format()) {
        if (line_text.empty()) {
          hide_line_options = true;
        } else {
          ImGui::InputInt("Starting Frame", &subtitle_line->metadata.frame_start,
                          ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
        }
      } else {
        ImGui::InputInt2("Start and End Frame", frames,
                         ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
      }
      if (!hide_line_options) {
        if (ImGui::BeginCombo(
                "Speaker",
                m_subtitle_db.m_banks[m_current_language]->m_speakers.at(line_speaker).c_str())) {
          for (const auto& [speaker_id, localized_name] :
               m_subtitle_db.m_banks[m_current_language]->m_speakers) {
            const bool is_selected = speaker_id == line_speaker;
            if (is_selected) {
              ImGui::SetItemDefaultFocus();
            }
            if (ImGui::Selectable(localized_name.c_str(), is_selected)) {
              subtitle_line->metadata.speaker = speaker_id;
            }
          }
          ImGui::EndCombo();
        }
        ImGui::InputText("Text", &subtitle_line->text,
                         line_meta.merge ? ImGuiInputTextFlags_ReadOnly : 0);
        ImGui::Checkbox("Offscreen?", &subtitle_line->metadata.offscreen);
        if (!is_v1_format()) {
          ImGui::SameLine();
          if (ImGui::Checkbox("Merge Text?", &subtitle_line->metadata.merge)) {
            // Clear text if they've checked it
            if (subtitle_line->metadata.merge) {
              subtitle_line->text = "";
            }
          }
        }
      }

      ImGui::PushStyleColor(ImGuiCol_Button, m_warning_color);
      if (ImGui::Button("Remove")) {
        subtitle_line = scene.m_lines.erase(subtitle_line);
        ImGui::PopStyleColor();
        ImGui::TreePop();
        continue;
      }
      ImGui::PopStyleColor();
      ImGui::TreePop();
    } else if (line_text.empty() || subtitle_line->metadata.offscreen) {
      ImGui::PopStyleColor();
    }
    line_meta.frame_start = frames[0];
    line_meta.frame_end = frames[1];
    i++;
    subtitle_line++;
  }
}

void SubtitleEditor::draw_new_scene_line_form() {
  if (is_v1_format()) {
    ImGui::InputInt("Starting Frame", &m_current_scene_frames[0],
                    ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
  } else {
    ImGui::InputInt2("Start and End Frame", m_current_scene_frames,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
  }
  std::string current_speaker = "";
  if (m_subtitle_db.m_banks[m_current_language]->m_speakers.find(m_current_scene_speaker) !=
      m_subtitle_db.m_banks[m_current_language]->m_speakers.end()) {
    current_speaker =
        m_subtitle_db.m_banks[m_current_language]->m_speakers.at(m_current_scene_speaker);
  }
  if (ImGui::BeginCombo("Speaker", current_speaker.c_str())) {
    for (const auto& [speaker_id, localized_name] :
         m_subtitle_db.m_banks[m_current_language]->m_speakers) {
      const bool is_selected = speaker_id == m_current_scene_speaker;
      if (is_selected) {
        ImGui::SetItemDefaultFocus();
      }
      if (ImGui::Selectable(localized_name.c_str(), is_selected)) {
        m_current_scene_speaker = speaker_id;
      }
    }
    ImGui::EndCombo();
  }
  ImGui::InputText("Text", &m_current_scene_text,
                   m_current_scene_merge ? ImGuiInputTextFlags_ReadOnly : 0);
  ImGui::Checkbox("Offscreen", &m_current_scene_offscreen);
  if (!is_v1_format()) {
    ImGui::SameLine();
    if (ImGui::Checkbox("Merge Text?", &m_current_scene_merge)) {
      // Clear text if they've checked it
      if (m_current_scene_merge) {
        m_current_scene_text = "";
      }
    }
  }
  bool rendered_text_entry_btn = false;
  // Validation:
  // - start frame > 0
  // - end frame > start_frame
  // - non-empty text
  // - pick a speaker
  if (m_current_scene_frames[0] < 0 ||
      (!is_v1_format() && m_current_scene_frames[1] < m_current_scene_frames[0]) ||
      (m_current_scene_text.empty() && !m_current_scene_merge) || m_current_scene_speaker.empty()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("Can't add a new text entry with the current fields!");
    ImGui::PopStyleColor();
  } else {
    rendered_text_entry_btn = true;
    if (ImGui::Button("Add Text Entry")) {
      m_current_scene->add_line(m_current_scene_text, m_current_scene_frames[0],
                                m_current_scene_frames[1], m_current_scene_offscreen,
                                m_current_scene_speaker, false);
    }
  }
  if (is_v1_format()) {
    if (m_current_scene_frames[0] < 0) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Can't add a clear screen entry with the current fields!");
      ImGui::PopStyleColor();
    } else {
      if (rendered_text_entry_btn) {
        ImGui::SameLine();
      }
      if (ImGui::Button("Add Clear Screen Entry")) {
        m_current_scene->add_line("", m_current_scene_frames[0], 0, m_current_scene_offscreen, "",
                                  false);
      }
    }
  }
}
