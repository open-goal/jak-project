#include "subtitle2_editor.h"

#include "common/serialization/subtitles2/subtitles2_deser.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

Subtitle2Editor::Subtitle2Editor(GameVersion version)
    : m_repl(8182), m_speaker_names(get_speaker_names(version)) {
  m_filter = m_filter_placeholder;
  m_filter_hints = m_filter_placeholder;
}

bool Subtitle2Editor::is_scene_in_current_lang(const std::string& scene_name) {
  return m_subtitle_db.m_banks.at(m_current_language)->scenes.count(scene_name) > 0;
}

const std::string Subtitle2Editor::speaker_name_by_index(int index) {
  return m_speaker_names.at(index);
}
int Subtitle2Editor::speaker_index_by_name(const std::string& name) {
  for (int i = 0; i < m_speaker_names.size(); ++i) {
    if (m_speaker_names.at(i) == name) {
      return i + 1;
    }
  }
  return 0;
}

void Subtitle2Editor::repl_rebuild_text() {
  m_repl.eval("(make-text)");
  // increment the language id of the in-memory text file so that it won't match the current
  // language and the game will want to reload it asap
  m_repl.eval("(1+! (-> *subtitle2-text* lang))");
}

void Subtitle2Editor::draw_window() {
  ImGui::Begin("Subtitle2 Editor");

  if (!db_loaded) {
    if (ImGui::Button("Load Subtitles")) {
      m_subtitle_db = load_subtitle2_project(g_game_version);
      db_loaded = true;
    }
    ImGui::End();
    return;
  }

  if (ImGui::Button("Save Changes")) {
    m_files_saved_successfully =
        std::make_optional(write_subtitle_db_to_files(m_subtitle_db, g_game_version));
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

  if (ImGui::TreeNode("All Cutscenes")) {
    ImGui::InputText("New Scene Name", &m_new_scene_name);
    ImGui::InputText("Filter", &m_filter, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    if (is_scene_in_current_lang(m_new_scene_name)) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
      ImGui::Text("Scene already exists with that name, no!");
      ImGui::PopStyleColor();
    }
    if (!is_scene_in_current_lang(m_new_scene_name) && !m_new_scene_name.empty()) {
      if (ImGui::Button("Add Scene")) {
        Subtitle2Scene newScene;
        newScene.name = m_new_scene_name;
        m_subtitle_db.m_banks.at(m_current_language)->add_scene(newScene);
        if (m_add_new_scene_as_current) {
          auto& scenes = m_subtitle_db.m_banks.at(m_current_language)->scenes;
          auto& scene_info = scenes.at(m_new_scene_name);
          m_current_scene = &scene_info;
        }
        m_new_scene_name = "";
      }
      ImGui::SameLine();
      ImGui::Checkbox("Add as Current Scene", &m_add_new_scene_as_current);
      ImGui::NewLine();
    }

    draw_all_scenes();
    ImGui::TreePop();
  }

  ImGui::End();
}

void Subtitle2Editor::draw_edit_options() {
  if (ImGui::TreeNode("Editing Options")) {
    if (ImGui::BeginCombo("Editor Language ID",
                          fmt::format("[{}] {}", m_subtitle_db.m_banks[m_current_language]->lang,
                                      m_subtitle_db.m_banks[m_current_language]->file_path)
                              .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_current_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->lang, value->file_path).c_str(),
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
                          fmt::format("[{}] {}", m_subtitle_db.m_banks[m_base_language]->lang,
                                      m_subtitle_db.m_banks[m_base_language]->file_path)
                              .c_str())) {
      for (const auto& [key, value] : m_subtitle_db.m_banks) {
        const bool isSelected = m_base_language == key;
        if (ImGui::Selectable(fmt::format("[{}] {}", value->lang, value->file_path).c_str(),
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
    ImGui::TreePop();
  }
}

void Subtitle2Editor::draw_repl_options() {
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

void Subtitle2Editor::draw_speaker_options() {
  if (ImGui::TreeNode("Speakers")) {
    const auto bank = m_subtitle_db.m_banks[m_current_language];
    for (int i = 0; i < m_speaker_names.size(); ++i) {
      auto speaker_name = speaker_name_by_index(i);
      // ImGui::Text(speaker_name.c_str());
      // ImGui::SameLine();
      if (bank->speakers.count(speaker_name) == 0) {
        // no speaker yet.
        std::string input = "";
        ImGui::InputText(speaker_name.c_str(), &input);
        if (!input.empty()) {
          // speaker got filled
          bank->speakers.insert({speaker_name, input});
        }
      } else {
        // existing speaker
        std::string input = bank->speakers.at(speaker_name);
        ImGui::InputText(speaker_name.c_str(), &input);
        if (input.empty()) {
          // speaker got deleted
          bank->speakers.erase(speaker_name);
        } else {
          // speaker got changed
          bank->speakers.at(speaker_name) = input;
        }
      }
    }
    ImGui::TreePop();
  }
}

void Subtitle2Editor::draw_all_scenes(bool base_cutscenes) {
  auto& scenes =
      m_subtitle_db.m_banks.at(base_cutscenes ? m_base_language : m_current_language)->scenes;
  for (auto& [name, scene] : scenes) {
    // Don't duplicate entries
    if (base_cutscenes && is_scene_in_current_lang(name)) {
      continue;
    }
    bool is_current_scene = m_current_scene && m_current_scene->name == scene.name;
    if ((!m_filter.empty() && m_filter != m_filter_placeholder) &&
        name.find(m_filter) == std::string::npos) {
      continue;
    }
    if (!base_cutscenes && is_current_scene) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_selected_text_color);
    } else if (base_cutscenes) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else {
      ImGui::PushStyleColor(ImGuiCol_Text, m_warning_color);
    }

    if (ImGui::TreeNode(
            fmt::format("{}-{}", name, base_cutscenes ? m_base_language : m_current_language)
                .c_str(),
            "%s", name.c_str())) {
      ImGui::PopStyleColor();
      if (!base_cutscenes && !is_current_scene) {
        if (ImGui::Button("Select as Current")) {
          m_current_scene = &scene;
        }
      }
      if (base_cutscenes) {
        if (ImGui::Button("Copy from Base Language")) {
          m_subtitle_db.m_banks.at(m_current_language)->add_scene(scene);
        }
      }
      draw_subtitle_options(scene);
      ImGui::TreePop();
    } else {
      ImGui::PopStyleColor();
    }
  }
}

void Subtitle2Editor::draw_subtitle_options(Subtitle2Scene& scene, bool current_scene) {
  if (!m_repl.is_connected()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("REPL not connected, can't play!");
    ImGui::PopStyleColor();
  } else {
    // Cutscenes
    if (ImGui::Button("Play Scene")) {
      // repl_execute_cutscene_code(m_db.at(scene.name));
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
  const auto bank = m_subtitle_db.m_banks[m_current_language];
  auto font = get_font_bank(m_subtitle_db.m_banks[m_current_language]->text_version);
  int i = 0;
  for (auto line = scene.lines.begin(); line != scene.lines.end();) {
    float times[2] = {line->start, line->end};
    auto linetext = font->convert_game_to_utf8(line->text.c_str());
    int speaker = line->speaker - 1;
    auto speaker_text =
        speaker == -1
            ? "N/A"
            : font->convert_game_to_utf8(bank->speakers.at(speaker_name_by_index(speaker)).c_str());
    std::string full_line = linetext;
    if (speaker != -1) {
      full_line = speaker_text + ": " + full_line;
    }
    auto summary =
        fmt::format("[{} - {}] {}", line->start, line->end,
                    full_line.length() <= 30 ? full_line : (full_line.substr(0, 27) + "..."));
    if (linetext.empty()) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_disabled_text_color);
    } else if (line->offscreen) {
      ImGui::PushStyleColor(ImGuiCol_Text, m_offscreen_text_color);
    }
    if (ImGui::TreeNode(fmt::format("{}", i).c_str(), "%s", summary.c_str())) {
      if (linetext.empty() || line->offscreen) {
        ImGui::PopStyleColor();
      }
      ImGui::InputFloat2("Start and End Frame", times, "%.0f",
                         ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
      if (ImGui::BeginCombo("Speaker", speaker_text.c_str())) {
        for (int i = 0; i < m_speaker_names.size(); ++i) {
          auto speaker_name = speaker_name_by_index(i);
          if (bank->speakers.count(speaker_name) == 0) {
            continue;
          }
          const bool isSelected = speaker == i;
          if (ImGui::Selectable(
                  fmt::format("{} ({})",
                              font->convert_game_to_utf8(bank->speakers.at(speaker_name).c_str()),
                              speaker_name)
                      .c_str(),
                  isSelected)) {
            speaker = i;
          }
          if (isSelected) {
            ImGui::SetItemDefaultFocus();
          }
        }
        ImGui::EndCombo();
      }
      ImGui::InputText("Text", &linetext);
      ImGui::Checkbox("Offscreen?", &line->offscreen);
      if (scene.lines.size() > 1) {  // prevent creating an empty scene
        ImGui::PushStyleColor(ImGuiCol_Button, m_warning_color);
        if (ImGui::Button("Remove")) {
          line = scene.lines.erase(line);
          ImGui::PopStyleColor();
          ImGui::TreePop();
          continue;
        }
        ImGui::PopStyleColor();
      }
      ImGui::TreePop();
    } else if (linetext.empty() || line->offscreen) {
      ImGui::PopStyleColor();
    }
    line->start = times[0];
    line->end = times[1];
    line->text = font->convert_utf8_to_game(linetext, true);
    line->speaker = speaker + 1;
    i++;
    line++;
  }
}

void Subtitle2Editor::draw_new_cutscene_line_form() {
  auto bank = m_subtitle_db.m_banks[m_current_language];
  auto font = get_font_bank(bank->text_version);
  ImGui::InputFloat2("Start and End Frame", m_current_scene_frame, "%.0f",
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsDecimal);
  const auto& speakers = bank->speakers;
  if (speakers.count(m_current_scene_speaker) == 0 && speakers.size() > 0) {
    // pick whatever the first one it finds is
    m_current_scene_speaker = speakers.begin()->first;
  }

  if (ImGui::BeginCombo("Speaker",
                        speakers.count(m_current_scene_speaker) == 0
                            ? "N/A"
                            : fmt::format("{} ({})",
                                          font->convert_game_to_utf8(
                                              bank->speakers.at(m_current_scene_speaker).c_str()),
                                          m_current_scene_speaker)
                                  .c_str())) {
    for (int i = 0; i < m_speaker_names.size(); ++i) {
      auto speaker_name = speaker_name_by_index(i);
      if (speakers.count(speaker_name) == 0) {
        continue;
      }
      const bool isSelected = m_current_scene_speaker == speaker_name;
      if (ImGui::Selectable(
              fmt::format("{} ({})",
                          font->convert_game_to_utf8(bank->speakers.at(speaker_name).c_str()),
                          speaker_name)
                  .c_str(),
              isSelected)) {
        m_current_scene_speaker = speaker_name;
      }
      if (isSelected) {
        ImGui::SetItemDefaultFocus();
      }
    }
    ImGui::EndCombo();
  }
  ImGui::InputText("Text", &m_current_scene_text);
  ImGui::Checkbox("Offscreen?", &m_current_scene_offscreen);
  bool rendered_text_entry_btn = false;
  if (m_current_scene_frame[0] < 0 || m_current_scene_frame[1] < 0 ||
      m_current_scene_text.empty()) {
    ImGui::PushStyleColor(ImGuiCol_Text, m_error_text_color);
    ImGui::Text("Can't add a new text entry with the current fields!");
    ImGui::PopStyleColor();
  } else {
    rendered_text_entry_btn = true;
    if (ImGui::Button("Add Text Entry")) {
      m_current_scene->lines.emplace_back(m_current_scene_frame[0], m_current_scene_frame[1],
                                          font->convert_utf8_to_game(m_current_scene_text, true),
                                          u16(speaker_index_by_name(m_current_scene_speaker)),
                                          m_current_scene_offscreen);
    }
  }
  ImGui::NewLine();
}
