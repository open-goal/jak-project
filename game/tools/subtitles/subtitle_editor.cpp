#include "subtitle_editor.h"

#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"
#include "third-party/fmt/core.h"

SubtitleEditor::SubtitleEditor() : m_repl(8181) {
  m_subtitle_db = load_subtitle_project();
  m_filter = m_filter_placeholder;
  m_filter_hints = m_filter_placeholder;
  m_repl.connect();
}

// Set Continue Point:
// - (start 'play (get-continue-by-name *game-info* "NAME"))

// Move Jak:
// - (move-to-point! (-> *target* control) (new 'static 'vector :x (meters -155.0) :y (meters 33.0)
// :z (meters 187.0)))

// Reset Game:
// - (set! (-> *game-info* mode) 'debug)
// - (initialize! *game-info* 'game (the-as game-save #f) (the-as string #f))

// Get a Process by Name:
// - (process-by-name \"flutflutegg-1\" *active-pool*)
// - better - (the-as assistant (process-by-name "assistant-11" *active-pool*))

// Teleport the Camera:
// - m_repl.eval("(send-event *camera* 'teleport)");

// TODO - have to special case characters like '

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");

  // TODO - REPL status and reconnection way / include instructions right here!
  if (ImGui::TreeNode("Currently Selected Movie")) {
    if (m_current_scene.has_value()) {
      if (ImGui::Button("Play Scene")) {
        // TODO - trigger the REPL!
      }
      ImGui::NewLine();
      ImGui::InputInt("Frame Number", &m_current_scene_frame);
      ImGui::InputText("Text", &m_current_scene_text);
      ImGui::InputText("Speaker", &m_current_scene_speaker);
      ImGui::Checkbox("Offscreen", &m_current_scene_offscreen);
      if (ImGui::Button("Add Text Entry")) {
        // TODO - validation
        m_current_scene->add_line(m_current_scene_frame, m_current_scene_text,
                                  m_current_scene_speaker, m_current_scene_offscreen);
      }
      ImGui::SameLine();
      if (ImGui::Button("Add Clear Screen Entry")) {
        // TODO - validation
        m_current_scene->add_line(m_current_scene_frame, "", "", false);
      }
      ImGui::NewLine();
      for (int i = 0; i < m_current_scene->m_lines.size(); i++) {
        auto& subtitleLine = m_current_scene->m_lines.at(i);
        std::string summary;
        if (subtitleLine.line.empty()) {
          summary = fmt::format("[{}] Clear Screen", subtitleLine.frame);
        } else {
          summary = fmt::format("[{}] {} - '{}...'", subtitleLine.frame, subtitleLine.speaker,
                                subtitleLine.line.substr(0, 30));
        }
        if (subtitleLine.line.empty()) {
          ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 255, 255, 175));
        }
        if (ImGui::TreeNode(fmt::format("{}", i).c_str(), summary.c_str())) {
          if (subtitleLine.line.empty()) {
            ImGui::PopStyleColor();
          }
          ImGui::InputInt("Starting Frame", &subtitleLine.frame);
          ImGuiInputTextFlags flags = ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsUppercase |
                                      ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsNoBlank;
          ImGui::InputText("Speaker", &subtitleLine.speaker, flags);
          ImGui::InputText("Text", &subtitleLine.line, flags);
          ImGui::Checkbox("Offscreen?", &subtitleLine.offscreen);
          ImGui::TreePop();
        } else if (subtitleLine.line.empty()) {
          ImGui::PopStyleColor();
        }
      }
    } else {
      ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 0, 0, 255));
      ImGui::Text("Select a Scene from Below!");
      ImGui::PopStyleColor();
    }
    ImGui::TreePop();
  }

  if (ImGui::TreeNode("All Movies")) {
    ImGui::InputText("Filter", &m_filter, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    // How does the subtitle code consistently map the same language to the same int id?  Hardcoded
    // somewhere?
    for (auto& [sceneName, sceneInfo] : m_subtitle_db.m_banks.at(0)->m_scenes) {
      if (sceneInfo.m_kind != SubtitleSceneKind::Movie) {
        continue;
      }
      if ((!m_filter.empty() && m_filter != m_filter_placeholder) &&
          sceneName.find(m_filter) == std::string::npos) {
        continue;
      }
      if (m_current_scene->m_name == sceneInfo.m_name) {
        ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(0, 255, 0, 255));
      }
      if (ImGui::TreeNode(sceneName.c_str())) {
        if (m_current_scene->m_name == sceneInfo.m_name) {
          ImGui::PopStyleColor();
        } else {
          if (ImGui::Button("Select as Current")) {
            m_current_scene = std::make_optional(sceneInfo);
          }
        }
        for (int i = 0; i < sceneInfo.m_lines.size(); i++) {
          auto& subtitleLine = sceneInfo.m_lines.at(i);
          std::string summary;
          if (subtitleLine.line.empty()) {
            summary = fmt::format("[{}] Clear Screen", subtitleLine.frame);
          } else {
            summary = fmt::format("[{}] {} - '{}...'", subtitleLine.frame, subtitleLine.speaker,
                                  subtitleLine.line.substr(0, 30));
          }
          if (subtitleLine.line.empty()) {
            ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 255, 255, 175));
          }
          if (ImGui::TreeNode(fmt::format("{}", i).c_str(), summary.c_str())) {
            if (subtitleLine.line.empty()) {
              ImGui::PopStyleColor();
            }
            ImGui::InputInt("Starting Frame", &subtitleLine.frame);
            ImGuiInputTextFlags flags = ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsUppercase |
                                        ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsNoBlank;
            ImGui::InputText("Speaker", &subtitleLine.speaker, flags);
            ImGui::InputText("Text", &subtitleLine.line, flags);
            ImGui::Checkbox("Offscreen?", &subtitleLine.offscreen);
            ImGui::TreePop();
          } else if (subtitleLine.line.empty()) {
            ImGui::PopStyleColor();
          }
        }
        ImGui::TreePop();
      } else if (m_current_scene->m_name == sceneInfo.m_name) {
        ImGui::PopStyleColor();
      }
    }
    ImGui::TreePop();
  }

  // TODO - are playing arbitrary hints easy? Find out later
  // TODO - flag to disable keyboard controls
  if (ImGui::TreeNode("Hints")) {
    ImGui::InputText("Filter", &m_filter_hints,
                     ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    for (auto& [sceneName, sceneInfo] : m_subtitle_db.m_banks.at(0)->m_scenes) {
      // TODO - hints are being labelled as invalid?
      if (sceneInfo.m_kind == SubtitleSceneKind::Movie) {
        continue;
      }
      if ((!m_filter_hints.empty() && m_filter_hints != m_filter_placeholder) &&
          sceneName.find(m_filter_hints) == std::string::npos) {
        continue;
      }
      if (ImGui::TreeNode(sceneName.c_str())) {
        for (int i = 0; i < sceneInfo.m_lines.size(); i++) {
          auto& subtitleLine = sceneInfo.m_lines.at(i);
          if (ImGui::TreeNode(fmt::format("{}", i).c_str(),
                              fmt::format("[{}] {} - '{}...'", subtitleLine.frame,
                                          subtitleLine.speaker, subtitleLine.line.substr(0, 30))
                                  .c_str())) {
            ImGui::Checkbox("Offscreen?", &subtitleLine.offscreen);
            ImGui::InputInt("Starting Frame", &subtitleLine.frame);
            ImGuiInputTextFlags flags = ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsUppercase |
                                        ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsNoBlank;
            ImGui::InputText("Speaker", &subtitleLine.speaker, flags);
            ImGui::InputText("Text", &subtitleLine.line, flags);
            ImGui::TreePop();
          }
        }
        ImGui::TreePop();
      }
    }
    ImGui::TreePop();
  }
  ImGui::End();
}
