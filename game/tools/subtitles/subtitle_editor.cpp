#include "subtitle_editor.h"

#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"
#include "third-party/fmt/core.h"

SubtitleEditor::SubtitleEditor() : m_repl(8181) {
  m_subtitle_db = load_subtitle_project();
  m_filter = m_filter_placeholder;
  m_repl.connect();
}

// Set Continue Point:
// - (start 'play (get-continue-by-name *game-info* "NAME"))

// Move Jak:
// - (move-to-point! (-> *target* control) (new 'static 'vector :x (meters -155.0) :y (meters 33.0) :z (meters 187.0)))

// Reset Game:
// - (set! (-> *game-info* mode) 'debug)
// - (initialize! *game-info* 'game (the-as game-save #f) (the-as string #f))

// Get a Process by Name:
// - (process-by-name \"flutflutegg-1\" *active-pool*)

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");

  if (ImGui::Button("TEST!")) {
    m_repl.eval("(set! (-> *game-info* mode) 'debug)");
    m_repl.eval("(initialize! *game-info* 'game (the-as game-save #f) (the-as string #f))");
    m_repl.eval("(move-to-point! (-> *target* control) (new 'static 'vector :x (meters -51.0) :y (meters 10.0) :z (meters -7.0)))");
    m_repl.eval("(send-event *camera* 'teleport)");
    m_repl.eval("(send-event (process-by-name \"bird-lady-4\" *active-pool*) 'play-anim)");
  }

  if (ImGui::TreeNode("Current Scene")) {
    if (m_current_scene.has_value()) {
      if (ImGui::Button("New Text Entry")) {
      }
      if (ImGui::Button("New Mute Entry")) {
      }
      if (ImGui::Button("Play Scene")) {
      }
      for (int i = 0; i < m_current_scene->m_lines.size(); i++) {
        auto& subtitleLine = m_current_scene->m_lines.at(i);
        if (ImGui::TreeNode(fmt::format("{}", i).c_str(),
                            fmt::format("[{}] {} - '{}...'", subtitleLine.frame,
                                        subtitleLine.speaker, subtitleLine.line.substr(0, 20))
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
    } else {
      ImGui::Text("Select a Scene from Below!");
    }
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("All Cutscenes")) {
    ImGui::InputText("Filter", &m_filter, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    for (auto& [sceneName, sceneInfo] : m_subtitle_db.m_banks.at(0)->m_scenes) {
      if ((!m_filter.empty() && m_filter != m_filter_placeholder) &&
          sceneName.find(m_filter) == std::string::npos) {
        continue;
      }
      if (ImGui::TreeNode(sceneName.c_str())) {
        if (ImGui::Button("Select as Current")) {
          m_current_scene = std::make_optional(sceneInfo);
        }
        for (int i = 0; i < sceneInfo.m_lines.size(); i++) {
          auto& subtitleLine = sceneInfo.m_lines.at(i);
          if (ImGui::TreeNode(fmt::format("{}", i).c_str(),
                              fmt::format("[{}] {} - '{}...'", subtitleLine.frame,
                                          subtitleLine.speaker, subtitleLine.line.substr(0, 20))
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
