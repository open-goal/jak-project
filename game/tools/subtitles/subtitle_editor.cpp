#include "subtitle_editor.h"

#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"
#include "third-party/fmt/core.h"

SubtitleEditor::SubtitleEditor() {
  m_subtitle_db = load_subtitle_project();
}

void SubtitleEditor::draw_window() {
  std::string filterPlaceholder = "Filter List...";
  std::string filter = "Filter List...";

  ImGui::Begin("Subtitle Editor");

  if (ImGui::TreeNode("Current Scene")) {
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("All Cutscenes")) {
    ImGui::InputText("Filter", &filter, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    for (auto& [sceneName, sceneInfo] : m_subtitle_db.m_banks.at(0)->m_scenes) {
      if (ImGui::TreeNode(sceneName.c_str())) {
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
            if (ImGui::Button("Select as Current")) {
            }
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
