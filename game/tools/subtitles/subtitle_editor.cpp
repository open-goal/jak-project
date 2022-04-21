#include "subtitle_editor.h"

#include "third-party/imgui/imgui.h"
#include "third-party/fmt/core.h"

SubtitleEditor::SubtitleEditor() {
  m_subtitle_db = load_subtitle_project();
}

void SubtitleEditor::draw_window() {
  ImGui::Begin("Subtitle Editor");

  if (ImGui::TreeNode("Current Scene")) {
    ImGui::TreePop();
  }
  if (ImGui::TreeNode("All Cutscenes")) {
    char str0[128] = "Filter List...";
    ImGui::InputText("Filter", str0, 128, ImGuiInputTextFlags_::ImGuiInputTextFlags_AutoSelectAll);
    for (auto& [sceneName, sceneInfo] : m_subtitle_db.m_banks.at(0)->m_scenes) {
      if (ImGui::TreeNode(sceneName.c_str())) {
        for (auto subtitleLine : sceneInfo.lines()) {
          if (ImGui::TreeNode(fmt::format("[{}] {} - '{}...'", subtitleLine.frame,
                                          subtitleLine.speaker, subtitleLine.line.substr(0, 20))
                                  .c_str())) {
            ImGui::Checkbox("Offscreen?", &subtitleLine.offscreen);
            ImGui::InputInt("Starting Frame", &subtitleLine.frame);
            static ImGuiInputTextFlags flags =
                ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsUppercase |
                ImGuiInputTextFlags_::ImGuiInputTextFlags_CharsNoBlank;
            ImGui::InputText("Speaker", str0, 128, flags);
            ImGui::InputText("Text", str0, 128, flags);
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
