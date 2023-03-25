#include "filter_menu.h"

#include "game/graphics/gfx.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

void to_json(json& j, const DebugTextFilter& obj) {
  j = json{{"content", obj.content}, {"type", obj.type}};
}

void from_json(const json& j, DebugTextFilter& obj) {
  j.at("content").get_to(obj.content);
  j.at("type").get_to(obj.type);
}

// TODO:
// - ability to remove individual filter

FiltersMenu::FiltersMenu() {}

void FiltersMenu::draw_window() {
  ImGui::Begin("Filters");

  ImGui::SetNextItemOpen(true);
  if (ImGui::TreeNode("Debug Text Filters")) {
    auto& current_filters = Gfx::g_debug_settings.text_filters;
    // Iterate and display all current debug text filters
    for (size_t i = 0; i < current_filters.size(); i++) {
      std::string label = "contains?";
      if (current_filters[i].type == DebugTextFilter::Type::NOT_CONTAINS) {
        label = "not-contains?";
      } else if (current_filters[i].type == DebugTextFilter::Type::REGEX) {
        label = "regex?";
      }
      ImGui::Text("%s", label.c_str());
      ImGui::SameLine();
      ImGui::InputText(fmt::format("##filter-{}", i).c_str(), &current_filters[i].content);
    }

    if (ImGui::Button("Add Contains")) {
      DebugTextFilter new_filter;
      new_filter.type = DebugTextFilter::Type::CONTAINS;
      current_filters.push_back(new_filter);
    }
    ImGui::SameLine();
    if (ImGui::Button("Add Not-Contains")) {
      DebugTextFilter new_filter;
      new_filter.type = DebugTextFilter::Type::NOT_CONTAINS;
      current_filters.push_back(new_filter);
    }
    if (ImGui::Button("Clear Filters")) {
      current_filters.clear();
    }
    // TODO - can't use regexes because i can't check it it's a valid regex without using a
    // try-catch and this has issues running within gk
    //
    // An option is to bring in boost's regex lib
    /*ImGui::SameLine();
    if (ImGui::Button("Add Regex")) {
      DebugTextFilter new_filter;
      new_filter.type = DebugTextFilter::Type::REGEX;
      current_filters.push_back(new_filter);
    }*/

    if (ImGui::Checkbox("Enable Distance Check", &Gfx::g_debug_settings.text_check_range)) {
    }
    if (Gfx::g_debug_settings.text_check_range) {
      if (ImGui::SliderFloat("Max Range", &Gfx::g_debug_settings.text_max_range, 0, 250)) {
      }
    }

    ImGui::TreePop();
  }

  ImGui::End();
}
