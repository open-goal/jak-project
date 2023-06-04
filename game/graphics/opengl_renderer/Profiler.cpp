#include "Profiler.h"

#include <algorithm>

#include "common/log/log.h"
#include "common/util/colors.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

ProfilerNode::ProfilerNode(const std::string& name) : m_name(name) {}

ProfilerNode* ProfilerNode::make_child(const std::string& name) {
  m_children.emplace_back(name);
  return &m_children.back();
}

void ProfilerNode::finish() {
  if (m_finished) {
    lg::error("finish() called twice on {}", m_name);
  } else {
    m_stats.duration = m_timer.getSeconds();
    float total_child_time = 0;
    for (const auto& child : m_children) {
      if (!child.finished()) {
        lg::error("finish() not called on {}", child.name());
      }
      total_child_time += child.m_stats.duration;
      m_stats.add_draw_stats(child.m_stats);
    }

    if (!m_children.empty()) {
      float unknown_time = m_stats.duration - total_child_time;
      if (unknown_time > 0.0001 || unknown_time > (m_stats.duration * 0.05)) {
        ProfilerNode unk("unknown");
        unk.m_stats.duration = unknown_time;
        m_children.push_back(unk);
      }
    }
  }
  m_finished = true;
}

void ProfilerNode::sort(ProfilerSort mode) {
  if (mode == ProfilerSort::NONE) {
    return;
  }
  std::sort(m_children.begin(), m_children.end(),
            [=](const ProfilerNode& a, const ProfilerNode& b) {
              switch (mode) {
                case ProfilerSort::DRAW_CALLS:
                  return a.m_stats.draw_calls > b.m_stats.draw_calls;
                case ProfilerSort::TIME:
                  return a.m_stats.duration > b.m_stats.duration;
                case ProfilerSort::TRIANGLES:
                  return a.m_stats.triangles > b.m_stats.triangles;
                default:
                  ASSERT(false);
              }
            });
  for (auto& child : m_children) {
    child.sort(mode);
  }
}

ScopedProfilerNode ProfilerNode::make_scoped_child(const std::string& name) {
  return ScopedProfilerNode(make_child(name));
}

Profiler::Profiler() : m_root("root") {}

void Profiler::clear() {
  m_root = ProfilerNode("root");
}

void Profiler::finish() {
  m_root.finish();
}

void Profiler::draw() {
  ImGui::Begin("Profiler");
  const char* listbox_entries[] = {"None", "Time", "Draw Calls", "Tris"};
  ImGui::Combo("Sort", &m_mode_selector, listbox_entries, 4);
  m_root.sort((ProfilerSort)m_mode_selector);
  ImGui::SameLine();
  bool all = ImGui::Button("Expand All");
  ImGui::Dummy(ImVec2(0.0f, 80.0f));
  draw_node(m_root, all, 0, 0.f);
  ImGui::End();
}

u32 name_to_color(const std::string& name) {
  u64 val = std::hash<std::string>{}(name);
  return colors::common_colors[val % colors::COLOR_COUNT] | 0xff000000;
}

void Profiler::draw_node(ProfilerNode& node, bool expand, int depth, float start_time) {
  u32 color = 0xFFFFFFFF;

  constexpr int origin_x = 40;
  constexpr int origin_y = 60;
  constexpr int row_height = 15;
  constexpr int px_per_ms = 75;

  if (node.m_stats.duration > 0.00001) {
    color = name_to_color(node.m_name);
  }
  bool color_orange = false;
  ImGui::PushStyleColor(ImGuiCol_Text, color);
  auto str =
      fmt::format("{:20s} {:.2f}ms {:6d} tri {:4d} draw", node.m_name, node.m_stats.duration * 1000,
                  node.m_stats.triangles, node.m_stats.draw_calls);
  if (node.m_children.empty()) {
    ImGui::Text("   %s", str.c_str());
    color_orange = ImGui::IsItemHovered();
  } else {
    if (expand) {
      ImGui::SetNextItemOpen(true);
    }
    if (ImGui::TreeNode(node.m_name.c_str(), "%s", str.c_str())) {
      color_orange = ImGui::IsItemHovered();
      float child_start = start_time;
      for (auto& child : node.m_children) {
        draw_node(child, expand, depth + 1, child_start);
        child_start += child.m_stats.duration;
      }
      ImGui::TreePop();
    }
  }

  if (node.m_stats.duration > 0.00001 || color_orange) {
    if (color_orange) {
      color = 0xff00a5ff;
    }
    auto dl = ImGui::GetWindowDrawList();
    auto window_pos = ImGui::GetWindowPos();
    float x0 = window_pos.x + origin_x + px_per_ms * 1000 * start_time;
    float x1 = x0 + px_per_ms * 1000 * node.m_stats.duration;
    float y0 = window_pos.y + origin_y + depth * row_height;
    float y1 = y0 + row_height;

    dl->AddRectFilled(ImVec2(x0, y0), ImVec2(x1, y1), color);
  }

  ImGui::PopStyleColor();
}

std::string Profiler::to_string() {
  m_root.sort(ProfilerSort::TIME);
  std::string str;
  m_root.to_string_helper(str, 0);
  return str;
}

void ProfilerNode::to_string_helper(std::string& str, int depth) const {
  str +=
      fmt::format("{}{:.2f} ms {:30s}\n", std::string(depth, ' '), m_stats.duration * 1000, m_name);
  for (const auto& child : m_children) {
    child.to_string_helper(str, depth + 1);
  }
}

void FramePlot::push(float val) {
  m_buffer[m_idx++] = val;
  if (m_idx == SIZE) {
    m_idx = 0;
  }
}

void FramePlot::draw(float max) {
  float worst = 0, total = 0;
  for (auto x : m_buffer) {
    worst = std::max(x, worst);
    total += x;
  }
  ImGui::SameLine();
  ImGui::Text("avg: %.1f", total / SIZE);

  ImGui::SameLine();
  ImGui::Text("worst: %.1f", worst);

  ImGui::Separator();
  ImGui::PlotLines(
      "",
      [](void* data, int idx) {
        auto* me = (FramePlot*)data;
        return me->m_buffer[(me->m_idx + idx) % SIZE];
      },
      (void*)this, SIZE, 0, nullptr, 0, max, ImVec2(300, 20));
}

void SmallProfiler::draw(const std::string& status, const SmallProfilerStats& stats) {
  ImGuiWindowFlags window_flags = ImGuiWindowFlags_NoDecoration |
                                  ImGuiWindowFlags_AlwaysAutoResize |
                                  ImGuiWindowFlags_NoSavedSettings |
                                  ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav;
  const float PAD = 10.0f;
  const ImGuiViewport* viewport = ImGui::GetMainViewport();
  ImVec2 work_pos = viewport->WorkPos;  // Use work area to avoid menu-bar/task-bar, if any!
  ImVec2 work_size = viewport->WorkSize;
  ImVec2 window_pos, window_pos_pivot;
  window_pos.x = (work_pos.x + PAD);
  window_pos.y = (work_pos.y + work_size.y - PAD);
  window_pos_pivot.x = 0.0f;
  window_pos_pivot.y = 1.0f;
  ImGui::SetNextWindowPos(window_pos, ImGuiCond_Always, window_pos_pivot);

  ImGui::SetNextWindowBgAlpha(0.85f);  // Transparent background
  if (ImGui::Begin("Profiler (short)", nullptr, window_flags)) {
    ImGui::Text(" tri: %7d\n", stats.triangles);
    ImGui::Text("  DC: %4d\n", stats.draw_calls);
    if (!status.empty()) {
      ImGui::Text("%s", status.c_str());
    }

    for (int i = 0; i < (int)BucketCategory::MAX_CATEGORIES; i++) {
      m_plots[i].push(stats.time_per_category[i] * 1000.f);
      ImGui::Text("%6s", BUCKET_CATEGORY_NAMES[i]);
      m_plots[i].draw(3.f);
    }
  }
  ImGui::End();
}
