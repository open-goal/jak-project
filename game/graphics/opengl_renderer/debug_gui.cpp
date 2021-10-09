
#include "debug_gui.h"
#include <algorithm>
#include "third-party/imgui/imgui.h"

void FrameTimeRecorder::finish_frame() {
  m_frame_times[m_idx++] = m_timer.getMs();
  if (m_idx == SIZE) {
    m_idx = 0;
  }
}

void FrameTimeRecorder::start_frame() {
  m_timer.start();
}

void FrameTimeRecorder::draw_window(const DmaStats& dma_stats) {
  auto* p_open = &m_open;
  ImGuiWindowFlags window_flags = ImGuiWindowFlags_NoDecoration |
                                  ImGuiWindowFlags_AlwaysAutoResize |
                                  ImGuiWindowFlags_NoSavedSettings |
                                  ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoNav;

  const float PAD = 10.0f;
  const ImGuiViewport* viewport = ImGui::GetMainViewport();
  ImVec2 work_pos = viewport->WorkPos;  // Use work area to avoid menu-bar/task-bar, if any!
  ImVec2 work_size = viewport->WorkSize;
  ImVec2 window_pos, window_pos_pivot;
  window_pos.x = (work_pos.x + work_size.x - PAD);
  window_pos.y = (work_pos.y + work_size.y - PAD);
  window_pos_pivot.x = 1.0f;
  window_pos_pivot.y = 1.0f;
  ImGui::SetNextWindowPos(window_pos, ImGuiCond_Always, window_pos_pivot);

  ImGui::SetNextWindowBgAlpha(0.35f);  // Transparent background
  if (ImGui::Begin("Frame Timing", p_open, window_flags)) {
    ImGui::Text("DMA: sync ms %.1f, tc %4d, sz %3d KB, ch %d", dma_stats.sync_time_ms,
                dma_stats.num_tags, (dma_stats.num_data_bytes) / (1 << 10), dma_stats.num_chunks);
    float worst = 0, total = 0;
    for (auto x : m_frame_times) {
      worst = std::max(x, worst);
      total += x;
    }
    if (total / SIZE > 17.) {
      ImGui::TextColored(ImVec4(1.0, 0.3, 0.3, 1.0), "avg: %.1f", total / SIZE);
    } else {
      ImGui::Text("avg: %.1f", total / SIZE);
    }
    ImGui::SameLine();
    if (worst > 17.) {
      ImGui::TextColored(ImVec4(1.0, 0.3, 0.3, 1.0), "worst: %.1f", worst);
    } else {
      ImGui::Text("worst: %.1f", worst);
    }

    ImGui::Separator();
    ImGui::PlotLines(
        "0-20ms",
        [](void* data, int idx) {
          auto* me = (FrameTimeRecorder*)data;
          return me->m_frame_times[(me->m_idx + idx) % SIZE];
        },
        (void*)this, SIZE, 0, nullptr, 0, 20., ImVec2(300, 40));

    ImGui::Checkbox("Run", &m_play);
    ImGui::SameLine();
    if (ImGui::Button("Single Frame Advance")) {
      m_single_frame = true;
    }
  }
  ImGui::End();
}

void OpenGlDebugGui::start_frame() {
  m_frame_timer.start_frame();
}

void OpenGlDebugGui::finish_frame() {
  m_frame_timer.finish_frame();
}

void OpenGlDebugGui::draw(const DmaStats& dma_stats) {
  if (ImGui::BeginMainMenuBar()) {
    if (ImGui::BeginMenu("Windows")) {
      ImGui::MenuItem("Frame Time Plot", nullptr, &m_draw_frame_time);
      ImGui::MenuItem("Render Debug", nullptr, &m_draw_debug);
      ImGui::EndMenu();
    }

    if (ImGui::BeginMenu("Gfx Dump")) {
      ImGui::MenuItem("Dump Next Frame!", nullptr, &m_want_save);
      bool old_replay = m_want_replay;
      ImGui::MenuItem("Load Saved Dump", nullptr, &m_want_replay);
      if (!old_replay && m_want_replay) {
        m_want_dump_load = true;
      }
      ImGui::Separator();

      ImGui::InputText("Filename", m_dump_save_name, 12);
      ImGui::EndMenu();
    }
  }
  ImGui::EndMainMenuBar();

  if (m_draw_frame_time) {
    m_frame_timer.draw_window(dma_stats);
  }
}