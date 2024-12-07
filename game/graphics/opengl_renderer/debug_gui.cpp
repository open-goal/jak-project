#include "debug_gui.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/util/string_util.h"

#include "game/graphics/display.h"
#include "game/graphics/gfx.h"
#include "game/graphics/screenshot.h"
#include "game/system/hid/sdl_util.h"

#include "fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_style.h"

void FrameTimeRecorder::finish_frame() {
  m_frame_times[m_idx++] = m_compute_timer.getMs();
  if (m_idx == SIZE) {
    m_idx = 0;
  }
}

void FrameTimeRecorder::start_frame() {
  m_compute_timer.start();
  float frame_time = m_fps_timer.getSeconds();
  m_last_frame_time = (0.9 * m_last_frame_time) + (0.1 * frame_time);
  m_fps_timer.start();
}

void FrameTimeRecorder::draw_window(const DmaStats& /*dma_stats*/) {
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

  ImGui::SetNextWindowBgAlpha(0.85f);  // Transparent background
  if (ImGui::Begin("Frame Timing", p_open, window_flags)) {
    //    ImGui::Text("DMA: sync ms %.1f, tc %4d, sz %3d KB, ch %d", dma_stats.sync_time_ms,
    //                dma_stats.num_tags, (dma_stats.num_data_bytes) / (1 << 10),
    //                dma_stats.num_chunks);
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
    ImGui::SameLine();
    ImGui::Text("fps-avg: %.1f", 1.f / m_last_frame_time);

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
    ImGui::SameLine();
    ImGui::Checkbox("GLFinish", &do_gl_finish);
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
    if (ImGui::BeginMenu("Debugging")) {
      ImGui::MenuItem("Frame Time Plot", nullptr, &m_draw_frame_time);
      ImGui::MenuItem("Render Debug", nullptr, &m_draw_debug);
      ImGui::MenuItem("Profiler", nullptr, &m_draw_profiler);
      ImGui::MenuItem("Small Profiler", nullptr, &small_profiler);
      ImGui::MenuItem("Loader", nullptr, &m_draw_loader);
      if (ImGui::MenuItem("Reboot In Debug Mode!")) {
        want_reboot_in_debug = true;
      }
      ImGui::EndMenu();
    }

    if (ImGui::BeginMenu("Tools")) {
      if (ImGui::BeginMenu("Screenshot")) {
        ImGui::MenuItem("Screenshot Next Frame!", nullptr, &m_want_screenshot);
        ImGui::InputText("File", g_screen_shot_settings->name,
                         sizeof(g_screen_shot_settings->name));
        ImGui::InputInt("Width", &g_screen_shot_settings->width);
        ImGui::InputInt("Height", &g_screen_shot_settings->height);
        ImGui::InputInt("MSAA", &g_screen_shot_settings->msaa);
        ImGui::Checkbox("Quick-Screenshot on F2", &screenshot_hotkey_enabled);
        ImGui::EndMenu();
      }
      ImGui::MenuItem("Subtitle Editor", nullptr, &m_subtitle_editor);
      ImGui::MenuItem("Debug Text Filter", nullptr, &m_filters_menu);
      ImGui::EndMenu();
    }

    if (ImGui::BeginMenu("Settings")) {
      if (ImGui::TreeNode("ImGui Styling (restart required for these)")) {
        ImGui::InputInt("Font Size", &Gfx::g_debug_settings.imgui_font_size);
        ImGui::Checkbox("Monospaced Font", &Gfx::g_debug_settings.monospaced_font);
        if (ImGui::Checkbox("Alternate Style", &Gfx::g_debug_settings.alternate_style)) {
          if (Gfx::g_debug_settings.alternate_style) {
            ImGui::applyAlternateStyle();
          } else {
            ImGui::applyClassicStyle();
          }
        }
        ImGui::TreePop();
      }
      ImGui::Checkbox("Ignore Hide ImGui Bind", &Gfx::g_debug_settings.ignore_hide_imgui);
      if (ImGui::TreeNode("Frame Rate")) {
        ImGui::Checkbox("Framelimiter", &Gfx::g_global_settings.framelimiter);
        ImGui::InputFloat("Target FPS", &target_fps_input);
        if (ImGui::MenuItem("Apply")) {
          Gfx::g_global_settings.target_fps = target_fps_input;
        }
        ImGui::Separator();
        ImGui::Checkbox("Accurate Lag Mode", &Gfx::g_global_settings.experimental_accurate_lag);
        ImGui::Checkbox("Sleep in Frame Limiter", &Gfx::g_global_settings.sleep_in_frame_limiter);
        ImGui::TreePop();
      }
      ImGui::Checkbox("Treat Pad0 as Pad1", &Gfx::g_debug_settings.treat_pad0_as_pad1);
      auto is_keyboard_enabled =
          Display::GetMainDisplay()->get_input_manager()->is_keyboard_enabled();
      if (ImGui::Checkbox("Enable Keyboard (forced on if no controllers detected)",
                          &is_keyboard_enabled)) {
        Display::GetMainDisplay()->get_input_manager()->enable_keyboard(is_keyboard_enabled);
      }
      ImGui::EndMenu();
    }

    if (ImGui::BeginMenu("Event Profiler")) {
      if (ImGui::Checkbox("Record Events", &record_events)) {
        prof().set_enable(record_events);
      }
      ImGui::SameLine();
      ImGui::Text("%s",
                  fmt::format("({}/{})", prof().get_next_idx(), prof().get_max_events()).c_str());
      ImGui::InputInt("Event Buffer Size", &max_event_buffer_size);
      if (ImGui::Button("Resize")) {
        prof().update_event_buffer_size(max_event_buffer_size);
      }
      if (ImGui::Button("Reset Events")) {
        prof().clear();
      }
      ImGui::Separator();
      ImGui::Checkbox("Enable Compression", &prof().m_enable_compression);
      if (ImGui::Button("Dump to File")) {
        record_events = false;
        prof().dump_to_json();
      }
      // if (ImGui::Button("Open dump folder")) {
      //  // TODO - https://github.com/mlabbe/nativefiledialog
      // }
      ImGui::EndMenu();
    }

    if (!Gfx::g_debug_settings.ignore_hide_imgui) {
      std::string button_text =
          fmt::format("Click here or Press {} to hide Toolbar",
                      sdl_util::get_keyboard_button_name(Gfx::g_debug_settings.hide_imgui_key,
                                                         InputModifiers()));

      ImVec2 text_size = ImGui::CalcTextSize(button_text.c_str());
      float button_width = text_size.x + ImGui::GetStyle().FramePadding.x * 2;
      float button_height = text_size.y + ImGui::GetStyle().FramePadding.y * 2;

      ImGui::PushStyleColor(ImGuiCol_Header, ImGui::GetStyleColorVec4(ImGuiCol_MenuBarBg));
      ImGui::PushStyleColor(ImGuiCol_HeaderHovered,
                            ImGui::GetStyleColorVec4(ImGuiCol_HeaderHovered));
      ImGui::PushStyleColor(ImGuiCol_HeaderActive, ImGui::GetStyleColorVec4(ImGuiCol_HeaderActive));

      if (ImGui::Selectable(button_text.c_str(), false, ImGuiSelectableFlags_DontClosePopups,
                            ImVec2(button_width, button_height))) {
        std::shared_ptr<GfxDisplay> display = Display::GetMainDisplay();
        display->set_imgui_visible(false);
      }
      ImGui::PopStyleColor(3);
    }
  }
  ImGui::EndMainMenuBar();

  if (m_draw_frame_time) {
    m_frame_timer.draw_window(dma_stats);
  }
}
