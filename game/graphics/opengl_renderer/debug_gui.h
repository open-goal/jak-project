#pragma once

/*!
 * @file debug_gui.h
 * The debug menu-bar and frame timing window
 */

#include "common/dma/dma.h"
#include "common/util/Timer.h"
#include "common/versions/versions.h"

class FrameTimeRecorder {
 public:
  static constexpr int SIZE = 60 * 5;

  void finish_frame();
  void start_frame();
  void draw_window(const DmaStats& dma_stats);
  bool should_advance_frame() {
    if (m_single_frame) {
      m_single_frame = false;
      return true;
    }
    return m_play;
  }

  bool do_gl_finish = false;

 private:
  float m_frame_times[SIZE] = {0};
  float m_last_frame_time = 0;
  int m_idx = 0;
  Timer m_compute_timer;
  Timer m_fps_timer;
  bool m_open = true;

  bool m_play = true;
  bool m_single_frame = false;
};

class OpenGlDebugGui {
 public:
  OpenGlDebugGui() {}

  void start_frame();
  void finish_frame();
  void draw(const DmaStats& dma_stats);
  bool should_draw_render_debug() const { return master_enable && m_draw_debug; }
  bool should_draw_profiler() const { return master_enable && m_draw_profiler; }
  bool should_draw_subtitle_editor() const { return master_enable && m_subtitle_editor; }
  bool should_draw_filters_menu() const { return master_enable && m_filters_menu; }
  bool should_draw_loader_menu() const { return master_enable && m_draw_loader; }
  const char* screenshot_name() const { return m_screenshot_save_name; }

  bool should_advance_frame() { return m_frame_timer.should_advance_frame(); }
  bool should_gl_finish() const { return m_frame_timer.do_gl_finish; }

  bool get_screenshot_flag() {
    if (m_want_screenshot) {
      m_want_screenshot = false;
      return true;
    }
    return false;
  }

  bool small_profiler = false;
  bool record_events = false;
  bool dump_events = false;
  bool want_reboot_in_debug = false;

  int screenshot_width = 1920;
  int screenshot_height = 1080;
  int screenshot_samples = 16;
  bool screenshot_hotkey_enabled = true;

  bool master_enable = false;

 private:
  FrameTimeRecorder m_frame_timer;
  bool m_draw_frame_time = false;
  bool m_draw_profiler = false;
  bool m_draw_debug = false;
  bool m_draw_loader = false;
  bool m_subtitle_editor = false;
  bool m_filters_menu = false;
  bool m_want_screenshot = false;
  char m_screenshot_save_name[256] = "screenshot.png";
  float target_fps_input = 60.f;
};
