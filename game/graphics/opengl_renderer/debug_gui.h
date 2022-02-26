#pragma once

/*!
 * @file debug_gui.h
 * The debug menu-bar and frame timing window
 */

#include "common/util/Timer.h"
#include "common/dma/dma.h"

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
  void start_frame();
  void finish_frame();
  void draw(const DmaStats& dma_stats);
  bool should_draw_render_debug() const { return m_draw_debug; }
  bool should_draw_profiler() const { return m_draw_profiler; }
  bool& want_save() { return m_want_save; }
  bool& want_dump_replay() { return m_want_replay; }
  bool& want_dump_load() { return m_want_dump_load; }
  const char* dump_name() const { return m_dump_save_name; }
  const char* screenshot_name() const { return m_screenshot_save_name; }

  bool should_advance_frame() { return m_frame_timer.should_advance_frame(); }
  bool should_gl_finish() { return m_frame_timer.do_gl_finish; }

  bool get_screenshot_flag() {
    if (m_want_screenshot) {
      m_want_screenshot = false;
      return true;
    }
    return false;
  }

  bool get_vsync_flag() { return m_vsync; }

  bool framelimiter = false;
  float target_fps = 60.f;
  bool experimental_accurate_lag = false;
  bool sleep_in_frame_limiter = true;
  bool small_profiler = true;

 private:
  FrameTimeRecorder m_frame_timer;
  bool m_draw_frame_time = false;
  bool m_draw_profiler = false;
  bool m_draw_debug = false;
  bool m_want_save = false;
  bool m_want_replay = false;
  bool m_want_dump_load = false;
  bool m_want_screenshot = false;
  char m_dump_save_name[256] = "dump.bin";
  char m_screenshot_save_name[256] = "screenshot.png";
  bool m_vsync = true;
  float m_target_fps_text = 60.0;
};
