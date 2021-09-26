#pragma once

/*!
 * @file debug_gui.h
 * The debug menu-bar and frame timing window
 */

#include "common/util/Timer.h"
#include "game/graphics/dma/dma.h"

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

 private:
  float m_frame_times[SIZE] = {0};
  int m_idx = 0;
  Timer m_timer;
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
  bool& want_save() { return m_want_save; }
  bool& want_dump_replay() { return m_want_replay; }
  bool& want_dump_load() { return m_want_dump_load; }
  const char* dump_name() const { return m_dump_save_name; }

  bool should_advance_frame() { return m_frame_timer.should_advance_frame(); }

 private:
  FrameTimeRecorder m_frame_timer;
  bool m_draw_frame_time = false;
  bool m_draw_debug = false;
  bool m_want_save = false;
  bool m_want_replay = false;
  bool m_want_dump_load = false;
  char m_dump_save_name[256] = "dump.bin";
};