#pragma once

#include <common/serialization/subtitles/subtitles.h>

class SubtitleEditor {
 public:
  SubtitleEditor();
  // Reference to the SubtitleDB...not sure if it should be pulled out of the compiler though
  void draw_window();

 private:
  GameSubtitleDB m_subtitle_db;
};
