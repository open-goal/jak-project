#pragma once

#include <common/serialization/subtitles/subtitles.h>
#include <optional>
#include <common/nrepl/ReplClient.h>

class SubtitleEditor {
 public:
  SubtitleEditor();
  // Reference to the SubtitleDB...not sure if it should be pulled out of the compiler though
  void draw_window();

 private:
  GameSubtitleDB m_subtitle_db;
  std::optional<GameSubtitleSceneInfo> m_current_scene = {};
  std::string m_filter;

  ReplClient m_repl;

  std::string m_filter_placeholder = "Filter List...";
};
