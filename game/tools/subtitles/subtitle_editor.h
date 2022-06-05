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
  std::string m_filter_hints;

  ReplClient m_repl;

  int m_current_scene_frame = 0;
  std::string m_current_scene_text = "";
  std::string m_current_scene_speaker = "";
  bool m_current_scene_offscreen = false;

  std::string m_filter_placeholder = "Filter List...";
};
