#pragma once

#include <optional>
#include <string_view>

#include "common/repl/nrepl/ReplClient.h"
#include "common/serialization/subtitles2/subtitles2_ser.h"

#include "third-party/imgui/imgui.h"

// TODO Later:
// - Hints, these seem less annoying but there are a lot of them

class Subtitle2Editor {
 public:
  Subtitle2Editor(GameVersion version);
  void draw_window();

 private:
  void draw_edit_options();
  void draw_repl_options();
  void draw_speaker_options();

  void draw_all_scenes(bool base_cutscenes = false);
  void draw_subtitle_options(Subtitle2Scene& scene,
                             const std::string& name,
                             bool current_scene = false);
  void draw_new_cutscene_line_form();

  bool db_loaded = false;

  GameSubtitle2DB m_subtitle_db;
  Subtitle2Scene* m_current_scene = nullptr;
  std::string m_current_scene_name = "";
  std::string m_filter;
  std::string m_filter_hints;

  ReplClient m_repl;

  float m_current_scene_frame[2] = {0, 0};
  std::string m_current_scene_text = "";
  std::string m_current_scene_speaker = "";
  bool m_current_scene_offscreen = false;
  bool m_current_scene_merge = false;
  bool m_add_new_scene_as_current = false;

  std::string m_new_scene_name = "";
  std::string m_new_scene_id = "0";

  std::string m_filter_placeholder = "Filter List...";

  std::optional<bool> m_files_saved_successfully = {};

  int m_base_language = 0;
  int m_current_language = 0;
  // bool m_base_show_lines = false;
  bool m_base_show_missing_cutscenes = true;

  // TODO - let the user customize these colors
  ImVec4 m_normal_text_color = ImVec4(1.0f, 0.0f, 1.0f, 1.0f);
  int m_selected_text_color = IM_COL32(89, 227, 225, 255);
  ImVec4 m_success_text_color = ImVec4(0.0f, 1.0f, 0.0f, 1.0f);
  ImVec4 m_error_text_color = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
  ImVec4 m_disabled_text_color = ImVec4(1.0f, 1.0f, 1.0f, 0.7f);
  ImVec4 m_warning_color = ImVec4(0.619f, 0.443f, 0.0f, 1.0f);
  int m_offscreen_text_color = IM_COL32(240, 242, 102, 255);
  // TODO - cycle speaker colors

  const std::vector<std::string> m_speaker_names;

  void repl_rebuild_text();
  void repl_play_vag(const std::string& name, bool is_scene);

  bool is_scene_in_current_lang(const std::string& scene_name);
};
