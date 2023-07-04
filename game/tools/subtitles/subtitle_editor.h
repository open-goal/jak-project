#pragma once

#include <optional>

#include "subtitle_editor_db.h"
#include "subtitle_editor_repl_client.h"

#include <common/serialization/subtitles/subtitles_v2.h>

#include "third-party/imgui/imgui.h"

class SubtitleEditor {
 public:
  SubtitleEditor();
  void draw_window();

 private:
  void draw_edit_options();
  void draw_repl_options();

  void draw_speaker_options();

  void draw_scene_section_header(const bool non_cutscenes);
  void draw_all_cutscenes(bool base_cutscenes = false);
  void draw_all_non_cutscenes(bool base_cutscenes);
  void draw_new_cutscene_line_form();
  void draw_subtitle_options(GameSubtitleSceneInfo& scene, bool current_scene = false);

  bool db_loaded = false;
  GameSubtitleDB m_subtitle_db;
  SubtitleEditorReplClient m_repl;

  // Jak 1 Specifics
  Jak1SubtitleEditorDB m_jak1_editor_db;

  // GUI Controls
  int m_base_language = 0;
  int m_current_language = 0;
  bool m_base_show_missing_cutscenes = true;
  std::string m_filter_cutscenes;
  std::string m_filter_non_cutscenes;
  std::shared_ptr<GameSubtitleSceneInfo> m_current_scene = nullptr;

  // GUI Styling
  ImVec4 m_normal_text_color = ImVec4(1.0f, 0.0f, 1.0f, 1.0f);
  int m_selected_text_color = IM_COL32(89, 227, 225, 255);
  ImVec4 m_success_text_color = ImVec4(0.0f, 1.0f, 0.0f, 1.0f);
  ImVec4 m_error_text_color = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
  ImVec4 m_disabled_text_color = ImVec4(1.0f, 1.0f, 1.0f, 0.7f);
  ImVec4 m_warning_color = ImVec4(0.619f, 0.443f, 0.0f, 1.0f);
  int m_offscreen_text_color = IM_COL32(240, 242, 102, 255);

  int m_current_scene_frame = 0;
  std::string m_current_scene_text = "";
  std::string m_current_scene_speaker = "";
  bool m_current_scene_offscreen = false;

  std::string m_new_scene_name = "";
  std::string m_new_scene_id = "0";

  std::string m_new_scene_group_name = "";

  std::string m_filter_placeholder = "Filter List...";

  std::optional<bool> m_files_saved_successfully = {};

  bool is_scene_in_current_lang(const std::string& scene_name);
};
