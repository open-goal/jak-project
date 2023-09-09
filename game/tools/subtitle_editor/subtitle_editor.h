#pragma once

#include <optional>

#include "subtitle_editor_db.h"
#include "subtitle_editor_repl_client.h"

#include "common/serialization/subtitles/subtitles_v2.h"

#include "third-party/imgui/imgui.h"

class SubtitleEditor {
 public:
  SubtitleEditor();
  void draw_window();

 private:
  bool is_v1_format();

  void draw_edit_options();
  void draw_repl_options();

  void draw_speaker_options();

  void draw_scene_section_header(const bool non_cutscenes);
  void draw_scene_node(const bool base_cutscenes,
                       const std::string& scene_name,
                       GameSubtitleSceneInfo& scene_info,
                       std::unordered_set<std::string>& scenes_to_delete);
  void draw_all_cutscenes(bool base_cutscenes = false);
  void draw_all_non_cutscenes(bool base_cutscenes = false);
  std::string subtitle_line_summary(const SubtitleLine& line,
                                    const SubtitleLineMetadata& line_meta,
                                    const std::shared_ptr<GameSubtitleBank> bank);
  void draw_subtitle_options(GameSubtitleSceneInfo& scene, bool current_scene = false);
  void draw_new_scene_line_form();

  bool m_db_loaded = false;
  bool m_db_failed_to_load = false;
  GameSubtitleDB m_subtitle_db;
  SubtitleEditorReplClient m_repl;
  GameSubtitleDB::SubtitleFormat m_subtitle_version;

  // Jak 1 Specifics
  Jak1SubtitleEditorDB m_jak1_editor_db;

  // GUI Controls
  int m_current_language = 0;
  bool m_truncate_summaries = false;
  std::string m_filter_cutscenes;
  std::string m_filter_non_cutscenes;
  GameSubtitleSceneInfo* m_current_scene = nullptr;

  // GUI Styling
  ImVec4 m_normal_text_color = ImVec4(1.0f, 0.0f, 1.0f, 1.0f);
  int m_selected_text_color = IM_COL32(89, 227, 225, 255);
  ImVec4 m_success_text_color = ImVec4(0.0f, 1.0f, 0.0f, 1.0f);
  ImVec4 m_error_text_color = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
  ImVec4 m_disabled_text_color = ImVec4(1.0f, 1.0f, 1.0f, 0.7f);
  ImVec4 m_warning_color = ImVec4(0.619f, 0.443f, 0.0f, 1.0f);
  int m_offscreen_text_color = IM_COL32(240, 242, 102, 255);

  // State
  int m_current_scene_frames[2] = {0, 0};
  std::string m_current_scene_speaker = "";
  std::string m_current_scene_text = "";
  bool m_current_scene_offscreen = false;
  bool m_current_scene_merge = false;

  bool m_new_scene_as_current = true;
  std::string m_new_scene_name = "";
  std::string m_new_scene_id = "0";

  std::string m_filter_placeholder = "Filter List...";
  std::optional<bool> m_files_saved_successfully = {};

  bool is_scene_in_current_lang(const std::string& scene_name);
};
