#pragma once

#include <optional>
#include <string_view>

#include "common/nrepl/ReplClient.h"
#include "common/serialization/subtitles/subtitles.h"

#include "third-party/imgui/imgui.h"

class SubtitleEditorDB {
 public:
  struct Entry {
    std::string entity_type;
    std::string process_name;
    std::string continue_name;
    std::vector<double> move_to;
    std::string execute_code;
    bool move_first;
    std::vector<std::string> requirements;
  };
};

// TODO Later:
// - Hints, these seem less annoying but there are a lot of them

class SubtitleEditor {
 public:
  SubtitleEditor();
  void draw_window();

 private:
  void draw_edit_options();
  void draw_repl_options();

  void draw_all_cutscene_groups();
  void draw_all_scenes(std::string group_name, bool base_cutscenes = false);
  void draw_subtitle_options(GameSubtitleSceneInfo& scene, bool current_scene = false);
  void draw_new_cutscene_line_form();
  void draw_all_hint_groups();
  void draw_all_hints(std::string group_name, bool base_cutscenes);

  GameSubtitleDB m_subtitle_db;
  std::map<std::string, SubtitleEditorDB::Entry> m_db = {};
  GameSubtitleSceneInfo* m_current_scene = nullptr;
  std::string m_filter;
  std::string m_filter_hints;

  ReplClient m_repl;

  int m_current_scene_frame = 0;
  std::string m_current_scene_text = "";
  std::string m_current_scene_speaker = "";
  bool m_current_scene_offscreen = false;

  std::string m_new_scene_name = "";
  std::string m_new_scene_group = "";
  std::string m_new_scene_id = "0";

  std::string m_new_scene_group_name = "";

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

  void repl_set_continue_point(const std::string_view& continue_point);
  void repl_move_jak(const double x, const double y, const double z);
  void repl_reset_game();
  std::string repl_get_process_string(const std::string_view& entity_type,
                                      const std::string_view& process_name);
  void repl_execute_cutscene_code(const SubtitleEditorDB::Entry& entry);
  void repl_rebuild_text();
  void repl_play_hint(const std::string_view& hint_name);

  bool is_scene_in_current_lang(const std::string& scene_name);
};
