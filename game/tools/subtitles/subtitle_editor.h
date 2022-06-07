#pragma once

#include <common/serialization/subtitles/subtitles.h>
#include <optional>
#include <common/nrepl/ReplClient.h>
#include <third-party/imgui/imgui.h>

class SubtitleEditorDB {
 public:
  struct Entry {
    std::string entity_type;
    std::string process_name;
    std::string continue_name;
    std::vector<double> move_to;
    std::string execute_code;
    std::vector<std::string> requirements;
  };
};

// TODO List:
// - validation on inputs
// - add metadata for groups (can use groups in my output!)
// - create a deserializer to update the file
// - dropdown to change sorting group

// TODO Later:
// - Hints, these seem less annoying but there are a lot of them

class SubtitleEditor {
 public:
  SubtitleEditor();
  void draw_window();

 private:
  void draw_edit_options();
  void draw_repl_options();

  void draw_all_cutscenes(bool base_cutscenes = false);
  void draw_current_cutscene();

  GameSubtitleDB m_subtitle_db;
  std::map<std::string, SubtitleEditorDB::Entry> m_db = {};
  std::optional<GameSubtitleSceneInfo> m_current_scene = {};
  std::string m_filter;
  std::string m_filter_hints;

  ReplClient m_repl;
  bool m_repl_connected;

  int m_current_scene_frame = 0;
  std::string m_current_scene_text = "";
  std::string m_current_scene_speaker = "";
  bool m_current_scene_offscreen = false;

  std::string m_new_scene_name = "";
  std::string m_new_scene_group = "";

  std::string m_filter_placeholder = "Filter List...";

  int m_base_language = 0;
  // NOTE - English is mapped to 0,6 but I don't know how to handle this UX condition where
  // basically everything is the same, for now that is a special case
  int m_current_language = 3;
  // bool m_base_show_lines = false;
  bool m_base_show_missing_cutscenes = true;

  // TODO - let the user customize these colors
  int m_normal_text_color = IM_COL32(255, 255, 255, 255);
  int m_selected_text_color = IM_COL32(89, 227, 225, 255);
  int m_success_text_color = IM_COL32(0, 255, 0, 255);
  int m_error_text_color = IM_COL32(255, 0, 0, 255);
  int m_disabled_text_color = IM_COL32(255, 255, 255, 175);
  int m_offscreen_text_color = IM_COL32(240, 242, 102, 255);
  // TODO - cycle speaker colors

  void repl_set_continue_point(const std::string_view& continue_point);
  void repl_move_jak(const double x, const double y, const double z);
  void repl_reset_game();
  std::string repl_get_process_string(const std::string_view& entity_type,
                                      const std::string_view& process_name);
  void repl_execute_cutscene_code(const SubtitleEditorDB::Entry& entry);
  void repl_rebuild_text();

  bool is_scene_in_current_lang(const std::string& scene_name);
};
