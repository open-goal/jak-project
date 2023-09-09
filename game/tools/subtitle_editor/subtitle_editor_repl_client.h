#pragma once
#include <string>

#include "subtitle_editor_db.h"

#include "common/repl/nrepl/ReplClient.h"
#include "common/versions/versions.h"

class SubtitleEditorReplClient {
 public:
  SubtitleEditorReplClient();
  std::unique_ptr<ReplClient> m_repl;

  void connect() { m_repl->connect(); }
  bool is_connected() { return m_repl->is_connected(); }

  void set_continue_point(const std::string& continue_point);
  void move_jak(const double x, const double y, const double z);
  void reset_game();
  std::string get_process_string(const std::string& entity_type, const std::string& process_name);
  void execute_jak1_cutscene_code(const Jak1SubtitleEditorDB::Entry& entry);
  void rebuild_text();
  void play_hint(const std::string& hint_name);
  void play_vag(const std::string& scene_name, bool is_cutscene);
};
