#include "subtitle_editor_repl_client.h"

#include <regex>

#include "game/runtime.h"

#include "fmt/core.h"

SubtitleEditorReplClient::SubtitleEditorReplClient() {
  int port = 8181;
  m_repl = std::make_unique<ReplClient>(port);
}

void SubtitleEditorReplClient::set_continue_point(const std::string& continue_point) {
  m_repl->eval(
      fmt::format("(start 'play (get-continue-by-name *game-info* \"{}\"))", continue_point));
}

void SubtitleEditorReplClient::move_jak(const double x, const double y, const double z) {
  m_repl->eval(
      fmt::format("(move-to-point! (-> *target* control) (new 'static 'vector :x (meters {:.3f}) "
                  ":y (meters {:.3f}) :z (meters {:.3f})))",
                  x, y, z));
  m_repl->eval("(send-event *camera* 'teleport)");
}

void SubtitleEditorReplClient::reset_game() {
  m_repl->eval("(set! (-> *game-info* mode) 'debug)");
  m_repl->eval("(initialize! *game-info* 'game (the-as game-save #f) (the-as string #f))");
}

std::string SubtitleEditorReplClient::get_process_string(const std::string& entity_type,
                                                         const std::string& process_name) {
  return fmt::format("(the-as {} (process-by-name \"{}\" *active-pool*))", entity_type,
                     process_name);
}

void SubtitleEditorReplClient::play_hint(const std::string& hint_name) {
  reset_game();
  m_repl->eval(
      fmt::format("(level-hint-spawn (text-id zero) \"{}\" (the-as entity #f) *entity-pool* "
                  "(game-task none))",
                  hint_name));
}

void SubtitleEditorReplClient::execute_jak1_cutscene_code(
    const Jak1SubtitleEditorDB::Entry& entry) {
  // Reset the game first to get to a known state
  reset_game();

  if (entry.move_first) {
    // Set Jak's Continue Point
    if (!entry.continue_name.empty()) {
      set_continue_point(entry.continue_name);
    }
    // Move Jak into position
    if (!entry.move_to.empty()) {
      move_jak(entry.move_to[0], entry.move_to[1], entry.move_to[2]);
    }
  }

  // Run any requirements to setup the task state
  if (!entry.requirements.empty()) {
    // Replace __GET-PROCESS__
    for (const auto& form : entry.requirements) {
      std::string temp = form;
      temp = std::regex_replace(temp, std::regex("__GET-PROCESS__"),
                                get_process_string(entry.entity_type, entry.process_name));
      m_repl->eval(temp);
    }
  }

  if (!entry.move_first) {
    // Set Jak's Continue Point
    if (!entry.continue_name.empty()) {
      set_continue_point(entry.continue_name);
    }
    // Move Jak into position
    if (!entry.move_to.empty()) {
      move_jak(entry.move_to[0], entry.move_to[1], entry.move_to[2]);
    }
  }

  // Execute the critical code - typically this means sending a 'play-anim event to the
  // process-taskable in question
  if (!entry.execute_code.empty()) {
    std::string temp = entry.execute_code;
    temp = std::regex_replace(temp, std::regex("__GET-PROCESS__"),
                              get_process_string(entry.entity_type, entry.process_name));
    m_repl->eval("(send-event *camera* 'teleport)");
    if (entry.delay_frames == 0) {
      m_repl->eval(temp);
    } else {
      // We do this in a separate thread to introduce a delay -- allow the game to catch up before
      // running the critical section
      auto code =
          fmt::format("(process-spawn-function process (lambda () (dotimes (i {}) (suspend)) {}))",
                      entry.delay_frames, temp);
      m_repl->eval(code);
    }
  }
}

void SubtitleEditorReplClient::rebuild_text() {
  if (g_game_version == GameVersion::Jak1) {
    m_repl->eval("(make-text)");
    // increment the language id of the in-memory text file so that it won't match the current
    // language and the game will want to reload it asap
    m_repl->eval("(1+! (-> *subtitle-text* lang))");
  } else {
    // reload subtitles immediately
    m_repl->eval("(reload-subtitles)");
  }
}

void SubtitleEditorReplClient::play_vag(const std::string& scene_name, bool is_cutscene) {
  if (g_game_version != GameVersion::Jak1) {
    if (is_cutscene) {
      m_repl->eval(fmt::format("(scene-find-and-play \"{}\")", scene_name));
    } else {
      m_repl->eval(fmt::format("(vag-player-play-from-name \"{}\")", scene_name));
    }
  }
}
