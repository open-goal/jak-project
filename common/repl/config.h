#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/versions/versions.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

namespace REPL {
struct KeyBind {
  // NOTE - in my experience, meta doesn't work on windows and shift is probably a bad idea when
  // typing text! but I leave it up to the user.
  enum class Modifier { CTRL, SHIFT, META };
  Modifier modifier;
  std::string key;
  std::string description;
  std::string command;

  std::string string() const;
};
void to_json(json& j, const KeyBind& obj);
void from_json(const json& j, KeyBind& obj);

// TODO - per-game config
struct Config {
  GameVersion game_version;
  Config(GameVersion _game_version) : game_version(_game_version){};

  // this is the default REPL configuration
  int nrepl_port = 8181;
  int temp_nrepl_port = -1;
  std::string game_version_folder;
  int target_connect_attempts = 30;
  std::vector<std::string> asm_file_search_dirs = {};
  bool append_keybinds = true;
  std::vector<KeyBind> keybinds = {
      {KeyBind::Modifier::CTRL, "T", "Starts up the game runtime", "(test-play)"},
      {KeyBind::Modifier::CTRL, "Q", "Exit the REPL", "(e)"},
      {KeyBind::Modifier::CTRL, "L", "Listen to an available game process", "(lt)"},
      {KeyBind::Modifier::CTRL, "W",
       "Halt the attached process so you can re-launch a crashed game", "(:stop)"},
      {KeyBind::Modifier::CTRL, "G", "Attach the debugger to the process", "(dbgc)"},
      {KeyBind::Modifier::CTRL, "B", "Displays the most recently caught backtrace", "(:di)"},
      {KeyBind::Modifier::CTRL, "N", "Full build of the game", "(mi)"}};
  bool per_game_history = true;
  bool permissive_redefinitions = false;
  std::string iso_path;

  int get_nrepl_port() {
    if (temp_nrepl_port != -1) {
      return temp_nrepl_port;
    }
    return nrepl_port;
  }
};
void to_json(json& j, const Config& obj);
void from_json(const json& j, Config& obj);

}  // namespace REPL
