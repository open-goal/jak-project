#include "util.h"

#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"
#include "common/versions/versions.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"
#include "third-party/replxx/include/replxx.hxx"
// TODO - expand a list of hints (ie. a hint for defun to show at a glance how to write a function,
// or perhaps, show the docstring for the current function being used?)

namespace REPL {
void Wrapper::clear_screen() {
  repl.clear_screen();
}

void Wrapper::print_welcome_message() {
  // TODO - dont print on std-out
  // Welcome message / brief intro for documentation
  std::string ascii;
  ascii += " _____             _____ _____ _____ __    \n";
  ascii += "|     |___ ___ ___|   __|     |  _  |  |   \n";
  ascii += "|  |  | . | -_|   |  |  |  |  |     |  |__ \n";
  ascii += "|_____|  _|___|_|_|_____|_____|__|__|_____|\n";
  ascii += "      |_|                                  \n";
  fmt::print(fmt::emphasis::bold | fg(fmt::color::orange), ascii);

  fmt::print("Welcome to OpenGOAL {}.{}!\n", versions::GOAL_VERSION_MAJOR,
             versions::GOAL_VERSION_MINOR);
  fmt::print("Run {} or {} for help with common commands and REPL usage.\n",
             fmt::styled("(repl-help)", fmt::emphasis::bold | fg(fmt::color::cyan)),
             fmt::styled("(repl-keybinds)", fmt::emphasis::bold | fg(fmt::color::cyan)));
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(lt)");
  fmt::print(" to connect to the local target.\n");
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(mi)");
  fmt::print(" to rebuild the entire game.\n\n");
}

void Wrapper::print_to_repl(const std::string& str) {
  repl.print(str.data());
}

void Wrapper::set_history_max_size(size_t len) {
  repl.set_max_history_size(len);
}

const char* Wrapper::readline(const std::string& prompt) {
  return repl.input(prompt);
}

void Wrapper::add_to_history(const std::string& line) {
  repl.history_add(line);
}

void Wrapper::save_history() {
  fs::path path;
  if (repl_config.per_game_history) {
    path = file_util::get_user_config_dir() / game_version_names[repl_config.game_version] /
           ".opengoal.repl.history";
  } else {
    path = file_util::get_user_config_dir() / ".opengoal.repl.history";
  }
  file_util::create_dir_if_needed_for_file(path.string());
  repl.history_save(path.string());
}

void Wrapper::load_history() {
  fs::path path;
  if (repl_config.per_game_history) {
    path = file_util::get_user_config_dir() / game_version_names[repl_config.game_version] /
           ".opengoal.repl.history";
  } else {
    path = file_util::get_user_config_dir() / ".opengoal.repl.history";
  }
  if (fs::exists(path)) {
    repl.history_load(path.string());
  } else {
    fmt::print("Couldn't locate REPL history file at '{}'\n", path.string());
  }
}

std::pair<std::string, bool> Wrapper::get_current_repl_token(std::string const& context) {
  // Find the current token
  std::string token = "";
  for (auto c = context.crbegin(); c != context.crend(); c++) {
    if (std::isspace(*c)) {
      break;
    } else {
      token = *c + token;
    }
  }

  // If there is a preceeding '(' remove it
  if (!token.empty() && token.at(0) == '(') {
    token.erase(0, 1);
    return {token, true};
  }
  return {token, false};
}

void Wrapper::print_help_message() {
  fmt::print(fmt::emphasis::bold, "\nREPL Controls:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(:clear)\n");
  fmt::print(" - Clear the current screen\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(e)\n");
  fmt::print(" - Exit the compiler\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(lt [ip-address] [port-number])\n");
  fmt::print(
      " - Connect the listener to a running target. The IP address defaults to `127.0.0.1` and the "
      "port to `8112`\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(r [ip-address] [port-number])\n");
  fmt::print(
      " - Attempt to reset the target and reconnect. After this, the target will have nothing "
      "loaded.\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(:status)\n");
  fmt::print(" - Send a ping-like message to the target. Requires the target to be connected\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(shutdown-target)\n");
  fmt::print(" - If the target is connected, make it exit\n");

  fmt::print(fmt::emphasis::bold, "\nCompiling & Building:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(mi)\n");
  fmt::print(" - Build entire game\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(mng)\n");
  fmt::print(" - Build game engine\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(m \"filename\")\n");
  fmt::print(" - Compile an OpenGOAL source file\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(ml \"filename\")\n");
  fmt::print(" - Compile and Load (or reload) an OpenGOAL source file\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(build-kernel)\n");
  fmt::print(" - Build the GOAL kernel\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::lime_green), "(make \"file-name\")\n");
  fmt::print(
      " - Build a file and any out-of-date dependencies. This file must be a target in the make "
      "system.\n");

  fmt::print(fmt::emphasis::bold, "\nOther:\n");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::magenta), "(gs)\n");
  fmt::print(" - Enter a GOOS REPL\n");
}

void Wrapper::print_keybind_help() {
  fmt::print(fmt::emphasis::bold, "\nREPL KeyBinds:\n");
  for (const auto& bind : repl_config.keybinds) {
    fmt::print("{}\n", fmt::styled(bind.string(), fmt::fg(fmt::color::cyan)));
    fmt::print("{}\n", fmt::styled(bind.description, fmt::fg(fmt::color::gray)));
  }
}

replxx::Replxx::key_press_handler_t Wrapper::commit_text_action(std::string text_to_commit) {
  return [this, text_to_commit](char32_t code) {
    repl.set_state(
        replxx::Replxx::State(text_to_commit.c_str(), static_cast<int>(text_to_commit.size())));
    return repl.invoke(replxx::Replxx::ACTION::COMMIT_LINE, code);
  };
}

void Wrapper::init_settings() {
  // NOTE - a nice popular project that uses replxx
  // - https://github.com/ClickHouse/ClickHouse/blob/master/base/base/ReplxxLineReader.cpp#L366
  repl.set_word_break_characters(" \t");
  repl.set_complete_on_empty(false);
  repl.set_indent_multiline(false);
  repl.enable_bracketed_paste();
  // Setup default keybinds
  for (const auto& bind : repl_config.keybinds) {
    char32_t code;
    switch (bind.modifier) {
      case KeyBind::Modifier::CTRL:
        code = replxx::Replxx::KEY::control(bind.key.at(0));
        break;
      case KeyBind::Modifier::SHIFT:
        code = replxx::Replxx::KEY::shift(bind.key.at(0));
        break;
      case KeyBind::Modifier::META:
        code = replxx::Replxx::KEY::meta(bind.key.at(0));
        break;
    }
    repl.bind_key(code, commit_text_action(bind.command));
  }
}

void Wrapper::reload_startup_file() {
  startup_file = load_user_startup_file(username, repl_config.game_version);
}

std::string find_repl_username() {
  // Two options - either:
  // 1. look for the `user.txt` file, which should only contain the username
  // 2. if this is absent AND there is a single folder inside the "user" folder, use that as the
  // username
  auto user_dir = file_util::get_jak_project_dir() / "goal_src" / "user";
  auto dirs = file_util::find_directories_in_dir(user_dir);
  if (dirs.size() == 1) {
    return dirs.at(0).filename().string();
  }

  std::regex allowed_chars("(^[0-9a-zA-Z\\-\\.\\!\\?<>]*$)");
  if (file_util::file_exists((user_dir / "user.txt").string())) {
    auto text = file_util::read_text_file(user_dir / "user.txt");
    text = str_util::trim(text);
    if (!text.empty() && std::regex_match(text, allowed_chars)) {
      return text;
    }
  }

  return "#f";
}

fs::path get_startup_file_path(const std::string& username, const GameVersion game_version) {
  // - first check to see if there is a game version specific startup file to prefer
  auto game_specific_path = file_util::get_jak_project_dir() / "goal_src" / "user" / username /
                            fmt::format("startup-{}.gc", version_to_game_name(game_version));
  if (file_util::file_exists(game_specific_path.string())) {
    return game_specific_path;
  }
  return file_util::get_jak_project_dir() / "goal_src" / "user" / username / "startup.gc";
}

StartupFile load_user_startup_file(const std::string& username, const GameVersion game_version) {
  // Check for a `startup.gc` file, each line will be executed on the REPL on startup
  auto startup_file_path = get_startup_file_path(username, game_version);
  StartupFile startup_file;
  if (file_util::file_exists(startup_file_path.string())) {
    auto data = file_util::read_text_file(startup_file_path);
    auto startup_cmds = str_util::split(data);
    bool found_run_on_listen_line = false;
    for (const auto& cmd : startup_cmds) {
      if (found_run_on_listen_line) {
        startup_file.run_after_listen.push_back(cmd);
      } else {
        startup_file.run_before_listen.push_back(cmd);
      }
      if (str_util::contains(cmd, "og:run-below-on-listen")) {
        found_run_on_listen_line = true;
      }
    }
  }
  return startup_file;
}

REPL::Config load_repl_config(const std::string& username, const GameVersion game_version) {
  auto repl_config_path =
      file_util::get_jak_project_dir() / "goal_src" / "user" / username / "repl-config.json";
  if (file_util::file_exists(repl_config_path.string())) {
    try {
      REPL::Config config(game_version);
      auto repl_config_data =
          parse_commented_json(file_util::read_text_file(repl_config_path), "repl-config.json");
      from_json(repl_config_data, config);
      return config;
    } catch (std::exception& e) {
      REPL::Config config(game_version);
    }
  }
  return REPL::Config(game_version);
}
}  // namespace REPL
