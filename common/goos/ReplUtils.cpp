#include "ReplUtils.h"

#include "common/util/FileUtil.h"
#include "common/versions.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"
#include "third-party/replxx/include/replxx.hxx"

// TODO - expand a list of hints (ie. a hint for defun to show at a glance how to write a function,
// or perhaps, show the docstring for the current function being used?)

using Replxx = replxx::Replxx;

void ReplWrapper::clear_screen() {
  repl.clear_screen();
}

void ReplWrapper::print_welcome_message() {
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
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(repl-help)");
  fmt::print(" for help with common commands and REPL usage.\n");
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(lt)");
  fmt::print(" to connect to the local target.\n");
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(mi)");
  fmt::print(" to rebuild the entire game.\n\n");
}

void ReplWrapper::print_to_repl(const std::string_view& str) {
  repl.print(str.data());
}

void ReplWrapper::set_history_max_size(size_t len) {
  repl.set_max_history_size(len);
}

const char* ReplWrapper::readline(const std::string& prompt) {
  return repl.input(prompt);
}

void ReplWrapper::add_to_history(const std::string& line) {
  repl.history_add(line);
}

void ReplWrapper::save_history() {
  fs::path path = file_util::get_user_config_dir() / ".opengoal.repl.history";
  file_util::create_dir_if_needed_for_file(path.string());
  repl.history_save(path.string());
}

void ReplWrapper::load_history() {
  fs::path path = file_util::get_user_config_dir() / ".opengoal.repl.history";
  if (fs::exists(path)) {
    repl.history_load(path.string());
  } else {
    fmt::print("Couldn't locate REPL history file at '{}'\n", path.string());
  }
}

std::pair<std::string, bool> ReplWrapper::get_current_repl_token(std::string const& context) {
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

void ReplWrapper::print_help_message() {
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

replxx::Replxx::key_press_handler_t ReplWrapper::commit_text_action(std::string text_to_commit) {
  return [this, text_to_commit](char32_t code) {
    repl.set_state(
        replxx::Replxx::State(text_to_commit.c_str(), static_cast<int>(text_to_commit.size())));
    return repl.invoke(Replxx::ACTION::COMMIT_LINE, code);
  };
}

void ReplWrapper::init_default_settings() {
  // NOTE - a nice popular project that uses replxx
  // - https://github.com/ClickHouse/ClickHouse/blob/master/base/base/ReplxxLineReader.cpp#L366
  repl.set_word_break_characters(" \t");
  // Setup default keybinds
  // (test-play) : Ctrl-T
  repl.bind_key(Replxx::KEY::control('T'), commit_text_action("(test-play)"));
  // (e) : Ctrl-Q
  repl.bind_key(Replxx::KEY::control('Q'), commit_text_action("(e)"));
  // (lt) : Ctrl-L
  repl.bind_key(Replxx::KEY::control('L'), commit_text_action("(lt)"));
  /// (:stop) : Ctrl-W
  repl.bind_key(Replxx::KEY::control('W'), commit_text_action("(:stop)"));
  // (dbgc) : Ctrl-G
  repl.bind_key(Replxx::KEY::control('G'), commit_text_action("(dbgc)"));
  // (:di) : Ctrl-B
  repl.bind_key(Replxx::KEY::control('B'), commit_text_action("(:di)"));
  // (mi) : Ctrl-N
  repl.bind_key(Replxx::KEY::control('N'), commit_text_action("(mi)"));
}
