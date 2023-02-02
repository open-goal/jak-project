#pragma once

#include <functional>
#include <optional>
#include <string>
#include <vector>

#include "config.h"

#include "third-party/replxx/include/replxx.hxx"

namespace REPL {

struct StartupFile {
  std::vector<std::string> run_before_listen = {};
  std::vector<std::string> run_after_listen = {};
};

class Wrapper {
  replxx::Replxx repl;

 public:
  std::string username;
  Config repl_config;
  StartupFile startup_file;
  std::vector<std::string> examples{};
  std::vector<std::pair<std::string, replxx::Replxx::Color>> regex_colors{};

  Wrapper(GameVersion version) : repl_config(version) {}
  Wrapper(const std::string& _username, const Config& config, const StartupFile& startup)
      : username(_username), repl_config(config), startup_file(startup) {}
  replxx::Replxx& get_repl() { return repl; }
  void init_settings();
  void reload_startup_file();

  // Functionality / Commands
  void clear_screen();
  void print_to_repl(const std::string& str);
  void print_welcome_message();
  void set_history_max_size(size_t len);
  const char* readline(const std::string& prompt);
  void add_to_history(const std::string& line);
  void save_history();
  void load_history();
  void print_help_message();
  void print_keybind_help();
  std::pair<std::string, bool> get_current_repl_token(std::string const& context);

 private:
  replxx::Replxx::key_press_handler_t commit_text_action(std::string text_to_commit);
  std::vector<REPL::KeyBind> keybindings = {};
};

std::string find_repl_username();
StartupFile load_user_startup_file(const std::string& username, const GameVersion game_version);
REPL::Config load_repl_config(const std::string& username, const GameVersion game_version);
}  // namespace REPL
