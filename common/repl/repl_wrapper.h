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
 public:
  std::string username;
  Config repl_config;
  StartupFile startup_file;
  bool nrepl_alive = false;
  std::vector<std::string> examples{};
  std::vector<std::pair<std::string, replxx::Replxx::Color>> regex_colors{};

  Wrapper(GameVersion version) : repl_config(version) {}
  Wrapper(const std::string& _username,
          const Config& config,
          const StartupFile& startup,
          bool _nrepl_alive)
      : username(_username),
        repl_config(config),
        startup_file(startup),
        nrepl_alive(_nrepl_alive) {}
  replxx::Replxx& get_repl() { return repl; }
  void init_settings();
  void reload_startup_file();

  // Functionality / Commands
  void clear_screen();
  void print_to_repl(const std::string& str);
  void print_welcome_message(const std::vector<std::string>& loaded_projects);
  void set_history_max_size(size_t len);
  const char* readline(const std::string& prompt);
  void add_to_history(const std::string& line);
  void save_history();
  void load_history();
  void print_help_message();
  void print_keybind_help();
  std::pair<std::string, bool> get_current_repl_token(std::string const& context);

 private:
  replxx::Replxx repl;
  replxx::Replxx::key_press_handler_t commit_text_action(std::string text_to_commit);
  std::vector<REPL::KeyBind> keybindings = {};
};

#if defined(__SWITCH__)
// repl_wrapper.cpp (the real implementation) isn't built for Switch at all -- no interactive
// REPL there -- but this one free function is also called from jak2/jak3/jakx kboot.cpp to name
// a per-user dev startup-script folder, unrelated to the REPL itself. There's no such concept of
// per-user dev folders on a shipped Switch build, so this just returns the same "unknown"
// fallback the real implementation uses when it can't find a real username either.
inline std::string find_repl_username() {
  return "unknown";
}
#else
std::string find_repl_username();
#endif
StartupFile load_user_startup_file(const std::string& username, const GameVersion game_version);
REPL::Config load_repl_config(const std::string& username,
                              const GameVersion game_version,
                              const int nrepl_port);
}  // namespace REPL
