#pragma once

#include <functional>
#include <string>
#include <vector>

#include "third-party/replxx/include/replxx.hxx"

using Replxx = replxx::Replxx;

class ReplWrapper {
  Replxx repl;

 public:
  ReplWrapper() {}
  Replxx& get_repl() { return repl; }
  void init_default_settings();

  // Functionality / Commands
  void clear_screen();
  void print_to_repl(const std::string_view& str);
  void print_welcome_message();
  void set_history_max_size(size_t len);
  const char* readline(const std::string& prompt);
  void add_to_history(const std::string& line);
  void save_history();
  void load_history();
  void print_help_message();
  std::pair<std::string, bool> get_current_repl_token(std::string const& context);

  std::vector<std::string> examples{};
  using cl = Replxx::Color;
  std::vector<std::pair<std::string, cl>> regex_colors{};

 private:
  replxx::Replxx::key_press_handler_t commit_text_action(std::string text_to_commit);
};
