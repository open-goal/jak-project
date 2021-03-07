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

  // Functionality / Commands
  void clear_screen();
  void set_history_max_size(size_t len);
  const char* readline(const std::string& prompt);
  void add_to_history(const std::string& line);
  void save_history();
  void load_history();
  void print_help_message();
};
