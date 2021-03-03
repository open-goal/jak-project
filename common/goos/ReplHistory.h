#pragma once

#include <string>
#include <vector>

// Linenoise declares history as a static array, so when trying to use the
// library across files, the history array is different
// Solve this by wrapping the API
namespace ReplHistory {
bool repl_set_history_max_size(size_t len);
bool repl_readline(const char* prompt, std::string& line);
bool repl_add_to_history(const char* line);
bool repl_save_history();
bool repl_load_history();
const std::vector<std::string>& repl_get_history();
};  // namespace ReplHistory
