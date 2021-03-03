#include "ReplHistory.h"

#include "common/util/FileUtil.h"
#include "third-party/linenoise.h"

bool ReplHistory::repl_set_history_max_size(size_t len) {
  return linenoise::SetHistoryMaxLen(len);
}

bool ReplHistory::repl_readline(const char* prompt, std::string& line) {
  return linenoise::Readline(prompt, line);
}

bool ReplHistory::repl_add_to_history(const char* line) {
  return linenoise::AddHistory(line);
}

bool ReplHistory::repl_save_history() {
  // NOTE - library doesn't seem unicode safe on windows
  std::filesystem::path path = file_util::get_user_home_dir();
  if (std::filesystem::exists(path)) {
    return linenoise::SaveHistory((path / ".opengoal.repl.history").string().c_str());
  }
  return false;
}

bool ReplHistory::repl_load_history() {
  // NOTE - library doesn't seem unicode safe on windows
  std::filesystem::path path = file_util::get_user_home_dir();
  if (std::filesystem::exists(path)) {
    return linenoise::LoadHistory((path / ".opengoal.repl.history").string().c_str());
  }
  return false;
}

const std::vector<std::string>& ReplHistory::repl_get_history() {
  return linenoise::GetHistory();
}
