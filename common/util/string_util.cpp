#include "string_util.h"

#include <regex>

#include "common/util/diff.h"

namespace str_util {

const std::string WHITESPACE = " \n\r\t\f\v";

bool contains(const std::string& s, const std::string& substr) {
  return s.find(substr) != std::string::npos;
}

bool starts_with(const std::string& s, const std::string& prefix) {
  return s.size() >= prefix.size() && 0 == s.compare(0, prefix.size(), prefix);
}

bool ends_with(const std::string& s, const std::string& suffix) {
  return s.size() >= suffix.size() &&
         0 == s.compare(s.size() - suffix.size(), suffix.size(), suffix);
}

std::string ltrim(const std::string& s) {
  size_t start = s.find_first_not_of(WHITESPACE);
  return (start == std::string::npos) ? "" : s.substr(start);
}

std::string rtrim(const std::string& s) {
  size_t end = s.find_last_not_of(WHITESPACE);
  return (end == std::string::npos) ? "" : s.substr(0, end + 1);
}

std::string trim(const std::string& s) {
  return rtrim(ltrim(s));
}

std::string trim_newline_indents(const std::string& s) {
  auto lines = split(s, '\n');
  std::vector<std::string> trimmed_lines;
  std::transform(lines.begin(), lines.end(), std::back_inserter(trimmed_lines),
                 [](const std::string& line) { return ltrim(line); });
  return join(trimmed_lines, "\n");
}

std::string join(const std::vector<std::string>& strs, const std::string& join_with) {
  std::string out;
  for (size_t i = 0; i < strs.size(); i++) {
    out += strs.at(i);
    if (i < strs.size() - 1) {
      out += join_with;
    }
  }
  return out;
}

int line_count(const std::string& str) {
  int result = 0;
  for (auto& c : str) {
    if (c == '\n') {
      result++;
    }
  }
  return result;
}

// NOTE - this won't work running within gk.exe!
bool valid_regex(const std::string& regex) {
  try {
    std::regex re(regex);
  } catch (const std::regex_error& e) {
    return false;
  }
  return true;
}

std::string diff(const std::string& lhs, const std::string& rhs) {
  return google_diff::diff_strings(lhs, rhs);
}
/// Default splits on \n characters
std::vector<std::string> split(const ::std::string& str, char delimiter) {
  return google_diff::split_string(str, delimiter);
}

std::vector<std::string> regex_get_capture_groups(const std::string& str,
                                                  const std::string& regex) {
  std::vector<std::string> groups;
  std::smatch matches;
  if (std::regex_search(str, matches, std::regex(regex))) {
    for (size_t i = 1; i < matches.size(); i++) {
      groups.push_back(matches[i].str());
    }
  }
  return groups;
}

bool replace(std::string& str, const std::string& from, const std::string& to) {
  size_t start_pos = str.find(from);
  if (start_pos == std::string::npos)
    return false;
  str.replace(start_pos, from.length(), to);
  return true;
}
}  // namespace str_util
