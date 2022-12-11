#include "StringUtil.h"

#include "diff.h"

namespace str_util {

const std::string WHITESPACE = " \n\r\t\f\v";

bool contains(const std::string& s, const std::string& substr) {
  return s.find(substr) != std::string::npos;
}

bool starts_with(const std::string& s, const std::string& prefix) {
  return s.rfind(prefix) == 0;
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

std::vector<std::string> split(const ::std::string& str, char delimiter) {
  return split_string(str, delimiter);
}
}  // namespace str_util
