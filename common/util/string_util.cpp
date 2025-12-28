#include "string_util.h"

#include <iomanip>
#include <random>
#include <regex>
#include <sstream>

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

// Left-trims any leading whitespace up to and including the final leading newline
// For example:
// " \n\n  hello world" => "  hello world"
std::string ltrim_newlines(const std::string& s) {
  size_t start = s.find_first_not_of(WHITESPACE);
  // Seek backwards until we hit the beginning of the string, or a newline -- this is the actual
  // substr point we want to use
  for (int i = start - 1; i >= 0; i--) {
    const auto& c = s.at(i);
    if (c == '\n') {
      break;
    }
    start--;
  }
  return (start == std::string::npos) ? "" : s.substr(start);
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

std::vector<std::string> split_string(const std::string& str, const std::string& delimiter) {
  std::vector<std::string> parsed;
  std::string::size_type pos = 0;
  while (true) {
    const std::string::size_type found = str.find(delimiter, pos);
    if (found == std::string::npos) {
      parsed.push_back(str.substr(pos));
      break;
    } else {
      parsed.push_back(str.substr(pos, found - pos));
      pos = found + delimiter.length();
    }
  }
  return parsed;
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

std::string lower(const std::string& str) {
  std::string res;
  for (auto c : str) {
    res.push_back(tolower(c));
  }
  return res;
}

std::string uuid() {
  static std::random_device dev;
  static std::mt19937 rng(dev());

  std::uniform_int_distribution<int> dist(0, 15);

  const char* v = "0123456789abcdef";
  const bool dash[] = {0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0};

  std::string res;
  for (int i = 0; i < 16; i++) {
    if (dash[i])
      res += "-";
    res += v[dist(rng)];
    res += v[dist(rng)];
  }
  return res;
}

std::string repeat(size_t n, const std::string& str) {
  if (n == 0 || str.empty())
    return {};
  if (n == 1)
    return str;
  const auto period = str.size();
  if (period == 1)
    return std::string(n, str.front());

  std::string ret(str);
  ret.reserve(period * n);
  std::size_t m{2};
  for (; m < n; m *= 2)
    ret += ret;
  ret.append(ret.c_str(), (n - (m / 2)) * period);
  return ret;
}

std::string current_local_timestamp() {
  std::time_t now = std::time(nullptr);
  std::tm local_time = *std::localtime(&now);
  const std::string format = "%Y-%m-%dT%H:%M:%S";
  std::ostringstream oss;
  oss << std::put_time(&local_time, format.c_str());
  return oss.str();
}

std::string current_local_timestamp_no_colons() {
  std::time_t now = std::time(nullptr);
  std::tm local_time = *std::localtime(&now);
  const std::string format = "%Y-%m-%dT%H-%M-%S";
  std::ostringstream oss;
  oss << std::put_time(&local_time, format.c_str());
  return oss.str();
}

std::string current_isotimestamp() {
  std::time_t now = std::time(nullptr);
  std::tm utc_time = *std::gmtime(&now);
  const std::string format = "%Y-%m-%dT%H:%M:%SZ";
  std::ostringstream oss;
  oss << std::put_time(&utc_time, format.c_str());
  return oss.str();
}

std::string to_upper(const std::string& str) {
  std::string new_str(str.size(), ' ');
  std::transform(str.begin(), str.end(), new_str.begin(), ::toupper);
  return new_str;
}

std::string to_lower(const std::string& str) {
  std::string new_str(str.size(), ' ');
  std::transform(str.begin(), str.end(), new_str.begin(), ::tolower);
  return new_str;
}

bool hex_char(char c) {
  return !((c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F'));
}

std::string titlize(const std::string& str) {
  // Iterate through the string, capitalizing any character that either comes first, or is preceeded
  // by whitespace
  const auto trimmed_string = trim(str);
  std::string new_str = "";
  bool capitalize_next_char = true;
  for (const auto& character : trimmed_string) {
    if (capitalize_next_char) {
      new_str.push_back(toupper(character));
      capitalize_next_char = false;
    } else {
      if (character == ' ') {
        capitalize_next_char = true;
      }
      new_str.push_back(character);
    }
  }
  return new_str;
}

std::string pad_right(const std::string& input, const int width, const char padding_char) {
  if ((int)input.length() >= width) {
    return input;  // No need to pad if input length is already greater or equal to width
  } else {
    int padding_width = width - input.length();
    return input + std::string(padding_width, padding_char);
  }
}

char32_t next_utf8_char(const std::string& s, size_t& i) {
  char32_t cp = 0;
  unsigned char c = s[i];
  if (c < 0x80) {  // 1-byte ASCII
    cp = c;
    ++i;
  } else if ((c >> 5) == 0x6) {  // 2-byte
    cp = ((c & 0x1F) << 6) | (s[i + 1] & 0x3F);
    i += 2;
  } else if ((c >> 4) == 0xE) {  // 3-byte
    cp = ((c & 0x0F) << 12) | ((s[i + 1] & 0x3F) << 6) | (s[i + 2] & 0x3F);
    i += 3;
  } else if ((c >> 3) == 0x1E) {  // 4-byte
    cp = ((c & 0x07) << 18) | ((s[i + 1] & 0x3F) << 12) | ((s[i + 2] & 0x3F) << 6) |
         (s[i + 3] & 0x3F);
    i += 4;
  } else {
    // invalid
    ++i;
  }
  return cp;
}

std::string utf8_encode(char32_t cp) {
  std::string out;
  if (cp <= 0x7F) {
    out += static_cast<char>(cp);
  } else if (cp <= 0x7FF) {
    out += static_cast<char>(0xC0 | ((cp >> 6) & 0x1F));
    out += static_cast<char>(0x80 | (cp & 0x3F));
  } else if (cp <= 0xFFFF) {
    out += static_cast<char>(0xE0 | ((cp >> 12) & 0x0F));
    out += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    out += static_cast<char>(0x80 | (cp & 0x3F));
  } else if (cp <= 0x10FFFF) {
    out += static_cast<char>(0xF0 | ((cp >> 18) & 0x07));
    out += static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
    out += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
    out += static_cast<char>(0x80 | (cp & 0x3F));
  }
  return out;
}
}  // namespace str_util
