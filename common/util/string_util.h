#pragma once

#include <string>
#include <vector>

namespace str_util {
bool contains(const std::string& s, const std::string& substr);
bool starts_with(const std::string& s, const std::string& prefix);
bool ends_with(const std::string& s, const std::string& suffix);
std::string ltrim_newlines(const std::string& s);
std::string ltrim(const std::string& s);
std::string rtrim(const std::string& s);
std::string trim(const std::string& s);
/// Given a string with new-lines, split and trim the leading whitespace from each line
/// then return the string with the new-lines back in place.
std::string trim_newline_indents(const std::string& s);
int line_count(const std::string& str);
bool valid_regex(const std::string& regex);
std::string diff(const std::string& lhs, const std::string& rhs);
/// Default splits on \n characters
std::vector<std::string> split(const ::std::string& str, char delimiter = '\n');
std::string join(const std::vector<std::string>& strs, const std::string& join_with);
std::vector<std::string> regex_get_capture_groups(const std::string& str, const std::string& regex);
bool replace(std::string& str, const std::string& from, const std::string& to);
std::string lower(const std::string& str);
std::string uuid();
std::string repeat(size_t n, const std::string& str);
std::string current_local_timestamp();
std::string current_local_timestamp_no_colons();
std::string current_isotimestamp();
std::string to_upper(const std::string& str);
std::string to_lower(const std::string& str);
/// Is this a valid character for a hex number?
bool hex_char(char c);
}  // namespace str_util
