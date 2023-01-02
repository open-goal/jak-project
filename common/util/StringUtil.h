#include <string>

namespace str_util {
bool contains(const std::string& s, const std::string& substr);
bool starts_with(const std::string& s, const std::string& prefix);
std::string ltrim(const std::string& s);
std::string rtrim(const std::string& s);
std::string trim(const std::string& s);
int line_count(const std::string& str);
bool valid_regex(const std::string& regex);
}  // namespace str_util
