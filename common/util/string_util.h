#include <string>

namespace str_util {
bool starts_with(const std::string& s, const std::string& prefix);
std::string ltrim(const std::string& s);
std::string rtrim(const std::string& s);
std::string trim(const std::string& s);
int line_count(const std::string& str);
}  // namespace str_util
