#include <string>
#include <vector>

namespace str_util {
bool contains(const std::string& s, const std::string& substr);
bool starts_with(const std::string& s, const std::string& prefix);
std::string ltrim(const std::string& s);
std::string rtrim(const std::string& s);
std::string trim(const std::string& s);
std::vector<std::string> split(const ::std::string& str, char delimiter = '\n');
}  // namespace str_util
