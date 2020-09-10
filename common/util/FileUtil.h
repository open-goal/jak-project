#pragma once
#include <string>
#include <vector>

namespace FileUtil {
std::string GetExecutablePath();
std::string get_file_path(const std::vector<std::string>& input);
}  // namespace FileUtil
