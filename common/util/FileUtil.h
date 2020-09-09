#pragma once
#include <string>
#include <vector>

namespace FileUtil {
std::string GetCurrentWorkingDir();
std::string get_file_path(std::vector<std::string> input);
}  // namespace FileUtil
