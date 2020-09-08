#ifndef JAK1_FILE_IO_H
#define JAK1_FILE_IO_H

#include <string>
#include <vector>

namespace util {
std::string read_text_file(const std::string& path);
std::string combine_path(const std::string& parent, const std::string& child);
std::string combine_path(std::vector<std::string> path);
}  // namespace util

#endif  // JAK1_FILE_IO_H
