#pragma once
#include <string>
#include <vector>

namespace file_util {
std::string get_project_path();
std::string get_file_path(const std::vector<std::string>& input);
void write_binary_file(const std::string& name, void* data, size_t size);
void write_text_file(const std::string& file_name, const std::string& text);
std::vector<uint8_t> read_binary_file(const std::string& filename);
std::string read_text_file(const std::string& path);
bool is_printable_char(char c);
}  // namespace file_util
