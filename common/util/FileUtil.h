#pragma once

/*!
 * @file FileUtil.h
 * Utility functions for reading and writing files.
 */

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "common/common_types.h"

namespace fs = std::filesystem;

namespace file_util {
std::filesystem::path get_user_home_dir();
std::filesystem::path get_user_config_dir();
std::filesystem::path get_user_settings_dir();
std::filesystem::path get_user_memcard_dir();
std::filesystem::path get_jak_project_dir();

bool create_dir_if_needed(const std::filesystem::path& path);
bool create_dir_if_needed_for_file(const std::string& path);
bool setup_project_path(std::optional<std::filesystem::path> project_path_override);
std::string get_file_path(const std::vector<std::string>& path);
void write_binary_file(const std::string& name, const void* data, size_t size);
void write_binary_file(const std::filesystem::path& name, const void* data, size_t size);
void write_rgba_png(const std::filesystem::path& name, void* data, int w, int h);
void write_text_file(const std::string& file_name, const std::string& text);
void write_text_file(const std::filesystem::path& file_name, const std::string& text);
std::vector<uint8_t> read_binary_file(const std::string& filename);
std::vector<uint8_t> read_binary_file(const std::filesystem::path& filename);
std::string read_text_file(const std::string& path);
std::string read_text_file(const std::filesystem::path& path);
bool is_printable_char(char c);
std::string combine_path(const std::string& parent, const std::string& child);
bool file_exists(const std::string& path);
std::string base_name(const std::string& filename);
void MakeISOName(char* dst, const char* src);
void ISONameFromAnimationName(char* dst, const char* src);
void assert_file_exists(const char* path, const char* error_message);
bool dgo_header_is_compressed(const std::vector<u8>& data);
std::vector<u8> decompress_dgo(const std::vector<u8>& data_in);
}  // namespace file_util
