#pragma once

/*!
 * @file FileUtil.h
 * Utility functions for reading and writing files.
 */

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#endif

#include "third-party/filesystem.hpp"

#ifdef _WIN32
#undef FALSE
#endif

#include <optional>
#include <regex>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/versions.h"

namespace fs = ghc::filesystem;

namespace file_util {
fs::path get_user_home_dir();
fs::path get_user_config_dir();
fs::path get_user_settings_dir(GameVersion game_version);
fs::path get_user_memcard_dir(GameVersion game_version);
fs::path get_user_misc_dir(GameVersion game_version);
fs::path get_jak_project_dir();

bool create_dir_if_needed(const fs::path& path);
bool create_dir_if_needed_for_file(const std::string& path);
bool create_dir_if_needed_for_file(const fs::path& path);
bool setup_project_path(std::optional<fs::path> project_path_override);
std::string get_file_path(const std::vector<std::string>& path);
void write_binary_file(const std::string& name, const void* data, size_t size);
void write_binary_file(const fs::path& name, const void* data, size_t size);
void write_rgba_png(const fs::path& name, void* data, int w, int h);
void write_text_file(const std::string& file_name, const std::string& text);
void write_text_file(const fs::path& file_name, const std::string& text);
std::vector<uint8_t> read_binary_file(const std::string& filename);
std::vector<uint8_t> read_binary_file(const fs::path& filename);
std::string read_text_file(const std::string& path);
std::string read_text_file(const fs::path& path);
bool is_printable_char(char c);
std::string combine_path(const std::string& parent, const std::string& child);
bool file_exists(const std::string& path);
std::string base_name(const std::string& filename);
std::string base_name_no_ext(const std::string& filename);
std::string split_path_at(const fs::path& path, const std::vector<std::string>& folders);
std::string convert_to_unix_path_separators(const std::string& path);
void MakeISOName(char* dst, const char* src);
void ISONameFromAnimationName(char* dst, const char* src);
void assert_file_exists(const char* path, const char* error_message);
bool dgo_header_is_compressed(const std::vector<u8>& data);
std::vector<u8> decompress_dgo(const std::vector<u8>& data_in);
FILE* open_file(const fs::path& path, const std::string& mode);
std::vector<fs::path> find_files_recursively(const fs::path& base_dir, const std::regex& pattern);
std::vector<fs::path> find_directories_in_dir(const fs::path& base_dir);
/// Will overwrite the destination if it exists
void copy_file(const fs::path& src, const fs::path& dst);
}  // namespace file_util
