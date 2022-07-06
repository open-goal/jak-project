/*!
 * @file FileUtil.cpp
 * Utility functions for reading and writing files.
 */

#include "FileUtil.h"

#include <cstdio> /* defines FILENAME_MAX */
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

#include "BinaryWriter.h"

#include "common/common_types.h"
#include "common/util/BinaryReader.h"

// This disables the use of PCLMULQDQ which is probably ok, but let's just be safe and disable it
// because nobody will care if png compression is 10% slower.
#define FPNG_NO_SSE 1
#include "third-party/fmt/core.h"
#include "third-party/fpng/fpng.cpp"
#include "third-party/fpng/fpng.h"
#include "third-party/lzokay/lzokay.hpp"

#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <cstring>
#include <unistd.h>
#endif
#include "common/util/Assert.h"
#include <common/log/log.h>

namespace file_util {
fs::path get_user_home_dir() {
#ifdef _WIN32
  // NOTE - on older systems, this may case issues if it cannot be found!
  std::string home_dir = std::getenv("USERPROFILE");
  return fs::path(home_dir);
#else
  std::string home_dir = std::getenv("HOME");
  return fs::path(home_dir);
#endif
}

fs::path get_user_config_dir() {
  fs::path config_base_path;
#ifdef _WIN32
  auto config_base_dir = std::getenv("APPDATA");
  config_base_path = fs::path(std::string(config_base_dir));
#elif __linux
  // Docs - https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
  // Prefer XDG_CONFIG_HOME if available
  auto config_base_dir = std::getenv("XDG_CONFIG_HOME");
  if (!config_base_dir) {
    config_base_path = get_user_home_dir() / ".config";
  } else {
    config_base_path = std::string(config_base_dir);
  }
#endif
  return config_base_path / "OpenGOAL";
}

fs::path get_user_settings_dir() {
  // TODO - jak2
  return get_user_config_dir() / "jak1" / "settings";
}

fs::path get_user_memcard_dir() {
  // TODO - jak2
  return get_user_config_dir() / "jak1" / "saves";
}

struct {
  bool initialized = false;
  fs::path path_to_data;
} gFilePathInfo;

/*!
 * Get the path to the current executable.
 */
std::string get_current_executable_path() {
#ifdef _WIN32
  char buffer[FILENAME_MAX];
  GetModuleFileNameA(NULL, buffer, FILENAME_MAX);
  std::string file_path(buffer);
  if (file_path.rfind("\\\\?\\", 0) == 0) {
    return file_path.substr(4);
  }
  return file_path;
#else
  // do Linux stuff
  char buffer[FILENAME_MAX + 1];
  auto len = readlink("/proc/self/exe", buffer,
                      FILENAME_MAX);  // /proc/self acts like a "virtual folder" containing
  // information about the current process
  buffer[len] = '\0';
  return std::string(buffer);
#endif
}

/*!
 * See if the current executable is somewhere in jak-project/. If so, return the path to jak-project
 */
std::optional<std::string> try_get_jak_project_path() {
  std::string my_path = get_current_executable_path();

  std::string::size_type pos =
      std::string(my_path).rfind("jak-project");  // Strip file path down to /jak-project/ directory
  if (pos == std::string::npos) {
    return {};
  }

  return std::make_optional(std::string(my_path).substr(
      0, pos + 11));  // + 12 to include "/jak-project" in the returned filepath
}

std::optional<fs::path> try_get_data_dir() {
  fs::path my_path = get_current_executable_path();
  auto data_dir = my_path.parent_path() / "data";
  if (fs::exists(data_dir) && fs::is_directory(data_dir)) {
    return std::make_optional(data_dir);
  } else {
    return {};
  }
}

bool setup_project_path(std::optional<fs::path> project_path_override) {
  if (gFilePathInfo.initialized) {
    return true;
  }

  if (project_path_override) {
    gFilePathInfo.path_to_data = *project_path_override;
    gFilePathInfo.initialized = true;
    fmt::print("Using explicitly set project path: {}\n", project_path_override->string());
    return true;
  }

  auto data_path = try_get_data_dir();
  if (data_path) {
    gFilePathInfo.path_to_data = *data_path;
    gFilePathInfo.initialized = true;
    fmt::print("Using data path: {}\n", data_path->string());
    return true;
  }

  auto development_repo_path = try_get_jak_project_path();
  if (development_repo_path) {
    gFilePathInfo.path_to_data = *development_repo_path;
    gFilePathInfo.initialized = true;
    fmt::print("Using development repo path: {}\n", *development_repo_path);
    return true;
  }

  fmt::print("Failed to initialize project path.\n");
  return false;
}

fs::path get_jak_project_dir() {
  ASSERT(gFilePathInfo.initialized);
  return gFilePathInfo.path_to_data;
}

std::string get_file_path(const std::vector<std::string>& input) {
  // TODO - clean this behaviour up, it causes unexpected behaviour when working with files
  // the project path should be explicitly provided by whatever if needed
  // TEMP HACK
  // - if the provided path is absolute, don't add the project path
  if (input.size() == 1 && fs::path(input.at(0)).is_absolute()) {
    return input.at(0);
  }

  auto current_path = file_util::get_jak_project_dir();
  for (auto& str : input) {
    current_path /= str;
  }

  return current_path.string();
}

bool create_dir_if_needed(const fs::path& path) {
  if (!fs::is_directory(path)) {
    fs::create_directories(path);
    return true;
  }
  return false;
}

bool create_dir_if_needed_for_file(const std::string& path) {
  return create_dir_if_needed_for_file(fs::path(path));
}

bool create_dir_if_needed_for_file(const fs::path& path) {
  return fs::create_directories(path.parent_path());
}

void write_binary_file(const fs::path& name, const void* data, size_t size) {
  FILE* fp = file_util::open_file(name.string().c_str(), "wb");
  if (!fp) {
    throw std::runtime_error("couldn't open file " + name.string());
  }

  if (fwrite(data, size, 1, fp) != 1) {
    fclose(fp);
    throw std::runtime_error("couldn't write file " + name.string());
  }

  fclose(fp);
}

void write_binary_file(const std::string& name, const void* data, size_t size) {
  write_binary_file(fs::path(name), data, size);
}

void write_rgba_png(const fs::path& name, void* data, int w, int h) {
  auto flags = 0;

  auto ok = fpng::fpng_encode_image_to_file(name.string().c_str(), data, w, h, 4, flags);

  if (!ok) {
    throw std::runtime_error("couldn't save png file " + name.string());
  }
}

void write_text_file(const std::string& file_name, const std::string& text) {
  write_text_file(fs::path(file_name), text);
}

void write_text_file(const fs::path& file_name, const std::string& text) {
  FILE* fp = file_util::open_file(file_name.string().c_str(), "w");
  if (!fp) {
    lg::error("Failed to fopen {}\n", file_name.string());
    throw std::runtime_error("Failed to open file");
  }
  fprintf(fp, "%s\n", text.c_str());
  fclose(fp);
}
std::vector<uint8_t> read_binary_file(const std::string& filename) {
  return read_binary_file(fs::path(filename));
}

std::vector<uint8_t> read_binary_file(const fs::path& path) {
  // make sure file exists and isn't a directory

  auto status = fs::status(path);

  if (!fs::exists(status)) {
    throw std::runtime_error(
        fmt::format("File {} cannot be opened: does not exist.", path.string()));
  }

  if (status.type() != fs::file_type::regular && status.type() != fs::file_type::symlink) {
    throw std::runtime_error(
        fmt::format("File {} cannot be opened: not a regular file or symlink.", path.string()));
  }

  auto fp = file_util::open_file(path.string().c_str(), "rb");
  if (!fp)
    throw std::runtime_error("File " + path.string() +
                             " cannot be opened: " + std::string(strerror(errno)));
  fseek(fp, 0, SEEK_END);
  auto len = ftell(fp);
  rewind(fp);

  std::vector<uint8_t> data;
  data.resize(len);

  if (fread(data.data(), len, 1, fp) != 1) {
    fclose(fp);
    throw std::runtime_error("File " + path.string() + " cannot be read");
  }
  fclose(fp);

  return data;
}

std::string read_text_file(const fs::path& path) {
  std::ifstream file(path.string());
  if (!file.good()) {
    throw std::runtime_error("couldn't open " + path.string());
  }
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

std::string read_text_file(const std::string& path) {
  return read_text_file(fs::path(path));
}

bool is_printable_char(char c) {
  return c >= ' ' && c <= '~';
}

std::string combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
}

bool file_exists(const std::string& path) {
  return fs::exists(path);
}

std::string base_name(const std::string& filename) {
  size_t pos = 0;
  ASSERT(!filename.empty());
  for (size_t i = filename.size() - 1; i-- > 0;) {
    if (filename.at(i) == '/' || filename.at(i) == '\\') {
      pos = (i + 1);
      break;
    }
  }

  return filename.substr(pos);
}

void ISONameFromAnimationName(char* dst, const char* src) {
  // The Animation Name is a bunch of words separated by dashes

  // copy first two chars of the first word exactly
  dst[0] = src[0];
  dst[1] = src[1];
  s32 i = 2;  // 2 chars added to dst.

  // skip ahead to the first dash (or \0 if there's no dashes)
  const char* src_ptr = src;
  while (*src_ptr && *src_ptr != '-') {
    src_ptr++;
  }

  // the points to the next dash (or \0 if there's none).
  const char* next_ptr = src_ptr;
  if (*src_ptr) {
    // loop over words (next_ptr points to dash before word, i counts chars in dest)
    while (src_ptr = next_ptr + 1, i < 8) {
      // scan next_ptr forward to next dash
      next_ptr = src_ptr;
      while (*next_ptr && *next_ptr != '-') {
        next_ptr++;
      }

      // there's no next word, so break (the current word will be handled there)
      if (!*next_ptr)
        break;

      // add a char for the current word:
      char char_to_add;
      if (next_ptr[-1] < '0' || next_ptr[-1] > '9') {
        // word doesn't end in a number.

        // some special case words map to special letters (likely to avoid animation name conflicts)
        if (next_ptr - src_ptr == 10 && !memcmp(src_ptr, "resolution", 10)) {
          char_to_add = 'z';
        } else if (next_ptr - src_ptr == 6 && !memcmp(src_ptr, "accept", 6)) {
          char_to_add = 'y';
        } else if (next_ptr - src_ptr == 6 && !memcmp(src_ptr, "reject", 6)) {
          char_to_add = 'n';
        } else {
          // not a special case, just take the first letter.
          char_to_add = *src_ptr;
        }
      } else {
        // the current word ends in a number, just use this number (I think usually the whole word
        // is just a number)
        char_to_add = next_ptr[-1];
      }

      dst[i++] = char_to_add;
    }

    // here we ran out of room in dest, or words in source.
    // if there's still room in dest and chars in source, just add them
    while (*src_ptr && (i < 8)) {
      dst[i] = *src_ptr;
      src_ptr++;
      i++;
    }
  }

  // pad with spaces (for ISO Name)
  while (i < 8) {
    dst[i++] = ' ';
  }

  // upper case
  for (i = 0; i < 8; i++) {
    if (dst[i] >= 'a' && dst[i] <= 'z') {
      dst[i] -= 0x20;
    }
  }

  // append file extension
  strcpy(dst + 8, "STR");
}

void MakeISOName(char* dst, const char* src) {
  int i = 0;
  const char* src_ptr = src;
  char* dst_ptr = dst;

  // copy name and upper case
  while ((i < 8) && (*src_ptr) && (*src_ptr != '.')) {
    char c = *src_ptr;
    src_ptr++;
    if (('`' < c) && (c < '{')) {  // lower case
      c -= 0x20;
    }
    *dst_ptr = c;
    dst_ptr++;
    i++;
  }

  // pad out name with spaces
  while (i < 8) {
    *dst_ptr = ' ';
    dst_ptr++;
    i++;
  }

  // increment past period
  if (*src_ptr == '.')
    src_ptr++;

  // same for extension
  while (i < 11 && (*src_ptr)) {
    char c = *src_ptr;
    src_ptr++;
    if (('`' < c) && (c < '{')) {  // lower case
      c -= 0x20;
    }
    *dst_ptr = c;
    dst_ptr++;
    i++;
  }

  while (i < 11) {
    *dst_ptr = ' ';
    dst_ptr++;
    i++;
  }
  *dst_ptr = 0;
}

void assert_file_exists(const char* path, const char* error_message) {
  if (!fs::exists(path)) {
    ASSERT_MSG(false, fmt::format("File {} was not found: {}", path, error_message));
  }
}

/*!
 * Check if the given DGO header (or entire file) is compressed.
 */
bool dgo_header_is_compressed(const std::vector<u8>& data) {
  const char compressed_header[] = "oZlB";
  bool is_compressed = true;
  for (int i = 0; i < 4; i++) {
    if (compressed_header[i] != data.at(i)) {
      is_compressed = false;
    }
  }
  return is_compressed;
}

/*!
 * Decompress a DGO. Resulting data will start at the DGO header.
 */
std::vector<u8> decompress_dgo(const std::vector<u8>& data_in) {
  constexpr int MAX_CHUNK_SIZE = 0x8000;
  BinaryReader compressed_reader(data_in);
  // seek past oZlB
  compressed_reader.ffwd(4);
  std::size_t decompressed_size = compressed_reader.read<uint32_t>();
  std::vector<uint8_t> decompressed_data;
  decompressed_data.resize(decompressed_size);
  size_t output_offset = 0;
  while (true) {
    // seek past alignment bytes and read the next chunk size
    uint32_t chunk_size = 0;
    while (!chunk_size) {
      chunk_size = compressed_reader.read<uint32_t>();
    }

    if (chunk_size < MAX_CHUNK_SIZE) {
      std::size_t bytes_written = 0;
      lzokay::EResult ok = lzokay::decompress(
          compressed_reader.here(), chunk_size, decompressed_data.data() + output_offset,
          decompressed_data.size() - output_offset, bytes_written);
      ASSERT(ok == lzokay::EResult::Success);
      compressed_reader.ffwd(chunk_size);
      output_offset += bytes_written;
    } else {
      // nope - sometimes chunk_size is bigger than MAX, but we should still use max.
      //        ASSERT(chunk_size == MAX_CHUNK_SIZE);
      memcpy(decompressed_data.data() + output_offset, compressed_reader.here(), MAX_CHUNK_SIZE);
      compressed_reader.ffwd(MAX_CHUNK_SIZE);
      output_offset += MAX_CHUNK_SIZE;
    }

    if (output_offset >= decompressed_size)
      break;
    while (compressed_reader.get_seek() % 4) {
      compressed_reader.ffwd(1);
    }
  }

  return decompressed_data;
}

FILE* open_file(const fs::path& path, std::string mode) {
#ifdef _WIN32
  return _wfopen(path.wstring().c_str(), std::wstring(mode.begin(), mode.end()).c_str());
#else
  return fopen(path.string().c_str(), mode.c_str());
#endif
}

std::vector<fs::path> find_files_recursively(const fs::path base_dir, const std::regex& pattern) {
  std::vector<fs::path> files = {};
  for (auto& p : fs::recursive_directory_iterator(base_dir)) {
    if (p.is_regular_file()) {
      if (std::regex_match(fs::path(p.path()).filename().string(), pattern)) {
        files.push_back(p.path());
      }
    }
  }
  return files;
}

}  // namespace file_util
