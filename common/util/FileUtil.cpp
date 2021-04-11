/*!
 * @file FileUtil.cpp
 * Utility functions for reading and writing files.
 */

#include "FileUtil.h"
#include <iostream>
#include <filesystem>
#include <cstdio> /* defines FILENAME_MAX */
#include <fstream>
#include <sstream>
#include <cassert>
#include <cstdlib>
#include "common/util/BinaryReader.h"
#include "BinaryWriter.h"
#include "common/common_types.h"
#include "third-party/svpng.h"
#include "third-party/lzokay/lzokay.hpp"

#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#include <cstring>
#endif

namespace file_util {
std::filesystem::path get_user_home_dir() {
#ifdef _WIN32
  // NOTE - on older systems, this may case issues if it cannot be found!
  std::string home_dir = std::getenv("USERPROFILE");
  return std::filesystem::path(home_dir);
#else
  std::string home_dir = std::getenv("HOME");
  return std::filesystem::path(home_dir);
#endif
}

std::string get_project_path() {
#ifdef _WIN32
  char buffer[FILENAME_MAX];
  GetModuleFileNameA(NULL, buffer, FILENAME_MAX);
  std::string::size_type pos =
      std::string(buffer).rfind("jak-project");  // Strip file path down to \jak-project\ directory
  return std::string(buffer).substr(
      0, pos + 11);  // + 12 to include "\jak-project" in the returned filepath
#else
  // do Linux stuff
  char buffer[FILENAME_MAX + 1];
  auto len = readlink("/proc/self/exe", buffer,
                      FILENAME_MAX);  // /proc/self acts like a "virtual folder" containing
  // information about the current process
  buffer[len] = '\0';
  std::string::size_type pos =
      std::string(buffer).rfind("jak-project");  // Strip file path down to /jak-project/ directory
  return std::string(buffer).substr(
      0, pos + 11);  // + 12 to include "/jak-project" in the returned filepath
#endif
}

std::string get_file_path(const std::vector<std::string>& input) {
  std::string currentPath = file_util::get_project_path();
  char dirSeparator;

#ifdef _WIN32
  dirSeparator = '\\';
#else
  dirSeparator = '/';
#endif

  std::string filePath = currentPath;
  for (int i = 0; i < int(input.size()); i++) {
    filePath = filePath + dirSeparator + input[i];
  }

  return filePath;
}

bool create_dir_if_needed(const std::string& path) {
  if (!std::filesystem::is_directory(path)) {
    std::filesystem::create_directories(path);
    return true;
  }
  return false;
}

void write_binary_file(const std::string& name, const void* data, size_t size) {
  FILE* fp = fopen(name.c_str(), "wb");
  if (!fp) {
    throw std::runtime_error("couldn't open file " + name);
  }

  if (fwrite(data, size, 1, fp) != 1) {
    throw std::runtime_error("couldn't write file " + name);
  }

  fclose(fp);
}

void write_rgba_png(const std::string& name, void* data, int w, int h) {
  FILE* fp = fopen(name.c_str(), "wb");
  if (!fp) {
    throw std::runtime_error("couldn't open file " + name);
  }

  svpng(fp, w, h, (const unsigned char*)data, 1);

  fclose(fp);
}

void write_text_file(const std::string& file_name, const std::string& text) {
  FILE* fp = fopen(file_name.c_str(), "w");
  if (!fp) {
    printf("Failed to fopen %s\n", file_name.c_str());
    throw std::runtime_error("Failed to open file");
  }
  fprintf(fp, "%s\n", text.c_str());
  fclose(fp);
}

std::vector<uint8_t> read_binary_file(const std::string& filename) {
  auto fp = fopen(filename.c_str(), "rb");
  if (!fp)
    throw std::runtime_error("File " + filename +
                             " cannot be opened: " + std::string(strerror(errno)));
  fseek(fp, 0, SEEK_END);
  auto len = ftell(fp);
  rewind(fp);

  std::vector<uint8_t> data;
  data.resize(len);

  if (fread(data.data(), len, 1, fp) != 1) {
    throw std::runtime_error("File " + filename + " cannot be read");
  }
  fclose(fp);

  return data;
}

std::string read_text_file(const std::string& path) {
  std::ifstream file(path);
  if (!file.good()) {
    throw std::runtime_error("couldn't open " + path);
  }
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

bool is_printable_char(char c) {
  return c >= ' ' && c <= '~';
}

std::string combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
}

std::string base_name(const std::string& filename) {
  size_t pos = 0;
  assert(!filename.empty());
  for (size_t i = filename.size() - 1; i-- > 0;) {
    if (filename.at(i) == '/' || filename.at(i) == '\\') {
      pos = (i + 1);
      break;
    }
  }

  return filename.substr(pos);
}

static bool sInitCrc = false;
static uint32_t crc_table[0x100];

void init_crc() {
  for (uint32_t i = 0; i < 0x100; i++) {
    uint32_t n = i << 24u;
    for (uint32_t j = 0; j < 8; j++)
      n = n & 0x80000000 ? (n << 1u) ^ 0x04c11db7u : (n << 1u);
    crc_table[i] = n;
  }
  sInitCrc = true;
}

uint32_t crc32(const uint8_t* data, size_t size) {
  assert(sInitCrc);
  uint32_t crc = 0;
  for (size_t i = size; i != 0; i--, data++) {
    crc = crc_table[crc >> 24u] ^ ((crc << 8u) | *data);
  }
  return ~crc;
}

uint32_t crc32(const std::vector<uint8_t>& data) {
  return crc32(data.data(), data.size());
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
    if (dst[i] > '`' && dst[i] < '{') {
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
  if (!std::filesystem::exists(path)) {
    fprintf(stderr, "File %s was not found: %s\n", path, error_message);
    assert(false);
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
      assert(ok == lzokay::EResult::Success);
      compressed_reader.ffwd(chunk_size);
      output_offset += bytes_written;
    } else {
      // nope - sometimes chunk_size is bigger than MAX, but we should still use max.
      //        assert(chunk_size == MAX_CHUNK_SIZE);
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

}  // namespace file_util
