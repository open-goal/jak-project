#include "FileUtil.h"
#include <iostream>
#include <stdio.h> /* defines FILENAME_MAX */
#include <fstream>
#include <sstream>
#include <cassert>

#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#include <cstring>
#endif

std::string file_util::get_project_path() {
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

std::string file_util::get_file_path(const std::vector<std::string>& input) {
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

void file_util::write_binary_file(const std::string& name, void* data, size_t size) {
  FILE* fp = fopen(name.c_str(), "wb");
  if (!fp) {
    throw std::runtime_error("couldn't open file " + name);
  }

  if (fwrite(data, size, 1, fp) != 1) {
    throw std::runtime_error("couldn't write file " + name);
  }

  fclose(fp);
}

void file_util::write_text_file(const std::string& file_name, const std::string& text) {
  FILE* fp = fopen(file_name.c_str(), "w");
  if (!fp) {
    printf("Failed to fopen %s\n", file_name.c_str());
    throw std::runtime_error("Failed to open file");
  }
  fprintf(fp, "%s\n", text.c_str());
  fclose(fp);
}

std::vector<uint8_t> file_util::read_binary_file(const std::string& filename) {
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

std::string file_util::read_text_file(const std::string& path) {
  std::ifstream file(path);
  if (!file.good()) {
    throw std::runtime_error("couldn't open " + path);
  }
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

bool file_util::is_printable_char(char c) {
  return c >= ' ' && c <= '~';
}

std::string file_util::combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
}

std::string file_util::base_name(const std::string& filename) {
  size_t pos = 0;
  assert(!filename.empty());
  for (size_t i = filename.size() - 1; i-- > 0;) {
    if (filename.at(i) == '/') {
      pos = (i + 1);
      break;
    }
  }

  return filename.substr(pos);
}

static bool sInitCrc = false;
static uint32_t crc_table[0x100];

void file_util::init_crc() {
  for (uint32_t i = 0; i < 0x100; i++) {
    uint32_t n = i << 24u;
    for (uint32_t j = 0; j < 8; j++)
      n = n & 0x80000000 ? (n << 1u) ^ 0x04c11db7u : (n << 1u);
    crc_table[i] = n;
  }
  sInitCrc = true;
}

uint32_t file_util::crc32(const uint8_t* data, size_t size) {
  assert(sInitCrc);
  uint32_t crc = 0;
  for (size_t i = size; i != 0; i--, data++) {
    crc = crc_table[crc >> 24u] ^ ((crc << 8u) | *data);
  }
  return ~crc;
}

uint32_t file_util::crc32(const std::vector<uint8_t>& data) {
  return crc32(data.data(), data.size());
}
