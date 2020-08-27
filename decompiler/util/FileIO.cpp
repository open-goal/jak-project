#include "FileIO.h"
#include <fstream>
#include <sstream>
#include <cassert>

std::string read_text_file(const std::string& path) {
  std::ifstream file(path);
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
}

std::string combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
}

std::vector<uint8_t> read_binary_file(const std::string& filename) {
  auto fp = fopen(filename.c_str(), "rb");
  if (!fp)
    throw std::runtime_error("File " + filename + " cannot be opened");
  fseek(fp, 0, SEEK_END);
  auto len = ftell(fp);
  rewind(fp);

  std::vector<uint8_t> data;
  data.resize(len);

  if (fread(data.data(), len, 1, fp) != 1) {
    throw std::runtime_error("File " + filename + " cannot be read");
  }

  return data;
}

std::string base_name(const std::string& filename) {
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

void write_text_file(const std::string& file_name, const std::string& text) {
  FILE* fp = fopen(file_name.c_str(), "w");
  if (!fp) {
    printf("Failed to fopen %s\n", file_name.c_str());
    throw std::runtime_error("Failed to open file");
  }
  fprintf(fp, "%s\n", text.c_str());
  fclose(fp);
}