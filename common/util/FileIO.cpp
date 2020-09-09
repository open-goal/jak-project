/* Basic File IO Utility */

#include "FileIO.h"
#include <fstream>
#include <sstream>
#include <cassert>
#include <cstring>
#include <cstdio>
#include "game/sce/stubs.h"

void FileCopy(const char* a, const char* b) {
  (void)a;
  (void)b;
}

std::string read_text_file(const std::string& path) {
  std::ifstream file(path);
  std::stringstream ss;
  ss << file.rdbuf();
  return ss.str();
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

void kstrcpy(char* dst, const char* src) {
  char* dst_ptr = dst;
  const char* src_ptr = src;

  while (*src_ptr != 0) {
    *dst_ptr = *src_ptr;
    src_ptr++;
    dst_ptr++;
  }
  *dst_ptr = 0;
}

u32 crc_table[0x100];
static bool sInitCrc = false;
void init_crc2() {
  for (uint32_t i = 0; i < 0x100; i++) {
    uint32_t n = i << 24u;
    for (uint32_t j = 0; j < 8; j++)
      n = n & 0x80000000 ? (n << 1u) ^ 0x04c11db7u : (n << 1u);
    crc_table[i] = n;
  }
  sInitCrc = true;
}

uint32_t crc32a(const uint8_t* data, size_t size) {
  assert(sInitCrc);
  uint32_t crc = 0;
  for (size_t i = size; i != 0; i--, data++) {
    crc = crc_table[crc >> 24u] ^ ((crc << 8u) | *data);
  }
  return ~crc;
}
/*!
 * Does the file exist?  No.  It doesn't.
 * @return 0 always, even if the file exists.
 * DONE, EXACT, UNUSED
 */
u32 FileExists(const char* name) {
  (void)name;
  return 0;
}

/*!
 * Does nothing. Likely is supposed to delete a file.
 * @param name
 * DONE, EXACT, UNUSED
 */
void FileDelete(const char* name) {
  (void)name;
}

/*!
 * Does nothing. Likely is supposed to copy a file.
 * @param a
 * @param b
 * DONE, EXACT, UNUSED
 */
