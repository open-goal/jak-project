#include "FileIO.h"
#include <fstream>
#include <sstream>
#include <cassert>

std::string combine_path(const std::string& parent, const std::string& child) {
  return parent + "/" + child;
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
