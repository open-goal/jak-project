#pragma once

/*!
 * @file BinaryReader.h
 * Read raw data like a stream.
 */

#include <cstdint>
#include "common/util/assert.h"
#include <vector>

class BinaryReader {
 public:
  explicit BinaryReader(const std::vector<uint8_t>& _buffer) : buffer(_buffer) {}

  template <typename T>
  T read() {
    assert(seek + sizeof(T) <= buffer.size());
    T& obj = *(T*)(buffer.data() + seek);
    seek += sizeof(T);
    return obj;
  }

  void ffwd(int amount) {
    seek += amount;
    assert(seek <= buffer.size());
  }

  uint32_t bytes_left() const { return buffer.size() - seek; }
  uint8_t* here() { return buffer.data() + seek; }
  uint32_t get_seek() const { return seek; }

 private:
  std::vector<u8> buffer;
  uint32_t seek = 0;
};
