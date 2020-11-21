#pragma once

/*!
 * @file BinaryReader.h
 * Read raw data like a stream.
 */

#include <cstdint>
#include <cassert>
#include <vector>

class BinaryReader {
 public:
  BinaryReader(uint8_t* _buffer, uint32_t _size) : buffer(_buffer), size(_size) {}

  explicit BinaryReader(std::vector<uint8_t>& _buffer)
      : buffer((uint8_t*)_buffer.data()), size(_buffer.size()) {}

  template <typename T>
  T read() {
    assert(seek + sizeof(T) <= size);
    T& obj = *(T*)(buffer + seek);
    seek += sizeof(T);
    return obj;
  }

  void ffwd(int amount) {
    seek += amount;
    assert(seek <= size);
  }

  uint32_t bytes_left() const { return size - seek; }

  uint8_t* here() { return buffer + seek; }

  uint32_t get_seek() { return seek; }

 private:
  uint8_t* buffer;
  uint32_t size;
  uint32_t seek = 0;
};
