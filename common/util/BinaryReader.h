#pragma once

/*!
 * @file BinaryReader.h
 * Read raw data like a stream.
 */

#include <cstdint>
#include <cstring>
#include "common/util/assert.h"
#include "common/common_types.h"
#include <vector>

class BinaryReader {
 public:
  explicit BinaryReader(const std::vector<uint8_t>& _buffer) : m_buffer(_buffer) {}

  template <typename T>
  T read() {
    assert(m_seek + sizeof(T) <= m_buffer.size());
    T obj;
    memcpy(&obj, m_buffer.data() + m_seek, sizeof(T));
    m_seek += sizeof(T);
    return obj;
  }

  void ffwd(int amount) {
    m_seek += amount;
    assert(m_seek <= m_buffer.size());
  }

  uint32_t bytes_left() const { return m_buffer.size() - m_seek; }
  uint8_t* here() { return m_buffer.data() + m_seek; }
  uint32_t get_seek() const { return m_seek; }
  void set_seek(u32 seek) { m_seek = seek; }

 private:
  std::vector<u8> m_buffer;
  uint32_t m_seek = 0;
};
