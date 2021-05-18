#pragma once

#include <string>
#include "common/util/assert.h"
#include "common/common_types.h"
#include "third-party/fmt/core.h"

class ConstantValue {
 public:
  ConstantValue(void* data, int size) : m_size(size) {
    assert(size == 8 || size == 16);
    memcpy(m_value, data, size);
  }

  std::string print() const {
    if (m_size == 8) {
      return "integer-constant-" + std::to_string(value_64());
    } else {
      return fmt::format("integer-constant128-0x{:08x}{:08x}", value_128_hi(), value_128_lo());
    }
  }

  s64 value_64() const {
    assert(m_size == 8);
    s64 result;
    memcpy(&result, m_value, sizeof(s64));
    return result;
  }

  u64 value_128_lo() const {
    assert(m_size == 16);
    s64 result;
    memcpy(&result, m_value, sizeof(s64));
    return result;
  }

  u64 value_128_hi() const {
    assert(m_size == 16);
    s64 result;
    memcpy(&result, m_value + sizeof(s64), sizeof(s64));
    return result;
  }

  /*!
   * 8 or 16 byte constant?
   */
  int size() const { return m_size; }

  /*!
   * Okay to put this in a gpr?
   */
  bool uses_gpr() const { return m_size == 8; }

 protected:
  u8 m_value[16] = {0};
  int m_size;
};